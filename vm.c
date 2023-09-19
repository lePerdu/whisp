#include "vm.h"

#include <assert.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "types.h"
#include "val_array.h"

#define STACK_INIT_CAP 8

/**
 * "Activation record" for a function.
 * TODO Change name?
 */
struct active_record {
  /** Index into the stack */
  unsigned frame_pointer;
  struct lisp_env *env;
};

static void active_record_visit(struct active_record *rec, visit_callback cb,
                                void *ctx) {
  cb(ctx, lisp_val_from_obj(rec->env));
}

struct call_stack {
  size_t size;
  size_t cap;
  struct active_record *data;
};

#define CALL_STACK_EMPTY \
  ((struct call_stack){.size = 0, .cap = 0, .data = NULL})

static void call_stack_push(struct call_stack *s, struct active_record v) {
  if (s->size >= s->cap) {
    s->cap = s->cap < STACK_INIT_CAP ? STACK_INIT_CAP : 2 * s->cap;
    s->data = realloc(s->data, s->cap * sizeof(s->data[0]));
  }

  s->data[s->size++] = v;
}

static struct active_record *call_stack_top(const struct call_stack *s) {
  assert(s->size > 0);
  return &s->data[s->size - 1];
}

static void call_stack_pop(struct call_stack *s) {
  assert(s->size > 0);
  --s->size;
}

static void call_stack_destroy(struct call_stack *s) { free(s->data); }

struct lisp_vm {
  struct lisp_obj header;

  // TODO Store global env as a base call frame?
  struct lisp_env *global_env;
  struct call_stack call_frames;
  struct val_array stack;

  bool has_exception;
  struct lisp_val current_exception;
};

static void vm_visit_children(struct lisp_val v, visit_callback cb, void *ctx) {
  struct lisp_vm *vm = lisp_val_as_obj(v);

  cb(ctx, lisp_val_from_obj(vm->global_env));
  for (unsigned i = 0; i < vm->call_frames.size; i++) {
    active_record_visit(&vm->call_frames.data[i], cb, ctx);
  }
  for (unsigned i = 0; i < vm->stack.size; i++) {
    cb(ctx, vm->stack.data[i]);
  }

  if (vm->has_exception) {
    cb(ctx, vm->current_exception);
  }
}

static void vm_destroy(struct lisp_val v) {
  struct lisp_vm *vm = lisp_val_as_obj(v);
  call_stack_destroy(&vm->call_frames);
  val_array_destroy(&vm->stack);
}

static const struct lisp_vtable VM_VTABLE = {
    .type = LISP_INVALID,
    .name = "vm",
    .is_gc_managed = true,
    .visit_children = vm_visit_children,
    .destroy = vm_destroy,
};

struct lisp_vm *vm_create(void) {
  // TODO Accept environment as parameter?
  struct lisp_env *global_env = lisp_env_create(NULL);
  gc_push_root_obj(global_env);
  struct lisp_vm *vm = lisp_obj_alloc(&VM_VTABLE, sizeof(*vm));
  vm->global_env = global_env;
  vm->call_frames = CALL_STACK_EMPTY;
  vm->stack = VAL_ARRAY_EMPTY;
  vm->has_exception = false;
  vm->current_exception = LISP_VAL_NIL;
  gc_pop_root_expect_obj(global_env);
  return vm;
}

bool vm_has_exception(const struct lisp_vm *vm) { return vm->has_exception; }

struct lisp_val vm_current_exception(const struct lisp_vm *vm) {
  return vm->current_exception;
}

void vm_raise_exception(struct lisp_vm *vm, struct lisp_val exception) {
  vm->has_exception = true;
  vm->current_exception = exception;
}

void vm_raise_format_exception(struct lisp_vm *vm, const char *format, ...) {
  va_list ap;
  va_start(ap, format);
  vm_raise_exception(vm, lisp_val_from_obj(lisp_string_vformat(format, ap)));
  va_end(ap);
}

void vm_clear_exception(struct lisp_vm *vm) {
  vm->has_exception = false;
  vm->current_exception = LISP_VAL_NIL;
}

struct lisp_env *vm_current_env(struct lisp_vm *vm) {
  if (vm->call_frames.size == 0) {
    return vm->global_env;
  } else {
    return call_stack_top(&vm->call_frames)->env;
  }
}

static inline unsigned active_frame_pointer(const struct lisp_vm *vm) {
  if (vm->call_frames.size == 0) {
    return 0;
  } else {
    return call_stack_top(&vm->call_frames)->frame_pointer;
  }
}

static inline unsigned active_frame_size(const struct lisp_vm *vm) {
  if (vm->call_frames.size == 0) {
    return vm->stack.size;
  } else {
    return vm->stack.size - call_stack_top(&vm->call_frames)->frame_pointer;
  }
}

unsigned vm_stack_size(const struct lisp_vm *vm) {
  return vm->stack.size - active_frame_pointer(vm);
}

void vm_stack_push(struct lisp_vm *vm, struct lisp_val v) {
  val_array_push(&vm->stack, v);
}

struct lisp_val vm_stack_top(struct lisp_vm *vm) {
  // Don't let external code see past the current frame
  assert(vm->stack.size > active_frame_pointer(vm));
  return val_array_top(&vm->stack);
}

struct lisp_val vm_stack_pop(struct lisp_vm *vm) {
  // Don't let external code pop past the current frame
  assert(vm->stack.size > active_frame_pointer(vm));
  return val_array_pop(&vm->stack);
}

void vm_stack_frame_clear(struct lisp_vm *vm) {
  vm->stack.size = active_frame_pointer(vm);
}

void vm_create_stack_frame(struct lisp_vm *vm, struct lisp_env *func_env,
                           unsigned arg_count) {
  assert(arg_count <= active_frame_size(vm));
  unsigned new_fp = vm->stack.size - arg_count;

  call_stack_push(&vm->call_frames, (struct active_record){
                                        .frame_pointer = new_fp,
                                        .env = func_env,
                                    });
}

void vm_create_tail_stack_frame(struct lisp_vm *vm, struct lisp_env *func_env,
                                unsigned arg_count) {
  unsigned frame_size = active_frame_size(vm);
  assert(arg_count <= frame_size);
  // Clear the stack except for the arguments
  val_array_skip_delete(&vm->stack, arg_count, frame_size - arg_count);
  assert(active_frame_size(vm) == arg_count);

  // Don't need to replace the frame pointer as it is unchanged
  call_stack_top(&vm->call_frames)->env = func_env;
}

void vm_stack_frame_return(struct lisp_vm *vm) {
  // Return value is the top of the frame stack
  struct lisp_val return_val = vm_stack_pop(vm);
  unsigned old_fp = active_frame_pointer(vm);
  // Pop the frame off the stack
  call_stack_pop(&vm->call_frames);
  vm->stack.size = old_fp;
  val_array_push(&vm->stack, return_val);
}

void vm_stack_frame_save(const struct lisp_vm *vm,
                         struct stack_frame_state *state) {
  state->frame_index = vm->call_frames.size - 1;
  state->stack_size = vm->stack.size;
}

void vm_stack_frame_unwind_to(struct lisp_vm *vm,
                              const struct stack_frame_state *state) {
  vm->call_frames.size = state->frame_index + 1;
  vm->stack.size = state->stack_size;
}

struct lisp_val vm_from_frame_pointer(const struct lisp_vm *vm,
                                      unsigned index) {
  // 0 -> element at FP
  unsigned stack_index = active_frame_pointer(vm) + index;
  assert(stack_index < vm->stack.size);
  return vm->stack.data[stack_index];
}

struct lisp_val vm_from_stack_pointer(const struct lisp_vm *vm,
                                      unsigned rev_index) {
  // 0 -> top of the stack
  int stack_index = vm->stack.size - rev_index - 1;
  assert(stack_index >= (int)active_frame_pointer(vm));
  return vm->stack.data[stack_index];
}
