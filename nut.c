#include "runtime.h"
#include <assert.h>
#include <string.h>

object_t *nut_eval_arg_list(object_t *args) {
  // TODO
  return args;
}

object_t *apply_plus(object_t *args) {
  int retval = 0;
  for (object_t *ptr = args; is_cons(ptr); ptr = cdr(ptr)) {
    assert(is_number(car(ptr)));
    retval += number_int(car(ptr));
  }
  return make_number(retval);
}

object_t *nut_eval(object_t *expr) {
  if (is_cons(expr)) {
    object_t *op = car(expr);
    object_t *args = nut_eval_arg_list(cdr(expr));
    if (is_symbol(op)) {
      if (strcmp(symbol_string(op), "+") == 0) {
        return apply_plus(args);
      }
    }
  }

  assert(0);
}
