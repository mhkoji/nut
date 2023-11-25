#include "runtime.h"

object_t *nut_eval(object_t *obj);

int main(void) {
  print_object(nut_eval(read_from_string("(+ 1 2)")));
  return 0;
}
