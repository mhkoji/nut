#ifndef __RUNTIME_H__
#define __RUNTIME_H__

#include <stdbool.h>

struct object_t;
typedef struct object_t object_t;

bool is_cons(object_t *obj);
object_t *car(object_t *obj);
object_t *cdr(object_t *obj);

bool is_number(object_t *obj);
object_t *make_number(int val);
int number_int(object_t *obj);

bool is_symbol(object_t *obj);
char *symbol_string(object_t *obj);

object_t *read_from_string(char *string);
void print_object(object_t *obj);

#endif
