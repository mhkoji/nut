#include "runtime.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum object_type_t { OT_CONS = 1, OT_SYMBOL, OT_NUMBER };
typedef enum object_type_t object_type_t;

struct cons_t {
  object_t *car;
  object_t *cdr;
};
typedef struct cons_t cons_t;

struct number_t {
  int val;
};
typedef struct number_t number_t;

struct symbol_t {
  char *string;
};
typedef struct symbol_t symbol_t;

struct object_t {
  object_type_t type;
  union {
    cons_t cons;
    number_t num;
    symbol_t sym;
  } u;
};

object_t *alloc(object_type_t type) {
  object_t *ret = malloc(sizeof(object_t));
  ret->type = type;
  return ret;
}

bool is_cons(object_t *obj) { return obj->type == OT_CONS; }

object_t *car(object_t *obj) {
  assert(is_cons(obj));
  return obj->u.cons.car;
}

object_t *cdr(object_t *obj) {
  assert(is_cons(obj));
  return obj->u.cons.cdr;
}

bool is_number(object_t *obj) { return obj->type == OT_NUMBER; }

object_t *make_number(int val) {
  object_t *ret = alloc(OT_NUMBER);
  ret->u.num.val = val;
  return ret;
}

int number_int(object_t *obj) {
  assert(is_number(obj));
  return obj->u.num.val;
}

bool is_symbol(object_t *obj) { return obj->type == OT_SYMBOL; }

char *symbol_string(object_t *obj) {
  assert(is_symbol(obj));
  return obj->u.sym.string;
}

object_t *read_from_string(char *string) {
  assert(strcmp(string, "(+ 1 2)") == 0);

  object_t *ret = alloc(OT_CONS);
  ret->u.cons.car = alloc(OT_SYMBOL);
  ret->u.cons.car->u.sym.string = malloc(2);
  strcpy(ret->u.cons.car->u.sym.string, "+");

  ret->u.cons.cdr = alloc(OT_CONS);
  ret->u.cons.cdr->u.cons.car = make_number(1);

  ret->u.cons.cdr->u.cons.cdr = alloc(OT_CONS);
  ret->u.cons.cdr->u.cons.cdr->u.cons.car = make_number(2);

  ret->u.cons.cdr->u.cons.cdr->u.cons.cdr = alloc(OT_SYMBOL);
  ret->u.cons.cdr->u.cons.cdr->u.cons.cdr->u.sym.string = malloc(4);
  strcpy(ret->u.cons.cdr->u.cons.cdr->u.cons.cdr->u.sym.string, "NIL");

  return ret;
}

void print_object(object_t *obj) {
  if (is_number(obj)) {
    printf("[num: %d]\n", obj->u.num.val);
  } else {
    printf("[unknown type: %d]\n", obj->type);
  }
}
