#ifndef KIANA_K423_INT_H
#define KIANA_K423_INT_H

#include "k423.h"

#include "string.h"

typedef k_status_t (*k_iter_get_fn)(k_iter_t, char*, size_t, k_tuple_t);
typedef k_status_t (*k_iter_next_fn)(k_iter_t, k_iter_t*);
typedef k_status_t (*k_iter_test_fn)(k_iter_t, bool*);
typedef k_status_t (*k_iter_rewind_fn)(k_iter_t, k_iter_t*);
typedef k_status_t (*k_iter_index_fn)(k_iter_t, size_t, const char*,
                                      char*, size_t, k_tuple_t);

typedef struct {
  k_iter_get_fn k_get;
  k_iter_next_fn k_next;
  k_iter_test_fn k_test;
  k_iter_rewind_fn k_rewind;
  k_iter_index_fn k_index;
} k_iter_vtable_t;

typedef struct {
  size_t bufsiz;
  size_t usage;
  
  char **columns_ptr;
  char *data_ptr;
  size_t tail;
} k_tuple_int_t;

#define K_ITER_COMMON k_iter_vtable_t *vptr;

typedef struct {
  k_iter_vtable_t *vptr;
} k_iter_int_t;

k_status_t k_iter_get(in k_iter_t iter,
                      in char *buffer,
                      in size_t bufsiz,
                      out k_tuple_t tuple) {
  k_iter_int_t *iter_int = (k_iter_int_t*)iter;
  return iter_int->vptr->k_get(iter, buffer, bufsiz, tuple);
}

k_status_t k_iter_next(in k_iter_t iter,
                       out k_iter_t *next_iter) {
  k_iter_int_t *iter_int = (k_iter_int_t*)iter;
  return iter_int->vptr->k_next(iter, next_iter);
}
                       
k_status_t k_iter_test(in k_iter_t iter,
                       out bool *at_end) {
  k_iter_int_t *iter_int = (k_iter_int_t*)iter;
  return iter_int->vptr->k_test(iter, at_end);
}

k_status_t k_iter_rewind(in k_iter_t iter,
                         out k_iter_t *rewind_iter) {
  k_iter_int_t *iter_int = (k_iter_int_t*)iter;
  return iter_int->vptr->k_rewind(iter, rewind_iter);
}

k_status_t k_iter_index(in k_iter_t iter,
                        in size_t column_id,
                        in const char *key,
                        in char *buffer,
                        in size_t bufsiz,
                        out k_tuple_t value) {
  k_iter_int_t *iter_int = (k_iter_int_t*)iter;
  return iter_int->vptr->k_index(iter, column_id, key,
                                 buffer, bufsiz, value);
}

void k_init_tuple(inout k_tuple_t tuple,
                  in char *buffer,
                  in size_t bufsiz,
                  in size_t column_count) {
  k_tuple_int_t *tuple_int = (k_tuple_int_t*)tuple;
  char *data_start = buffer + column_count * sizeof(size_t);
  tuple_int->bufsiz = bufsiz;
  tuple_int->usage = (column_count * sizeof(size_t));
  tuple_int->columns_ptr = (char**)buffer;
  tuple_int->data_ptr = data_start;
  tuple_int->tail = 0;
}

bool k_tuple_put(inout k_tuple_t tuple, 
                 in size_t column,
                 in char *value) {
  k_tuple_int_t *tuple_int = (k_tuple_int_t*)tuple;
  size_t value_len = strlen(value);
  
  char *data_piece_start = tuple_int->data_ptr + tuple_int->tail;
  tuple_int->columns_ptr[column] = data_piece_start;
  strcpy(tuple_int->columns_ptr[column], value);
  tuple_int->tail += value_len + 1;
  
  return true;
}

char *k_tuple_fetch(in k_tuple_t tuple, in size_t column) {
  k_tuple_int_t *tuple_int = (k_tuple_int_t*)tuple;
  return tuple_int->columns_ptr[column];
}

#endif // KIANA_K423_INT_H
