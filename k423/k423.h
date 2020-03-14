#ifndef KIANA_K423_H
#define KIANA_K423_H

#include <stdint.h>

#define in
#define out
#define inout in out

typedef struct {
  int32_t status_code;
  const char *desc;
} k_status_t;

typedef void* k_rsi_t;
typedef void* k_iter_t;
typedef void* k_tuple_t;

k_status_t k_init_rsi(inout k_rsi_t *rsi);

k_status_t k_create_table(inout k_rsi_t *rsi, 
                          in const char *table_name, 
                          in const char **columns,
                          in size_t *indexes,
                          out int32_t *table_id);

k_status_t k_table_id(in k_rsi_t *rsi,
                      in const char *table_name,
                      out int32_t *table_id);

k_status_t k_insert(inout k_rsi_t *rsi,
                    in int32_t table_id,
                    in k_tuple_t tuple);

k_status_t k_table_iter(in k_rsi_t *rsi,
                        in int32_t table_id,
                        out k_iter_t *table_iter);

char *k_tuple_fetch(in k_tuple_t tuple, 

k_status_t k_iter_get(in k_iter_t iter,
                      in char *buffer,
                      in size_t bufsiz,
                      out k_tuple_t *tuple);

k_status_t k_iter_next(in k_iter_t iter,
                       out k_iter_t *next_iter);
                       
k_status_t k_iter_test(in k_iter_t iter,
                       out bool *at_end);

k_status_t k_iter_rewind(in k_iter_t iter,
                         out k_iter_t *rewind_iter);

k_status_t k_iter_index(in k_iter_t iter,
                        in size_t column_id,
                        in const char *key,
                        in char *buffer,
                        in size_t bufsiz,
                        out char *value);

#endif // KIANA_K423_H
