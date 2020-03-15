#include "k423int.h"

#include <stdlib.h>

#define K_NEW(T) aligned_alloc(4, sizeof(T))
#define K_NEWN(T, N) aligned_alloc(4, (sizeof(T) * (N)))

#define K_VEC_PUSH(T, vec, size, cap, value) \
  { \
    if ((size) >= (cap)) { \
      if ((cap) == 0) { \
        (vec) = K_NEWN(T, 4); \
        (cap) = 4; \
      } else { \
        T* new_vec = K_NEWN(T, (cap) * 2); \
        free((vec)); \
        (vec) = new_vec; \
        (cap) *= 2; \ 
      } \
    } \
    (vec)[(size)] = (value); \
    (size) += 1; \
  }

#define K_VEC_DESTROY(T, vec, size, cap, dproc) \
  { \
    for (size_t i = 0; i < (cap); i++) { \
      (dproc)((vec)[i]); \
    } \
    free(vec); \
  }

typedef struct {
  K_ITER_COMMON
  
} k_mem_iter_t;

typedef struct {
  
} k_mem_table_t;

typedef struct {
  k_mem_table_t *tables;
  size_t size;
  size_t cap;
} k_mem_rsi_t;

k_status_t k_init_rsi(inout k_rsi_t *rsi) {
  k_mem_rsi_t *mem_rsi = K_NEW(k_mem_rsi_t);
  *rsi = (void*)mem_rsi;
}

k_status_t k_create_table(inout k_rsi_t rsi,
                          in const char *table_name,
                          in const char **columns,
                          in size_t *indexes,
                          out int32_t *table_id) {
  k_mem_rsi_t *mem_rsi = (k_mem_rsi_t*)rsi;
}
