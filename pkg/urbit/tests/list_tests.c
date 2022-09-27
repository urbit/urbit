/// @file list_tests.c

#include "c/list.h"

#include "c/defs.h"
#include "c/portable.h"
#include "c/types.h"

#define _arrlen(arr) (sizeof(arr) / sizeof(arr[0]))

static void
_test_init(void)
{
  {
    c3_list* lis_u = c3_list_init(C3_LIST_COPY);
    c3_assert(lis_u);
    c3_free(lis_u);
  }

  {
    c3_list* lis_u = c3_list_init(C3_LIST_TRANSFER);
    c3_assert(lis_u);
    c3_free(lis_u);
  }

  {
    c3_list* lis_u = c3_list_init(C3_LIST_OWNERSHIP_END);
    c3_assert(!lis_u);
  }
}

static void
_test_length(void)
{
  // Get length of NULL.
  {
    c3_list* lis_u = NULL;
    c3_assert(0 == c3_list_len(lis_u));
  }

  // Get length of empty list.
  {
    c3_list* lis_u = c3_list_init(C3_LIST_COPY);
    c3_assert(0 == c3_list_len(lis_u));
    c3_free(lis_u);
  }

  // Get length of empty list.
  {
    c3_list* lis_u = c3_list_init(C3_LIST_TRANSFER);
    c3_assert(0 == c3_list_len(lis_u));
    c3_free(lis_u);
  }
}

static void
_test_push_back(void)
{
  // Push a bunch of numbers (COPY).
  {
    c3_list* lis_u;
    c3_lode* nod_u;
    c3_assert(lis_u = c3_list_init(C3_LIST_COPY));
    c3_assert(!c3_list_peekb(lis_u));
    size_t len_i = 10;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      c3_list_pushb(lis_u, &idx_i, sizeof(idx_i));
      c3_assert(nod_u = c3_list_peekb(lis_u));
      c3_assert(idx_i == *(size_t*)c3_lode_data(nod_u));
    }
    c3_assert(len_i == c3_list_len(lis_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(lis_u);
  }

  // Push a bunch of numbers (TRANSFER).
  {
    c3_list* lis_u;
    c3_lode* nod_u;
    c3_assert(lis_u = c3_list_init(C3_LIST_TRANSFER));
    c3_assert(!c3_list_peekb(lis_u));
    size_t len_i = 10;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      c3_list_pushb(lis_u, &idx_i, sizeof(idx_i));
      c3_assert(nod_u = c3_list_peekb(lis_u));
      c3_assert(idx_i == **(size_t**)c3_lode_data(nod_u));
    }
    c3_assert(len_i == c3_list_len(lis_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(lis_u);
  }

  // Push a bunch of heap-allocated arrays (TRANSFER).
  {
    c3_list* lis_u;
    c3_lode* nod_u;
    c3_assert(lis_u = c3_list_init(C3_LIST_TRANSFER));
    c3_assert(!c3_list_peekb(lis_u));
    size_t len_i    = 10;
    c3_y*  arrs_y[] = {
       c3_malloc(32),
       c3_malloc(32),
       c3_malloc(32),
       c3_malloc(32),
       c3_malloc(32),
       c3_malloc(32),
       c3_malloc(32),
       c3_malloc(32),
       c3_malloc(32),
       c3_malloc(32),
    };
    static const c3_c str_c[] = "hello";
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      memcpy(arrs_y[idx_i], str_c, sizeof(str_c));
      c3_assert(idx_i == c3_list_len(lis_u));
      c3_list_pushb(lis_u, arrs_y[idx_i], sizeof(str_c));
      c3_assert(nod_u = c3_list_peekb(lis_u));
      c3_assert(sizeof(str_c) == c3_lode_len(nod_u));
      c3_assert(0 == strcmp(str_c, *(c3_c**)c3_lode_data(nod_u)));
    }
    c3_assert(len_i == c3_list_len(lis_u));

    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(lis_u);
  }

  // Push a bunch of strings (COPY).
  {
    c3_list*     lis_u;
    c3_lode*     nod_u;
    static char* strs_c[] = {
      "antonio",
      "bingbing",
      "catherine",
      "deandre",
      "emir",
    };
    c3_assert(lis_u = c3_list_init(C3_LIST_COPY));
    c3_assert(!c3_list_peekb(lis_u));
    size_t len_i = _arrlen(strs_c);
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      char* str_c = strs_c[idx_i];
      c3_list_pushb(lis_u, str_c, 1 + strlen(str_c));
      c3_assert(nod_u = c3_list_peekb(lis_u));
      c3_assert(0 == strcmp(str_c, c3_lode_data(nod_u)));
    }
    c3_assert(len_i == c3_list_len(lis_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(lis_u);
  }

  // Push a bunch of strings (TRANSFER).
  {
    c3_list*     lis_u;
    c3_lode*     nod_u;
    static char* strs_c[] = {
      "antonio",
      "bingbing",
      "catherine",
      "deandre",
      "emir",
    };
    c3_assert(lis_u = c3_list_init(C3_LIST_TRANSFER));
    c3_assert(!c3_list_peekb(lis_u));
    size_t len_i = _arrlen(strs_c);
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      char* str_c = strs_c[idx_i];
      c3_list_pushb(lis_u, str_c, 1 + strlen(str_c));
      c3_assert(nod_u = c3_list_peekb(lis_u));
      c3_assert(0 == strcmp(str_c, *(c3_c**)c3_lode_data(nod_u)));
    }
    c3_assert(len_i == c3_list_len(lis_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(lis_u);
  }
}

static void
_test_push_front(void)
{
  // Push a bunch of numbers (COPY).
  {
    c3_list* lis_u;
    c3_lode* nod_u;
    c3_assert(lis_u = c3_list_init(C3_LIST_COPY));
    c3_assert(!c3_list_peekf(lis_u));
    size_t len_i = 10;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      c3_list_pushf(lis_u, &idx_i, sizeof(idx_i));
      c3_assert(nod_u = c3_list_peekf(lis_u));
      c3_assert(idx_i == *(size_t*)c3_lode_data(nod_u));
    }
    c3_assert(len_i == c3_list_len(lis_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(lis_u);
  }

  // Push a bunch of numbers (TRANSFER).
  {
    c3_list* lis_u;
    c3_lode* nod_u;
    c3_assert(lis_u = c3_list_init(C3_LIST_TRANSFER));
    c3_assert(!c3_list_peekf(lis_u));
    size_t len_i = 10;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      c3_list_pushf(lis_u, &idx_i, sizeof(idx_i));
      c3_assert(nod_u = c3_list_peekf(lis_u));
      c3_assert(idx_i == **(size_t**)c3_lode_data(nod_u));
    }
    c3_assert(len_i == c3_list_len(lis_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(lis_u);
  }

  // Push a bunch of strings (COPY).
  {
    c3_list*     lis_u;
    c3_lode*     nod_u;
    static char* strs_c[] = {
      "antonio",
      "bingbing",
      "catherine",
      "deandre",
      "emir",
    };
    c3_assert(lis_u = c3_list_init(C3_LIST_COPY));
    c3_assert(!c3_list_peekf(lis_u));
    size_t len_i = _arrlen(strs_c);
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      char* str_c = strs_c[idx_i];
      c3_list_pushf(lis_u, str_c, 1 + strlen(str_c));
      c3_assert(nod_u = c3_list_peekf(lis_u));
      c3_assert(0 == strcmp(str_c, c3_lode_data(nod_u)));
    }
    c3_assert(len_i == c3_list_len(lis_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(lis_u);
  }

  // Push a bunch of strings (TRANSFER).
  {
    c3_list*     lis_u;
    c3_lode*     nod_u;
    static char* strs_c[] = {
      "antonio",
      "bingbing",
      "catherine",
      "deandre",
      "emir",
    };
    c3_assert(lis_u = c3_list_init(C3_LIST_TRANSFER));
    c3_assert(!c3_list_peekf(lis_u));
    size_t len_i = _arrlen(strs_c);
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      char* str_c = strs_c[idx_i];
      c3_list_pushf(lis_u, str_c, 1 + strlen(str_c));
      c3_assert(nod_u = c3_list_peekf(lis_u));
      c3_assert(0 == strcmp(str_c, *(c3_c**)c3_lode_data(nod_u)));
    }
    c3_assert(len_i == c3_list_len(lis_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(lis_u);
  }
}

static void
_test_push_mixed(void)
{
  // Push even numbers onto the front and odd numbers onto the back (COPY).
  {
    c3_list* lis_u;
    c3_lode* nod_u;
    c3_assert(lis_u = c3_list_init(C3_LIST_COPY));
    size_t len_i = 100;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      c3_list_end end_i = idx_i % 2;
      c3_list_push(lis_u, end_i, &idx_i, sizeof(idx_i));
      c3_assert(nod_u = c3_list_peek(lis_u, end_i));
      c3_assert(idx_i == *(size_t*)c3_lode_data(nod_u));
    }
    c3_assert(len_i == c3_list_len(lis_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(lis_u);
  }

  // Push even numbers onto the front and odd numbers onto the back (TRANSFER).
  {
    c3_list* lis_u;
    c3_lode* nod_u;
    c3_assert(lis_u = c3_list_init(C3_LIST_TRANSFER));
    size_t len_i = 100;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      c3_list_end end_i = idx_i % 2;
      c3_list_push(lis_u, end_i, &idx_i, sizeof(idx_i));
      c3_assert(nod_u = c3_list_peek(lis_u, end_i));
      c3_assert(idx_i == **(size_t**)c3_lode_data(nod_u));
    }
    c3_assert(len_i == c3_list_len(lis_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(lis_u);
  }
}

static void
_test_pop_back(void)
{
  // Push a bunch of numbers onto the front and then pop them off the back
  // (COPY).
  {
    c3_list* lis_u;
    c3_lode* nod_u;
    c3_assert(lis_u = c3_list_init(C3_LIST_COPY));
    c3_assert(!c3_list_peekf(lis_u));
    size_t len_i = 10;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      c3_list_pushf(lis_u, &idx_i, sizeof(idx_i));
      c3_assert(nod_u = c3_list_peekf(lis_u));
      c3_assert(idx_i == *(size_t*)c3_lode_data(nod_u));
    }
    c3_assert(len_i == c3_list_len(lis_u));

    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(len_i - idx_i == c3_list_len(lis_u));
      c3_assert(nod_u = c3_list_peekb(lis_u));
      c3_assert(idx_i == *(size_t*)c3_lode_data(nod_u));
      c3_assert(nod_u = c3_list_popb(lis_u));
      c3_assert(idx_i == *(size_t*)c3_lode_data(nod_u));
      c3_free(nod_u);
      if ( 0 < c3_list_len(lis_u) ) {
        c3_assert(nod_u = c3_list_peekb(lis_u));
        c3_assert(idx_i != *(size_t*)c3_lode_data(nod_u));
      }
    }
    c3_assert(0 == c3_list_len(lis_u));
    c3_free(lis_u);
  }

  // Push a bunch of numbers onto the front and then pop them off the back
  // (TRANSFER).
  {
    c3_list* lis_u;
    c3_lode* nod_u;
    c3_assert(lis_u = c3_list_init(C3_LIST_TRANSFER));
    c3_assert(!c3_list_peekf(lis_u));
    size_t len_i    = 10;
    size_t nums_i[] = {
      0,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
    };

    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      c3_list_pushf(lis_u, &nums_i[idx_i], sizeof(nums_i[idx_i]));
      c3_assert(nod_u = c3_list_peekf(lis_u));
      c3_assert(idx_i == **(size_t**)c3_lode_data(nod_u));
    }
    c3_assert(len_i == c3_list_len(lis_u));

    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(len_i - idx_i == c3_list_len(lis_u));
      c3_assert(nod_u = c3_list_peekb(lis_u));
      c3_assert(idx_i == **(size_t**)c3_lode_data(nod_u));
      c3_assert(nod_u = c3_list_popb(lis_u));
      c3_assert(idx_i == **(size_t**)c3_lode_data(nod_u));
      c3_free(nod_u);
      if ( 0 < c3_list_len(lis_u) ) {
        c3_assert(nod_u = c3_list_peekb(lis_u));
        c3_assert(idx_i != **(size_t**)c3_lode_data(nod_u));
      }
    }
    c3_assert(0 == c3_list_len(lis_u));
    c3_free(lis_u);
  }

  // Push a bunch of strings onto the front and then pop them off the back
  // (COPY).
  {
    c3_list*     lis_u;
    c3_lode*     nod_u;
    static char* strs_c[] = {
      "antonio",
      "bingbing",
      "catherine",
      "deandre",
      "emir",
    };
    c3_assert(lis_u = c3_list_init(C3_LIST_COPY));
    c3_assert(!c3_list_peekf(lis_u));
    size_t len_i = _arrlen(strs_c);
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      char* str_c = strs_c[idx_i];
      c3_list_pushf(lis_u, str_c, 1 + strlen(str_c));
      c3_assert(nod_u = c3_list_peekf(lis_u));
      c3_assert(0 == strcmp(str_c, c3_lode_data(nod_u)));
    }
    c3_assert(len_i == c3_list_len(lis_u));

    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(len_i - idx_i == c3_list_len(lis_u));
      char* str_c = strs_c[idx_i];
      c3_assert(nod_u = c3_list_peekb(lis_u));
      c3_assert(0 == strcmp(str_c, c3_lode_data(nod_u)));
      c3_assert(nod_u = c3_list_popb(lis_u));
      c3_assert(0 == strcmp(str_c, c3_lode_data(nod_u)));
      c3_free(nod_u);
      if ( 0 < c3_list_len(lis_u) ) {
        c3_assert(nod_u = c3_list_peekb(lis_u));
        c3_assert(0 != strcmp(str_c, c3_lode_data(nod_u)));
      }
    }
    c3_assert(0 == c3_list_len(lis_u));
    c3_free(lis_u);
  }

  // Push a bunch of strings onto the front and then pop them off the back
  // (TRANSFER).
  {
    c3_list*     lis_u;
    c3_lode*     nod_u;
    static char* strs_c[] = {
      "antonio",
      "bingbing",
      "catherine",
      "deandre",
      "emir",
    };
    c3_assert(lis_u = c3_list_init(C3_LIST_TRANSFER));
    c3_assert(!c3_list_peekf(lis_u));
    size_t len_i = _arrlen(strs_c);
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      char* str_c = strs_c[idx_i];
      c3_list_pushf(lis_u, str_c, 1 + strlen(str_c));
      c3_assert(nod_u = c3_list_peekf(lis_u));
      c3_assert(0 == strcmp(str_c, *(c3_c**)c3_lode_data(nod_u)));
    }
    c3_assert(len_i == c3_list_len(lis_u));

    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(len_i - idx_i == c3_list_len(lis_u));
      char* str_c = strs_c[idx_i];
      c3_assert(nod_u = c3_list_peekb(lis_u));
      c3_assert(0 == strcmp(str_c, *(c3_c**)c3_lode_data(nod_u)));
      c3_assert(nod_u = c3_list_popb(lis_u));
      c3_assert(0 == strcmp(str_c, *(c3_c**)c3_lode_data(nod_u)));
      c3_free(nod_u);
      if ( 0 < c3_list_len(lis_u) ) {
        c3_assert(nod_u = c3_list_peekb(lis_u));
        c3_assert(0 != strcmp(str_c, *(c3_c**)c3_lode_data(nod_u)));
      }
    }
    c3_assert(0 == c3_list_len(lis_u));
    c3_free(lis_u);
  }
}

static void
_test_pop_front(void)
{
  // Push a bunch of numbers onto the back and then pop them off the front
  // (COPY).
  {
    c3_list* lis_u;
    c3_lode* nod_u;
    c3_assert(lis_u = c3_list_init(C3_LIST_COPY));
    c3_assert(!c3_list_peekb(lis_u));
    size_t len_i = 10;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      c3_list_pushb(lis_u, &idx_i, sizeof(idx_i));
      c3_assert(nod_u = c3_list_peekb(lis_u));
      c3_assert(idx_i == *(size_t*)c3_lode_data(nod_u));
    }
    c3_assert(len_i == c3_list_len(lis_u));

    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(len_i - idx_i == c3_list_len(lis_u));
      c3_assert(nod_u = c3_list_peekf(lis_u));
      c3_assert(idx_i == *(size_t*)c3_lode_data(nod_u));
      c3_assert(nod_u = c3_list_popf(lis_u));
      c3_assert(idx_i == *(size_t*)c3_lode_data(nod_u));
      c3_free(nod_u);
      if ( 0 < c3_list_len(lis_u) ) {
        c3_assert(nod_u = c3_list_peekf(lis_u));
        c3_assert(idx_i != *(size_t*)c3_lode_data(nod_u));
      }
    }
    c3_assert(0 == c3_list_len(lis_u));
    c3_free(lis_u);
  }

  // Push a bunch of numbers onto the back and then pop them off the front
  // (TRANSFER).
  {
    c3_list* lis_u;
    c3_lode* nod_u;
    c3_assert(lis_u = c3_list_init(C3_LIST_TRANSFER));
    c3_assert(!c3_list_peekb(lis_u));
    size_t len_i    = 10;
    size_t nums_i[] = {
      0,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
    };
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      c3_list_pushb(lis_u, &nums_i[idx_i], sizeof(nums_i[idx_i]));
      c3_assert(nod_u = c3_list_peekb(lis_u));
      c3_assert(idx_i == **(size_t**)c3_lode_data(nod_u));
    }
    c3_assert(len_i == c3_list_len(lis_u));

    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(len_i - idx_i == c3_list_len(lis_u));
      c3_assert(nod_u = c3_list_peekf(lis_u));
      c3_assert(idx_i == **(size_t**)c3_lode_data(nod_u));
      c3_assert(nod_u = c3_list_popf(lis_u));
      c3_assert(idx_i == **(size_t**)c3_lode_data(nod_u));
      c3_free(nod_u);
      if ( 0 < c3_list_len(lis_u) ) {
        c3_assert(nod_u = c3_list_peekf(lis_u));
        c3_assert(idx_i != **(size_t**)c3_lode_data(nod_u));
      }
    }
    c3_assert(0 == c3_list_len(lis_u));
    c3_free(lis_u);
  }

  // Push a bunch of strings (COPY).
  {
    c3_list*     lis_u;
    c3_lode*     nod_u;
    static char* strs_c[] = {
      "antonio",
      "bingbing",
      "catherine",
      "deandre",
      "emir",
    };
    c3_assert(lis_u = c3_list_init(C3_LIST_COPY));
    c3_assert(!c3_list_peekb(lis_u));
    size_t len_i = _arrlen(strs_c);
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      char* str_c = strs_c[idx_i];
      c3_list_pushb(lis_u, str_c, 1 + strlen(str_c));
      c3_assert(nod_u = c3_list_peekb(lis_u));
      c3_assert(0 == strcmp(str_c, c3_lode_data(nod_u)));
    }

    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(len_i - idx_i == c3_list_len(lis_u));
      char* str_c = strs_c[idx_i];
      c3_assert(nod_u = c3_list_peekf(lis_u));
      c3_assert(0 == strcmp(str_c, c3_lode_data(nod_u)));
      c3_assert(nod_u = c3_list_popf(lis_u));
      c3_assert(0 == strcmp(str_c, c3_lode_data(nod_u)));
      c3_free(nod_u);
      if ( 0 < c3_list_len(lis_u) ) {
        c3_assert(nod_u = c3_list_peekf(lis_u));
        c3_assert(0 != strcmp(str_c, c3_lode_data(nod_u)));
      }
    }
    c3_assert(0 == c3_list_len(lis_u));
    c3_free(lis_u);
  }

  // Push a bunch of strings (TRANSFER).
  {
    c3_list*     lis_u;
    c3_lode*     nod_u;
    static char* strs_c[] = {
      "antonio",
      "bingbing",
      "catherine",
      "deandre",
      "emir",
    };
    c3_assert(lis_u = c3_list_init(C3_LIST_TRANSFER));
    c3_assert(!c3_list_peekb(lis_u));
    size_t len_i = _arrlen(strs_c);
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      char* str_c = strs_c[idx_i];
      c3_list_pushb(lis_u, str_c, 1 + strlen(str_c));
      c3_assert(nod_u = c3_list_peekb(lis_u));
      c3_assert(0 == strcmp(str_c, *(c3_c**)c3_lode_data(nod_u)));
    }

    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(len_i - idx_i == c3_list_len(lis_u));
      char* str_c = strs_c[idx_i];
      c3_assert(nod_u = c3_list_peekf(lis_u));
      c3_assert(0 == strcmp(str_c, *(c3_c**)c3_lode_data(nod_u)));
      c3_assert(nod_u = c3_list_popf(lis_u));
      c3_assert(0 == strcmp(str_c, *(c3_c**)c3_lode_data(nod_u)));
      c3_free(nod_u);
      if ( 0 < c3_list_len(lis_u) ) {
        c3_assert(nod_u = c3_list_peekf(lis_u));
        c3_assert(0 != strcmp(str_c, *(c3_c**)c3_lode_data(nod_u)));
      }
    }
    c3_assert(0 == c3_list_len(lis_u));
    c3_free(lis_u);
  }
}

static void
_test_iter(void)
{
  // Iterate front to back (COPY).
  {
    c3_list* lis_u;
    c3_assert(lis_u = c3_list_init(C3_LIST_COPY));
    for ( size_t idx_i = 0; idx_i < 1000; idx_i++ ) {
      c3_list_pushb(lis_u, &idx_i, sizeof(idx_i));
    }

    c3_lode* nod_u = c3_list_peekf(lis_u);
    size_t   idx_i = 0;
    while ( nod_u ) {
      c3_assert(idx_i++ == *(size_t*)c3_lode_data(nod_u));
      nod_u = c3_lode_next(nod_u);
    }
  }

  // Iterate back to front (COPY).
  {
    c3_list* lis_u;
    c3_assert(lis_u = c3_list_init(C3_LIST_COPY));
    for ( size_t idx_i = 0; idx_i < 1000; idx_i++ ) {
      c3_list_pushf(lis_u, &idx_i, sizeof(idx_i));
    }

    c3_lode* nod_u = c3_list_peekb(lis_u);
    size_t   idx_i = 0;
    while ( nod_u ) {
      c3_assert(idx_i++ == *(size_t*)c3_lode_data(nod_u));
      nod_u = c3_lode_prev(nod_u);
    }
  }
}

int
main(int argc, char* argv[])
{
  _test_init();
  _test_length();
  _test_push_back();
  _test_push_front();
  _test_push_mixed();
  _test_pop_back();
  _test_pop_front();
  _test_iter();

  fprintf(stderr, "test_list: ok\r\n");

  return 0;
}

#undef _arrlen
