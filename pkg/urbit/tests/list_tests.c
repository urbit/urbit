//! @file list_tests.c

#include "c/list.h"

#include "c/portable.h"
#include "c/types.h"
#include "c/defs.h"

#define _arrlen(arr) (sizeof(arr)/sizeof(arr[0]))

static void
_test_init(void)
{
  {
    c3_list* lis_u = c3_list_init();
    c3_assert(NULL != lis_u);
    c3_free(lis_u);
  }
}

static void
_test_length(void)
{
  // Get length of NULL.
  {
    c3_assert(0 == c3_list_length(NULL));
  }

  // Get length of empty list.
  {
    c3_list* lis_u = c3_list_init();
    c3_assert(0 == c3_list_length(lis_u));
    c3_free(lis_u);
  }
}

static void
_test_push_back(void)
{
  // Push a bunch of numbers.
  {
    c3_list* lis_u = c3_list_init();
    c3_assert(NULL == c3_list_peek_back(lis_u));
    size_t len_i = 10;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_length(lis_u));
      size_t* dat_i = c3_list_push_back(lis_u, &idx_i, sizeof(idx_i));
      c3_assert(NULL != dat_i);
      c3_assert(idx_i == *dat_i);
      c3_assert(idx_i == *(size_t*)c3_list_peek_back(lis_u));
    }
    c3_assert(len_i == c3_list_length(lis_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(lis_u);
  }

  // Push a bunch of strings.
  {
    c3_list* lis_u = c3_list_init();
    c3_assert(NULL == c3_list_peek_back(lis_u));
    static char* strs_c[] = {
      "antonio",
      "bingbing",
      "catherine",
      "deandre",
      "emir",
    };
    size_t len_i = _arrlen(strs_c);
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_length(lis_u));
      char* str_c = strs_c[idx_i];
      char* dat_c = c3_list_push_back(lis_u, str_c, 1 + strlen(str_c));
      c3_assert(NULL != dat_c);
      c3_assert(0 == strcmp(str_c, dat_c));
      c3_assert(0 == strcmp(str_c, (char*)c3_list_peek_back(lis_u)));
    }
    c3_assert(len_i == c3_list_length(lis_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(lis_u);
  }
}

static void
_test_push_front(void)
{
  // Push a bunch of numbers.
  {
    c3_list* lis_u = c3_list_init();
    c3_assert(NULL == c3_list_peek_front(lis_u));
    size_t len_i = 10;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_length(lis_u));
      size_t* dat_i = c3_list_push_front(lis_u, &idx_i, sizeof(idx_i));
      c3_assert(NULL != dat_i);
      c3_assert(idx_i == *dat_i);
      c3_assert(idx_i == *(size_t*)c3_list_peek_front(lis_u));
    }
    c3_assert(len_i == c3_list_length(lis_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(lis_u);
  }

  // Push a bunch of strings.
  {
    c3_list* lis_u = c3_list_init();
    c3_assert(NULL == c3_list_peek_front(lis_u));
    static char* strs_c[] = {
      "antonio",
      "bingbing",
      "catherine",
      "deandre",
      "emir",
    };
    size_t len_i = _arrlen(strs_c);
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_length(lis_u));
      char* str_c = strs_c[idx_i];
      char* dat_c = c3_list_push_front(lis_u, str_c, 1 + strlen(str_c));
      c3_assert(NULL != dat_c);
      c3_assert(0 == strcmp(str_c, dat_c));
      c3_assert(0 == strcmp(str_c, (char*)c3_list_peek_front(lis_u)));
    }
    c3_assert(len_i == c3_list_length(lis_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(lis_u);
  }
}

static void
_test_push_mixed(void)
{
  // Push even numbers onto the front and odd numbers onto the back.
  {
    c3_list* lis_u = c3_list_init();
    size_t len_i = 100;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_length(lis_u));
      size_t* dat_i = NULL;
      if ( 0 == idx_i % 2 ) {
        dat_i = c3_list_push_front(lis_u, &idx_i, sizeof(idx_i));
      }
      else {
        dat_i = c3_list_push_back(lis_u, &idx_i, sizeof(idx_i));
      }
      c3_assert(NULL != dat_i);
      c3_assert(idx_i == *dat_i);
    }
    c3_assert(len_i == c3_list_length(lis_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(lis_u);
  }
}

static void
_test_pop_back(void)
{
  // Push a bunch of numbers onto the front and then pop them off the back.
  {
    c3_list* lis_u = c3_list_init();
    c3_assert(NULL == c3_list_peek_front(lis_u));
    size_t len_i = 10;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_length(lis_u));
      size_t* dat_i = c3_list_push_front(lis_u, &idx_i, sizeof(idx_i));
      c3_assert(NULL != dat_i);
      c3_assert(idx_i == *dat_i);
      c3_assert(idx_i == *(size_t*)c3_list_peek_front(lis_u));
    }
    c3_assert(len_i == c3_list_length(lis_u));

    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(len_i - idx_i == c3_list_length(lis_u));
      c3_assert(idx_i == *(size_t*)c3_list_peek_back(lis_u));
      size_t* dat_i = c3_list_pop_back(lis_u);
      c3_assert(NULL != dat_i);
      c3_assert(idx_i == *dat_i);
      if ( 0 < c3_list_length(lis_u) ) {
        c3_assert(idx_i != *(size_t*)c3_list_peek_back(lis_u));
      }
      c3_free(dat_i);
    }
    c3_assert(0 == c3_list_length(lis_u));
    c3_free(lis_u);
  }

  // Push a bunch of strings onto the front and then pop them off the back.
  {
    c3_list* lis_u = c3_list_init();
    c3_assert(NULL == c3_list_peek_front(lis_u));
    static char* strs_c[] = {
      "antonio",
      "bingbing",
      "catherine",
      "deandre",
      "emir",
    };
    size_t len_i = _arrlen(strs_c);
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_length(lis_u));
      char* str_c = strs_c[idx_i];
      char* dat_c = c3_list_push_front(lis_u, str_c, 1 + strlen(str_c));
      c3_assert(NULL != dat_c);
      c3_assert(0 == strcmp(str_c, dat_c));
      c3_assert(0 == strcmp(str_c, (char*)c3_list_peek_front(lis_u)));
    }
    c3_assert(len_i == c3_list_length(lis_u));

    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(len_i - idx_i == c3_list_length(lis_u));
      char* str_c = strs_c[idx_i];
      c3_assert(0 == strcmp(str_c, c3_list_peek_back(lis_u)));
      char* dat_c = c3_list_pop_back(lis_u);
      c3_assert(NULL != dat_c);
      c3_assert(0 == strcmp(str_c, dat_c));
      if ( 0 < c3_list_length(lis_u) ) {
        c3_assert(0 != strcmp(str_c, c3_list_peek_back(lis_u)));
      }
      c3_free(dat_c);
    }
    c3_assert(0 == c3_list_length(lis_u));
    c3_free(lis_u);
  }
}

static void
_test_pop_front(void)
{
  // Push a bunch of numbers onto the back and then pop them off the front.
  {
    c3_list* lis_u = c3_list_init();
    c3_assert(NULL == c3_list_peek_back(lis_u));
    size_t len_i = 10;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_length(lis_u));
      size_t* dat_i = c3_list_push_back(lis_u, &idx_i, sizeof(idx_i));
      c3_assert(NULL != dat_i);
      c3_assert(idx_i == *dat_i);
      c3_assert(idx_i == *(size_t*)c3_list_peek_back(lis_u));
    }
    c3_assert(len_i == c3_list_length(lis_u));

    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(len_i - idx_i == c3_list_length(lis_u));
      c3_assert(idx_i == *(size_t*)c3_list_peek_front(lis_u));
      size_t* dat_i = c3_list_pop_front(lis_u);
      c3_assert(NULL != dat_i);
      c3_assert(idx_i == *dat_i);
      if ( 0 < c3_list_length(lis_u) ) {
        c3_assert(idx_i != *(size_t*)c3_list_peek_front(lis_u));
      }
      c3_free(dat_i);
    }
    c3_assert(0 == c3_list_length(lis_u));
    c3_free(lis_u);
  }

  // Push a bunch of strings.
  {
    c3_list* lis_u = c3_list_init();
    c3_assert(NULL == c3_list_peek_back(lis_u));
    static char* strs_c[] = {
      "antonio",
      "bingbing",
      "catherine",
      "deandre",
      "emir",
    };
    size_t len_i = _arrlen(strs_c);
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_length(lis_u));
      char* str_c = strs_c[idx_i];
      char* dat_c = c3_list_push_back(lis_u, str_c, 1 + strlen(str_c));
      c3_assert(NULL != dat_c);
      c3_assert(0 == strcmp(str_c, dat_c));
      c3_assert(0 == strcmp(str_c, (char*)c3_list_peek_back(lis_u)));
    }

    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(len_i - idx_i == c3_list_length(lis_u));
      char* str_c = strs_c[idx_i];
      c3_assert(0 == strcmp(str_c, c3_list_peek_front(lis_u)));
      char* dat_c = c3_list_pop_front(lis_u);
      c3_assert(NULL != dat_c);
      c3_assert(0 == strcmp(str_c, dat_c));
      if ( 0 < c3_list_length(lis_u) ) {
        c3_assert(0 != strcmp(str_c, c3_list_peek_front(lis_u)));
      }
      c3_free(dat_c);
    }
    c3_assert(0 == c3_list_length(lis_u));
    c3_free(lis_u);
  }
}

static void
_test_iter_init(void)
{
  // Create iterator over NULL.
  {
    c3_assert(NULL == c3_list_iter_init(NULL, C3_LIST_ITER_FRONT));
    c3_assert(NULL == c3_list_iter_init(NULL, C3_LIST_ITER_BACK));
  }

  // Create iterator over empty list.
  {
    c3_list* lis_u = c3_list_init();
    c3_assert(NULL != lis_u);
    c3_assert(NULL == c3_list_iter_init(lis_u, C3_LIST_ITER_FRONT));
    c3_assert(NULL == c3_list_iter_init(lis_u, C3_LIST_ITER_BACK));
    c3_free(lis_u);
  }

  // Create duplicate iterator.
  {
    c3_list* lis_u = c3_list_init();
    c3_assert(NULL != lis_u);
    size_t dat_i = 137;
    c3_assert(dat_i == *(size_t*)c3_list_push_back(lis_u, &dat_i, sizeof(dat_i)));
    c3_list_iter* itr_u = c3_list_iter_init(lis_u, C3_LIST_ITER_FRONT);
    c3_assert(NULL != itr_u);
    c3_assert(NULL == c3_list_iter_init(lis_u, C3_LIST_ITER_BACK));
    c3_list_iter_free(lis_u, itr_u);
    c3_free(c3_list_pop_front(lis_u));
    c3_free(lis_u);
  }

  // Use invalid starting point.
  {
    c3_list* lis_u = c3_list_init();
    c3_assert(NULL != lis_u);
    size_t dat_i = 137;
    c3_assert(dat_i == *(size_t*)c3_list_push_back(lis_u, &dat_i, sizeof(dat_i)));
    c3_list_iter* itr_u = c3_list_iter_init(lis_u, 2 * (C3_LIST_ITER_FRONT + C3_LIST_ITER_BACK));
    c3_assert(NULL == itr_u);
    c3_list_iter_free(lis_u, itr_u);
    c3_free(c3_list_pop_front(lis_u));
    c3_free(lis_u);
  }
}

static void
_test_iter_step(void)
{
  // Set up.
  c3_list* lis_u = c3_list_init();
  assert(NULL != lis_u);
  size_t len_i = 100;
  for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
    c3_list_push_back(lis_u, &idx_i, sizeof(idx_i));
  }

  // Step front to back.
  {
    c3_list_iter* itr_u = c3_list_iter_init(lis_u, C3_LIST_ITER_FRONT);
    c3_assert(NULL != itr_u);
    size_t idx_i = 0;
    while ( 1 ) {
      size_t* dat_v = c3_list_iter_step(lis_u, itr_u);
      if ( NULL == dat_v ) {
        break;
      }
      c3_assert(idx_i == *dat_v);
      idx_i++;
    }
    c3_assert(len_i == idx_i);
    c3_list_iter_free(lis_u, itr_u);
  }


  // Step back to front.
  {
    c3_list_iter* itr_u = c3_list_iter_init(lis_u, C3_LIST_ITER_BACK);
    c3_assert(NULL != itr_u);
    size_t idx_i = 0;
    while ( 1 ) {
      size_t* dat_v = c3_list_iter_step(lis_u, itr_u);
      if ( NULL == dat_v ) {
        break;
      }
      c3_assert((len_i - idx_i - 1) == *dat_v);
      idx_i++;
    }
    c3_assert(len_i == idx_i);
    c3_list_iter_free(lis_u, itr_u);
  }

  // Clean up.
  while ( 0 < c3_list_length(lis_u) ) {
    c3_free(c3_list_pop_front(lis_u));
  }
  c3_free(lis_u);
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
  _test_iter_init();

  fprintf(stderr, "test_list: ok\r\n");

  return 0;
}

#undef _arrlen
