//! @file queue_tests.c

#include "c/queue.h"

#include "c/portable.h"
#include "c/types.h"
#include "c/defs.h"

#define _arrlen(arr) (sizeof(arr)/sizeof(arr[0]))

static void
_test_init(void)
{
  {
    c3_queue* que_u = c3_queue_init();
    c3_assert(NULL != que_u);
    c3_free(que_u);
  }
}

static void
_test_length(void)
{
  // Get length of NULL.
  {
    c3_assert(0 == c3_queue_length(NULL));
  }

  // Get length of empty queue.
  {
    c3_queue* que_u = c3_queue_init();
    c3_assert(0 == c3_queue_length(que_u));
    c3_free(que_u);
  }
}

static void
_test_push_back(void)
{
  // Push a bunch of numbers.
  {
    c3_queue* que_u = c3_queue_init();
    size_t len_i = 10;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_queue_length(que_u));
      size_t* dat_i = c3_queue_push_back(que_u, &idx_i, sizeof(idx_i));
      c3_assert(NULL != dat_i);
      c3_assert(idx_i == *dat_i);
    }
    c3_assert(len_i == c3_queue_length(que_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(que_u);
  }

  // Push a bunch of strings.
  {
    c3_queue* que_u = c3_queue_init();
    static char* strs_c[] = {
      "antonio",
      "bingbing",
      "catherine",
      "deandre",
      "emir",
    };
    size_t len_i = _arrlen(strs_c);
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_queue_length(que_u));
      char* str_c = strs_c[idx_i];
      char* dat_c = c3_queue_push_back(que_u, str_c, 1 + strlen(str_c));
      c3_assert(NULL != dat_c);
      c3_assert(0 == strcmp(str_c, dat_c));
    }
    c3_assert(len_i == c3_queue_length(que_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(que_u);
  }
}

static void
_test_push_front(void)
{
  // Push a bunch of numbers.
  {
    c3_queue* que_u = c3_queue_init();
    size_t len_i = 10;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_queue_length(que_u));
      size_t* dat_i = c3_queue_push_front(que_u, &idx_i, sizeof(idx_i));
      c3_assert(NULL != dat_i);
      c3_assert(idx_i == *dat_i);
    }
    c3_assert(len_i == c3_queue_length(que_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(que_u);
  }

  // Push a bunch of strings.
  {
    c3_queue* que_u = c3_queue_init();
    static char* strs_c[] = {
      "antonio",
      "bingbing",
      "catherine",
      "deandre",
      "emir",
    };
    size_t len_i = _arrlen(strs_c);
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_queue_length(que_u));
      char* str_c = strs_c[idx_i];
      char* dat_c = c3_queue_push_front(que_u, str_c, 1 + strlen(str_c));
      c3_assert(NULL != dat_c);
      c3_assert(0 == strcmp(str_c, dat_c));
    }
    c3_assert(len_i == c3_queue_length(que_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(que_u);
  }
}

static void
_test_push_mixed(void)
{
  // Push even numbers onto the front and odd numbers onto the back.
  {
    c3_queue* que_u = c3_queue_init();
    size_t len_i = 100;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_queue_length(que_u));
      size_t* dat_i = NULL;
      if ( 0 == idx_i % 2 ) {
        dat_i = c3_queue_push_front(que_u, &idx_i, sizeof(idx_i));
      }
      else {
        dat_i = c3_queue_push_back(que_u, &idx_i, sizeof(idx_i));
      }
      c3_assert(NULL != dat_i);
      c3_assert(idx_i == *dat_i);
    }
    c3_assert(len_i == c3_queue_length(que_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(que_u);
  }
}

int
main(int argc, char* argv[])
{
  _test_init();
  _test_length();
  _test_push_back();
  _test_push_front();

  fprintf(stderr, "test_queue: ok\r\n");

  return 0;
}

#undef _arrlen
