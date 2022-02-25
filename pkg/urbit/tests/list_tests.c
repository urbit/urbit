//! @file list_tests.c

#include "c/list.h"

#include "c/defs.h"
#include "c/portable.h"
#include "c/types.h"

#define _arrlen(arr) (sizeof(arr) / sizeof(arr[0]))

static void
_test_init(void)
{
  {
    c3_list* lis_u = c3_list_init();
    c3_assert(lis_u);
    c3_free(lis_u);
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
    c3_list* lis_u = c3_list_init();
    c3_assert(0 == c3_list_len(lis_u));
    c3_free(lis_u);
  }
}

static void
_test_push_back(void)
{
  // Push a bunch of numbers.
  {
    c3_list*      lis_u;
    c3_list_node* nod_u;
    c3_assert(lis_u = c3_list_init());
    c3_assert(!c3_list_peekb(lis_u));
    size_t len_i = 10;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      c3_list_pushb(lis_u, &idx_i, sizeof(idx_i));
      c3_assert(nod_u = c3_list_peekb(lis_u));
      c3_assert(idx_i == *(size_t*)c3_list_data(nod_u));
    }
    c3_assert(len_i == c3_list_len(lis_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(lis_u);
  }

  // Push a bunch of strings.
  {
    c3_list*      lis_u;
    c3_list_node* nod_u;
    static char*  strs_c[] = {
      "antonio",
      "bingbing",
      "catherine",
      "deandre",
      "emir",
    };
    c3_assert(lis_u = c3_list_init());
    c3_assert(!c3_list_peekb(lis_u));
    size_t len_i = _arrlen(strs_c);
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      char* str_c = strs_c[idx_i];
      c3_list_pushb(lis_u, str_c, 1 + strlen(str_c));
      c3_assert(nod_u = c3_list_peekb(lis_u));
      c3_assert(0 == strcmp(str_c, c3_list_data(nod_u)));
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
  // Push a bunch of numbers.
  {
    c3_list*      lis_u;
    c3_list_node* nod_u;
    c3_assert(lis_u = c3_list_init());
    c3_assert(!c3_list_peekf(lis_u));
    size_t len_i = 10;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      c3_list_pushf(lis_u, &idx_i, sizeof(idx_i));
      c3_assert(nod_u = c3_list_peekf(lis_u));
      c3_assert(idx_i == *(size_t*)c3_list_data(nod_u));
    }
    c3_assert(len_i == c3_list_len(lis_u));
    // We leak the memory associated with the nodes here, but that's okay
    // because we exit soon after.
    c3_free(lis_u);
  }

  // Push a bunch of strings.
  {
    c3_list*      lis_u;
    c3_list_node* nod_u;
    static char*  strs_c[] = {
      "antonio",
      "bingbing",
      "catherine",
      "deandre",
      "emir",
    };
    c3_assert(lis_u = c3_list_init());
    c3_assert(!c3_list_peekf(lis_u));
    size_t len_i = _arrlen(strs_c);
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      char* str_c = strs_c[idx_i];
      c3_list_pushf(lis_u, str_c, 1 + strlen(str_c));
      c3_assert(nod_u = c3_list_peekf(lis_u));
      c3_assert(0 == strcmp(str_c, c3_list_data(nod_u)));
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
  // Push even numbers onto the front and odd numbers onto the back.
  {
    c3_list*      lis_u;
    c3_list_node* nod_u;
    c3_assert(lis_u = c3_list_init());
    size_t len_i = 100;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      c3_list_end end_i = idx_i % 2;
      c3_list_push(lis_u, end_i, &idx_i, sizeof(idx_i));
      c3_assert(nod_u = c3_list_peek(lis_u, end_i));
      c3_assert(idx_i == *(size_t*)c3_list_data(nod_u));
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
  // Push a bunch of numbers onto the front and then pop them off the back.
  {
    c3_list*      lis_u;
    c3_list_node* nod_u;
    c3_assert(lis_u = c3_list_init());
    c3_assert(!c3_list_peekf(lis_u));
    size_t len_i = 10;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      c3_list_pushf(lis_u, &idx_i, sizeof(idx_i));
      c3_assert(nod_u = c3_list_peekf(lis_u));
      c3_assert(idx_i == *(size_t*)c3_list_data(nod_u));
    }
    c3_assert(len_i == c3_list_len(lis_u));

    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(len_i - idx_i == c3_list_len(lis_u));
      c3_assert(nod_u = c3_list_peekb(lis_u));
      c3_assert(idx_i == *(size_t*)c3_list_data(nod_u));
      c3_assert(nod_u = c3_list_popb(lis_u));
      c3_assert(idx_i == *(size_t*)c3_list_data(nod_u));
      c3_free(nod_u);
      if ( 0 < c3_list_len(lis_u) ) {
        c3_assert(nod_u = c3_list_peekb(lis_u));
        c3_assert(idx_i != *(size_t*)c3_list_data(nod_u));
      }
    }
    c3_assert(0 == c3_list_len(lis_u));
    c3_free(lis_u);
  }

  // Push a bunch of strings onto the front and then pop them off the back.
  {
    c3_list*      lis_u;
    c3_list_node* nod_u;
    static char*  strs_c[] = {
      "antonio",
      "bingbing",
      "catherine",
      "deandre",
      "emir",
    };
    c3_assert(lis_u = c3_list_init());
    c3_assert(!c3_list_peekf(lis_u));
    size_t len_i = _arrlen(strs_c);
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      char* str_c = strs_c[idx_i];
      c3_list_pushf(lis_u, str_c, 1 + strlen(str_c));
      c3_assert(nod_u = c3_list_peekf(lis_u));
      c3_assert(0 == strcmp(str_c, c3_list_data(nod_u)));
    }
    c3_assert(len_i == c3_list_len(lis_u));

    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(len_i - idx_i == c3_list_len(lis_u));
      char* str_c = strs_c[idx_i];
      c3_assert(nod_u = c3_list_peekb(lis_u));
      c3_assert(0 == strcmp(str_c, c3_list_data(nod_u)));
      c3_assert(nod_u = c3_list_popb(lis_u));
      c3_assert(0 == strcmp(str_c, c3_list_data(nod_u)));
      c3_free(nod_u);
      if ( 0 < c3_list_len(lis_u) ) {
        c3_assert(nod_u = c3_list_peekb(lis_u));
        c3_assert(0 != strcmp(str_c, c3_list_data(nod_u)));
      }
    }
    c3_assert(0 == c3_list_len(lis_u));
    c3_free(lis_u);
  }
}

static void
_test_pop_front(void)
{
  // Push a bunch of numbers onto the back and then pop them off the front.
  {
    c3_list*      lis_u;
    c3_list_node* nod_u;
    c3_assert(lis_u = c3_list_init());
    c3_assert(!c3_list_peekb(lis_u));
    size_t len_i = 10;
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      c3_list_pushb(lis_u, &idx_i, sizeof(idx_i));
      c3_assert(nod_u = c3_list_peekb(lis_u));
      c3_assert(idx_i == *(size_t*)c3_list_data(nod_u));
    }
    c3_assert(len_i == c3_list_len(lis_u));

    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(len_i - idx_i == c3_list_len(lis_u));
      c3_assert(nod_u = c3_list_peekf(lis_u));
      c3_assert(idx_i == *(size_t*)c3_list_data(nod_u));
      c3_assert(nod_u = c3_list_popf(lis_u));
      c3_assert(idx_i == *(size_t*)c3_list_data(nod_u));
      c3_free(nod_u);
      if ( 0 < c3_list_len(lis_u) ) {
        c3_assert(nod_u = c3_list_peekf(lis_u));
        c3_assert(idx_i != *(size_t*)c3_list_data(nod_u));
      }
    }
    c3_assert(0 == c3_list_len(lis_u));
    c3_free(lis_u);
  }

  // Push a bunch of strings.
  {
    c3_list*      lis_u;
    c3_list_node* nod_u;
    static char*  strs_c[] = {
      "antonio",
      "bingbing",
      "catherine",
      "deandre",
      "emir",
    };
    c3_assert(lis_u = c3_list_init());
    c3_assert(!c3_list_peekb(lis_u));
    size_t len_i = _arrlen(strs_c);
    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(idx_i == c3_list_len(lis_u));
      char* str_c = strs_c[idx_i];
      c3_list_pushb(lis_u, str_c, 1 + strlen(str_c));
      c3_assert(nod_u = c3_list_peekb(lis_u));
      c3_assert(0 == strcmp(str_c, c3_list_data(nod_u)));
    }

    for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
      c3_assert(len_i - idx_i == c3_list_len(lis_u));
      char* str_c = strs_c[idx_i];
      c3_assert(nod_u = c3_list_peekf(lis_u));
      c3_assert(0 == strcmp(str_c, c3_list_data(nod_u)));
      c3_assert(nod_u = c3_list_popf(lis_u));
      c3_assert(0 == strcmp(str_c, c3_list_data(nod_u)));
      c3_free(nod_u);
      if ( 0 < c3_list_len(lis_u) ) {
        c3_assert(nod_u = c3_list_peekf(lis_u));
        c3_assert(0 != strcmp(str_c, c3_list_data(nod_u)));
      }
    }
    c3_assert(0 == c3_list_len(lis_u));
    c3_free(lis_u);
  }
}

static void
_test_iter(void)
{
  // Iterate front to back.
  {
    c3_list* lis_u;
    c3_assert(lis_u = c3_list_init());
    for ( size_t idx_i = 0; idx_i < 1000; idx_i++ ) {
      c3_list_pushb(lis_u, &idx_i, sizeof(idx_i));
    }

    c3_list_node* nod_u = c3_list_peekf(lis_u);
    size_t        idx_i = 0;
    while ( nod_u ) {
      c3_assert(idx_i++ == *(size_t*)c3_list_data(nod_u));
      nod_u = nod_u->nex_u;
    }
  }

  // Iterate back to front.
  {
    c3_list* lis_u;
    c3_assert(lis_u = c3_list_init());
    for ( size_t idx_i = 0; idx_i < 1000; idx_i++ ) {
      c3_list_pushf(lis_u, &idx_i, sizeof(idx_i));
    }

    c3_list_node* nod_u = c3_list_peekb(lis_u);
    size_t        idx_i = 0;
    while ( nod_u ) {
      c3_assert(idx_i++ == *(size_t*)c3_list_data(nod_u));
      nod_u = nod_u->pre_u;
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
