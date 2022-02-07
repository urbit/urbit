//! @file queue_tests.c

#include "c/queue.h"

#include "c/portable.h"
#include "c/types.h"
#include "c/defs.h"

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

int
main(int argc, char* argv[])
{
  _test_init();
  _test_length();

  fprintf(stderr, "test_queue: ok\r\n");

  return 0;
}
