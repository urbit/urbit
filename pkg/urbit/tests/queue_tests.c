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

int
main(int argc, char* argv[])
{
  _test_init();

  fprintf(stderr, "test_queue: ok\r\n");

  return 0;
}
