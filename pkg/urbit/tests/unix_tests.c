#include "all.h"
#include "vere/vere.h"

/* _setup(): prepare for tests.
*/
static void
_setup(void)
{
}

/* _test_safe():
*/
static c3_i
_test_safe()
{
  c3_i ret_i = 1;

  if ( !u3_unix_safe("a") ||
       !u3_unix_safe("a/b") ||
       !u3_unix_safe("a/b/c/defg/h/ijklmnop") )
  {
    fprintf(stderr, "_safe fail 1\n");
    ret_i = 0;
  }

  if ( u3_unix_safe("") ||
       u3_unix_safe(".") ||
       u3_unix_safe("..") ||
       u3_unix_safe("/.") ||
       u3_unix_safe("a/b/c//") ||
       u3_unix_safe("a/b/.") ||
       u3_unix_safe("/././../.") ||
       u3_unix_safe("/../etc") )
  {
    fprintf(stderr, "_safe fail 2\r\n");
    ret_i = 0;
  }

  if ( !u3_unix_safe(".a") ||
       !u3_unix_safe("/.a.b.c/..c") )
  {
    fprintf(stderr, "_safe fail 3\r\n");
    ret_i = 0;
  }

  return ret_i;
}

/* main(): run all test cases.
*/
int
main(int argc, char* argv[])
{
  _setup();

  if ( !_test_safe() ) {
    fprintf(stderr, "test unix: failed\r\n");
    exit(1);
  }

  fprintf(stderr, "test unix: ok\r\n");
  return 0;
}
