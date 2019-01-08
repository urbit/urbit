#include "all.h"

/* _setup(): prepare for tests.
*/
static void
_setup(void)
{
  u3m_init(c3y);
  u3m_pave(c3y, c3n);
}

/* _test_mug(): spot check u3r_mug hashes.
*/
static void
_test_mug(void)
{
  if ( 0x4d441035 != u3r_mug_string("Hello, world!") ) {
    fprintf(stderr, "fail\r\n");
    exit(1);
  }

  if ( 0x4d441035 != u3r_mug(u3i_string("Hello, world!")) ) {
    fprintf(stderr, "fail\r\n");
    exit(1);
  }

  if ( 0x79ff04e8 != u3r_mug_bytes(0, 0) ) {
    fprintf(stderr, "fail\r\n");
    exit(1);
  }

  if ( 0x64dfda5c != u3r_mug(u3i_string("xxxxxxxxxxxxxxxxxxxxxxxxxxxx")) ) {
    fprintf(stderr, "fail\r\n");
    exit(1);
  }

  if ( 0x389ca03a != u3r_mug_cell(0, 0) ) {
    fprintf(stderr, "fail\r\n");
    exit(1);
  }

  if ( 0x389ca03a != u3r_mug_cell(1, 1) ) {
    fprintf(stderr, "fail\r\n");
    exit(1);
  }

  if ( 0x5258a6c0 != u3r_mug_cell(0, u3qc_bex(32)) ) {
    fprintf(stderr, "fail\r\n");
    exit(1);
  }

  if ( 0x2ad39968 != u3r_mug_cell(u3qa_dec(u3qc_bex(128)), 1) ) {
    fprintf(stderr, "fail\r\n");
    exit(1);
  }
}

/* main(): run all test cases.
*/
int
main(int argc, char* argv[])
{
  _setup();

  _test_mug();

  return 0;
}
