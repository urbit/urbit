#include "all.h"

/* _setup(): prepare for tests.
*/
static void
_setup(void)
{
  u3m_init(c3y);
  u3m_pave(c3y, c3n);
}

/* _test_mur(): spot check u3r_mur hashes.
*/
static void
_test_mur(void)
{
  if ( 0x4d441035 != u3r_mur_string("Hello, world!") ) {
    fprintf(stderr, "fail\r\n");
    exit(1);
  }

  if ( 0x4d441035 != u3r_mur(u3i_string("Hello, world!")) ) {
    fprintf(stderr, "fail\r\n");
    exit(1);
  }

  if ( 0x79ff04e8 != u3r_mur_bytes(0, 0) ) {
    fprintf(stderr, "fail\r\n");
    exit(1);
  }

  if ( 0x64dfda5c != u3r_mur(u3i_string("xxxxxxxxxxxxxxxxxxxxxxxxxxxx")) ) {
    fprintf(stderr, "fail\r\n");
    exit(1);
  }

  if ( 0x389ca03a != u3r_mur_cell(0, 0) ) {
    fprintf(stderr, "fail\r\n");
    exit(1);
  }

  if ( 0x389ca03a != u3r_mur_cell(1, 1) ) {
    fprintf(stderr, "fail\r\n");
    exit(1);
  }

  if ( 0x5258a6c0 != u3r_mur_cell(0, u3qc_bex(32)) ) {
    fprintf(stderr, "fail\r\n");
    exit(1);
  }

  if ( 0x2ad39968 != u3r_mur_cell(u3qa_dec(u3qc_bex(128)), 1) ) {
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

  _test_mur();

  return 0;
}
