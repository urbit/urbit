#include "all.h"

/* _setup(): prepare for tests.
*/
static void
_setup(void)
{
  u3m_init();
  u3m_pave(c3y, c3n);
}

/* _test_mug(): spot check u3r_mug hashes.
*/
static void
_test_mug(void)
{
  if ( 0x4d441035 != u3r_mug_string("Hello, world!") ) {
    fprintf(stderr, "fail (a)\r\n");
    exit(1);
  }

  if ( 0x4d441035 != u3r_mug(u3i_string("Hello, world!")) ) {
    fprintf(stderr, "fail (b)\r\n");
    exit(1);
  }

  if ( 0x79ff04e8 != u3r_mug_bytes(0, 0) ) {
    fprintf(stderr, "fail (c)\r\n");
    exit(1);
  }

  if ( 0x64dfda5c != u3r_mug(u3i_string("xxxxxxxxxxxxxxxxxxxxxxxxxxxx")) ) {
    fprintf(stderr, "fail (d)\r\n");
    exit(1);
  }

  if ( 0x389ca03a != u3r_mug_cell(0, 0) ) {
    fprintf(stderr, "fail (e)\r\n");
    exit(1);
  }

  if ( 0x389ca03a != u3r_mug_cell(1, 1) ) {
    fprintf(stderr, "fail (f)\r\n");
    exit(1);
  }

  if ( 0x5258a6c0 != u3r_mug_cell(0, u3qc_bex(32)) ) {
    fprintf(stderr, "fail (g)\r\n");
    exit(1);
  }

  if ( 0x2ad39968 != u3r_mug_cell(u3qa_dec(u3qc_bex(128)), 1) ) {
    fprintf(stderr, "fail (h)\r\n");
    exit(1);
  }

  {
    //  stick some zero bytes in a string
    //
    u3_noun str = u3kc_lsh(3, 1,
                           u3kc_mix(u3qc_bex(212),
                           u3i_string("abcdefjhijklmnopqrstuvwxyz")));

    c3_w  byt_w = u3r_met(3, str);
    c3_w  wor_w = u3r_met(5, str);
    c3_y* str_y = c3_malloc(byt_w);
    c3_w* str_w = c3_malloc(4 * wor_w);
    c3_d  str_d = 0;

    u3r_bytes(0, byt_w, str_y, str);
    u3r_words(0, wor_w, str_w, str);

    str_d |= str_w[0];
    str_d |= ((c3_d)str_w[1] << 32ULL);

    if ( 0x34d08717 != u3r_mug(str) ) {
      fprintf(stderr, "fail (i) (1) \r\n");
      exit(1);
    }
    if ( 0x34d08717 != u3r_mug_bytes(str_y, byt_w) ) {
      fprintf(stderr, "fail (i) (2)\r\n");
      exit(1);
    }
    if ( 0x34d08717 != u3r_mug_words(str_w, wor_w) ) {
      fprintf(stderr, "fail (i) (3)\r\n");
      exit(1);
    }
    if ( u3r_mug_words(str_w, 2) != u3r_mug_chub(str_d) ) {
      fprintf(stderr, "fail (i) (4)\r\n");
      exit(1);
    }

    c3_free(str_y);
    c3_free(str_w);
  }

  fprintf(stderr, "test_mug: ok\n");
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
