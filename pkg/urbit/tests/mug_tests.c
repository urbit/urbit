#include "all.h"

/* _setup(): prepare for tests.
*/
static void
_setup(void)
{
  u3m_init();
  u3m_pave(c3y);
}

/* _test_mug(): spot check u3r_mug hashes.
*/
static c3_i
_test_mug(void)
{
  c3_i ret_i = 1;

  if ( 0x4d441035 != u3r_mug_c("Hello, world!") ) {
    fprintf(stderr, "fail (a)\r\n");
    ret_i = 0;
  }

  {
    u3_noun a = u3i_string("Hello, world!");

    if ( 0x4d441035 != u3r_mug(a) ) {
      fprintf(stderr, "fail (b)\r\n");
      ret_i = 0;
    }

    u3z(a);
  }

  {
    c3_y byt_y[1];

    if ( 0x79ff04e8 != u3r_mug_bytes(0, 0) ) {
      fprintf(stderr, "fail (c) (0)\r\n");
      ret_i = 0;
    }

    byt_y[0] = 1;

    if ( 0x715c2a60 != u3r_mug_bytes(byt_y, 1) ) {
      fprintf(stderr, "fail (c) (1)\r\n");
      ret_i = 0;
    }

    byt_y[0] = 2;

    if ( 0x718b9468 != u3r_mug_bytes(byt_y, 1) ) {
      fprintf(stderr, "fail (c) (2)\r\n");
      ret_i = 0;
    }
  }

  if ( 0x3a811aec != u3r_mug_both(0x715c2a60, u3r_mug_cell(2, 3)) ) {
    fprintf(stderr, "fail (d)\r\n");
    ret_i = 0;
  }


  {
    if ( 0x192f5588 != u3r_mug_cell(0, 0) ) {
      fprintf(stderr, "fail (e) (1)\r\n");
      ret_i = 0;
    }

    if ( 0x6b32ec46 != u3r_mug_cell(1, 1) ) {
      fprintf(stderr, "fail (e) (2)\r\n");
      ret_i = 0;
    }

    if ( 0x2effe10 != u3r_mug_cell(2, 2) ) {
      fprintf(stderr, "fail (e) (3)\r\n");
      ret_i = 0;
    }
  }

  {
    u3_noun a = u3i_string("xxxxxxxxxxxxxxxxxxxxxxxxxxxx");

    if ( 0x64dfda5c != u3r_mug(a) ) {
      fprintf(stderr, "fail (f)\r\n");
      ret_i = 0;
    }

    u3z(a);
  }

  {
    u3_noun a = u3qc_bex(32);

    if ( 0x7cefb7f != u3r_mug_cell(0, a) ) {
      fprintf(stderr, "fail (g)\r\n");
      ret_i = 0;
    }

    u3z(a);
  }

  {
    u3_noun a = u3ka_dec(u3qc_bex(128));

    if ( 0x2aa06bfc != u3r_mug_cell(a, 1) ) {
      fprintf(stderr, "fail (h)\r\n");
      ret_i = 0;
    }

    u3z(a);
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
      ret_i = 0;
    }
    if ( 0x34d08717 != u3r_mug_bytes(str_y, byt_w) ) {
      fprintf(stderr, "fail (i) (2)\r\n");
      ret_i = 0;
    }
    if ( 0x34d08717 != u3r_mug_words(str_w, wor_w) ) {
      fprintf(stderr, "fail (i) (3)\r\n");
      ret_i = 0;
    }
    if ( u3r_mug_words(str_w, 2) != u3r_mug_chub(str_d) ) {
      fprintf(stderr, "fail (i) (4)\r\n");
      ret_i = 0;
    }

    c3_free(str_y);
    c3_free(str_w);
    u3z(str);
  }

  {
    c3_w  som_w[4] = { 0, 0, 0, 1 };
    u3_noun som    = u3i_words(4, som_w);

    if ( 0x519bd45c != u3r_mug(som) ) {
      fprintf(stderr, "fail (j) (1)\r\n");
      ret_i = 0;
    }

    if ( 0x519bd45c != u3r_mug_words(som_w, 4) ) {
      fprintf(stderr, "fail (j) (2)\r\n");
      ret_i = 0;
    }

    u3z(som);
  }

  {
    c3_w  som_w[4] = { 0, 1, 0, 1 };
    u3_noun som    = u3i_words(4, som_w);

    if ( 0x540eb8a9 != u3r_mug(som) ) {
      fprintf(stderr, "fail (k) (1)\r\n");
      ret_i = 0;
    }

    if ( 0x540eb8a9 != u3r_mug_words(som_w, 4) ) {
      fprintf(stderr, "fail (k) (2)\r\n");
      ret_i = 0;
    }

    u3z(som);
  }

  {
    c3_w  som_w[4] = { 1, 1, 0, 1 };
    u3_noun som    = u3i_words(4, som_w);

    if ( 0x319d28f9 != u3r_mug(som) ) {
      fprintf(stderr, "fail (l) (1)\r\n");
      ret_i = 0;
    }

    if ( 0x319d28f9 != u3r_mug_words(som_w, 4) ) {
      fprintf(stderr, "fail (l) (2)\r\n");
      ret_i = 0;
    }

    u3z(som);
  }

  {
    c3_w  som_w[4] = { 0, 0, 0, 0xffff };
    u3_noun som    = u3i_words(4, som_w);

    if ( 0x5230a260 != u3r_mug(som) ) {
      fprintf(stderr, "fail (m) (1)\r\n");
      ret_i = 0;
    }

    if ( 0x5230a260 != u3r_mug_words(som_w, 4) ) {
      fprintf(stderr, "fail (m) (2)\r\n");
      ret_i = 0;
    }

    u3z(som);
  }

  return ret_i;
}

/* main(): run all test cases.
*/
int
main(int argc, char* argv[])
{
  _setup();

  if ( !_test_mug() ) {
    fprintf(stderr, "test_mug: failed\r\n");
    exit(1);
  }

  //  GC
  //
  u3m_grab(u3_none);

  fprintf(stderr, "test_mug: ok\n");

  return 0;
}
