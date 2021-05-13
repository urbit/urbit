#include "all.h"

/* _setup(): prepare for tests.
*/
static void
_setup(void)
{
  u3m_init();
  u3m_pave(c3y);
}

static inline c3_i
_ud_good(c3_w num_w, const c3_c* num_c)
{
  u3_weak out;
  if ( num_w != (out = u3s_sift_ud_bytes(strlen(num_c), (c3_y*)num_c)) ) {
    if ( u3_none == out ) {
      fprintf(stderr, "sift_ud: %s fail; expected %u\r\n", num_c, num_w);
    }
    else {
      fprintf(stderr, "sift_ud: %s wrong; expected %u: actual %u\r\n", num_c, num_w, out);
    }
    return 0;
  }

  return 1;
}

static inline c3_i
_ud_fail(const c3_c* num_c)
{
  u3_weak out;
  if ( u3_none != (out = u3s_sift_ud_bytes(strlen(num_c), (c3_y*)num_c)) ) {
    u3m_p("out", out);
    fprintf(stderr, "sift_ud: %s expected fail\r\n", num_c);
    return 0;
  }

  return 1;
}

static c3_i
_test_sift_ud(void)
{
  c3_i ret_i = 1;

  ret_i &= _ud_good(0, "0");
  ret_i &= _ud_good(1, "1");
  ret_i &= _ud_good(12, "12");
  ret_i &= _ud_good(123, "123");
  ret_i &= _ud_good(1234, "1.234");
  ret_i &= _ud_good(12345, "12.345");
  ret_i &= _ud_good(123456, "123.456");
  ret_i &= _ud_good(1234567, "1.234.567");
  ret_i &= _ud_good(12345678, "12.345.678");
  ret_i &= _ud_good(123456789, "123.456.789");
  ret_i &= _ud_good(100000000, "100.000.000");
  ret_i &= _ud_good(101101101, "101.101.101");
  ret_i &= _ud_good(201201201, "201.201.201");
  ret_i &= _ud_good(302201100, "302.201.100");

  ret_i &= _ud_fail("01");
  ret_i &= _ud_fail("02");
  ret_i &= _ud_fail("003");
  ret_i &= _ud_fail("1234");
  ret_i &= _ud_fail("1234.5");
  ret_i &= _ud_fail("1234.567.8");
  ret_i &= _ud_fail("1234.56..78.");
  ret_i &= _ud_fail("123.45a");
  ret_i &= _ud_fail(".123.456");

  {
    c3_c* num_c = "4.294.967.296";
    u3_weak out = u3s_sift_ud_bytes(strlen(num_c), (c3_y*)num_c);
    u3_atom pro = u3qc_bex(32);

    if ( u3_none == out ) {
      fprintf(stderr, "sift_ud: (bex 32) fail\r\n");
      ret_i = 0;
    }

    if ( c3n == u3r_sing(pro, out) ) {
      u3m_p("out", out);
      fprintf(stderr, "sift_ud: (bex 32) wrong\r\n");
      ret_i = 0;
    }

    u3z(out); u3z(pro);
  }


  {
    c3_c* num_c = "340.282.366.920.938.463.463.374.607.431.768.211.456";
    u3_weak out = u3s_sift_ud_bytes(strlen(num_c), (c3_y*)num_c);
    u3_atom pro = u3qc_bex(128);

    if ( u3_none == out ) {
      fprintf(stderr, "sift_ud: (bex 128) fail\r\n");
      ret_i = 0;
    }

    if ( c3n == u3r_sing(pro, out) ) {
      u3m_p("out", out);
      fprintf(stderr, "sift_ud: (bex 128) wrong\r\n");
      ret_i = 0;
    }

    u3z(out); u3z(pro);
  }

  return ret_i;
}

static c3_i
_test_en_base16(void)
{
  c3_i ret_i = 1;

  {
    u3_atom dat = 0xaa;
    u3_atom pro = u3qe_en_base16(u3r_met(3, dat), dat);

    if ( c3n == u3r_sing_c("aa", pro) ) {
      fprintf(stderr, "en_base16: fail (a)\r\n");
      ret_i = 0;
    }

    u3z(pro);
  }

  {
    u3_atom dat = 0x1234;
    u3_atom pro = u3qe_en_base16(u3r_met(3, dat), dat);

    if ( c3n == u3r_sing_c("1234", pro) ) {
      fprintf(stderr, "en_base16: fail (b)\r\n");
      ret_i = 0;
    }

    u3z(pro);
  }

  {
    u3_atom dat = 0xf012;
    u3_atom pro = u3qe_en_base16(u3r_met(3, dat), dat);

    if ( c3n == u3r_sing_c("f012", pro) ) {
      fprintf(stderr, "en_base16: fail (c)\r\n");
      ret_i = 0;
    }

    u3z(pro);
  }

  {
    u3_atom dat = 0x10b;
    u3_atom pro = u3qe_en_base16(u3r_met(3, dat), dat);

    if ( c3n == u3r_sing_c("010b", pro) ) {
      fprintf(stderr, "en_base16: fail (d)\r\n");
      ret_i = 0;
    }

    u3z(pro);
  }

  {
    u3_atom pro = u3qe_en_base16(3, 0x1234);

    if ( c3n == u3r_sing_c("001234", pro) ) {
      fprintf(stderr, "en_base16: fail (e)\r\n");
      ret_i = 0;
    }

    u3z(pro);
  }

  {
    u3_atom pro = u3qe_en_base16(1, 0x1234);

    if ( c3n == u3r_sing_c("34", pro) ) {
      fprintf(stderr, "en_base16: fail (f)\r\n");
      ret_i = 0;
    }

    u3z(pro);
  }

  return ret_i;
}


static c3_i
_test_de_base16(void)
{
  c3_i ret_i = 1;

  {
    u3_noun inp = u3i_string("aa");
    u3_noun pro = u3qe_de_base16(inp);
    u3_atom len, dat;

    if ( c3n == u3r_pq(pro, u3_nul, &len, &dat) ) {
      fprintf(stderr, "de_base16: fail cell (a)\r\n");
      ret_i = 0;
    }

    if ( 1 != len ) {
      fprintf(stderr, "de_base16: fail len (a)\r\n");
      ret_i = 0;
    }

    if ( 0xaa != dat ) {
      fprintf(stderr, "de_base16: fail dat (a)\r\n");
      ret_i = 0;
    }

    u3z(inp); u3z(pro);
  }

  {
    u3_noun inp = u3i_string("1234");
    u3_noun pro = u3qe_de_base16(inp);
    u3_atom len, dat;

    if ( c3n == u3r_pq(pro, u3_nul, &len, &dat) ) {
      fprintf(stderr, "de_base16: fail cell (b)\r\n");
      ret_i = 0;
    }

    if ( 2 != len ) {
      fprintf(stderr, "de_base16: fail len (b)\r\n");
      ret_i = 0;
    }

    if ( 0x1234 != dat ) {
      fprintf(stderr, "de_base16: fail dat (b)\r\n");
      ret_i = 0;
    }

    u3z(inp); u3z(pro);
  }

  {
    u3_noun inp = u3i_string("f012");
    u3_noun pro = u3qe_de_base16(inp);
    u3_atom len, dat;

    if ( c3n == u3r_pq(pro, u3_nul, &len, &dat) ) {
      fprintf(stderr, "de_base16: fail cell (c)\r\n");
      ret_i = 0;
    }

    if ( 2 != len ) {
      fprintf(stderr, "de_base16: fail len (c)\r\n");
      ret_i = 0;
    }

    if ( 0xf012 != dat ) {
      fprintf(stderr, "de_base16: fail dat (c)\r\n");
      ret_i = 0;
    }

    u3z(inp); u3z(pro);
  }

  {
    u3_noun inp = u3i_string("010b");
    u3_noun pro = u3qe_de_base16(inp);
    u3_atom len, dat;

    if ( c3n == u3r_pq(pro, u3_nul, &len, &dat) ) {
      fprintf(stderr, "de_base16: fail cell (d)\r\n");
      ret_i = 0;
    }

    if ( 2 != len ) {
      fprintf(stderr, "de_base16: fail len (d)\r\n");
      ret_i = 0;
    }

    if ( 0x10b != dat ) {
      fprintf(stderr, "de_base16: fail dat (d)\r\n");
      ret_i = 0;
    }

    u3z(inp); u3z(pro);
  }

  {
    u3_noun inp = u3i_string("10b");
    u3_noun pro = u3qe_de_base16(inp);
    u3_atom len, dat;

    if ( c3n == u3r_pq(pro, u3_nul, &len, &dat) ) {
      fprintf(stderr, "de_base16: fail cell (e)\r\n");
      ret_i = 0;
    }

    if ( 2 != len ) {
      fprintf(stderr, "de_base16: fail len (e)\r\n");
      ret_i = 0;
    }

    if ( 0x10b != dat ) {
      fprintf(stderr, "de_base16: fail dat (e)\r\n");
      ret_i = 0;
    }

    u3z(inp); u3z(pro);
  }

  {
    u3_noun inp = u3i_string("001234");
    u3_noun pro = u3qe_de_base16(inp);
    u3_atom len, dat;

    if ( c3n == u3r_pq(pro, u3_nul, &len, &dat) ) {
      fprintf(stderr, "de_base16: fail cell (f)\r\n");
      ret_i = 0;
    }

    if ( 3 != len ) {
      fprintf(stderr, "de_base16: fail len (f)\r\n");
      ret_i = 0;
    }

    if ( 0x1234 != dat ) {
      fprintf(stderr, "de_base16: fail dat (f)\r\n");
      ret_i = 0;
    }

    u3z(inp); u3z(pro);
  }

  return ret_i;
}

static c3_i
_test_base16(void)
{
  c3_i ret_i = 1;

  ret_i &= _test_en_base16();
  ret_i &= _test_de_base16();

  return ret_i;
}

static c3_w
_fein_ob_w(c3_w inp_w)
{
  u3_atom inp = u3i_word(inp_w);
  u3_atom act = u3qe_fein_ob(inp);
  c3_w  act_w = u3r_word(0, act);
  u3z(inp); u3z(act);
  return act_w;
}

static c3_i
_expect_fein_ob_w(c3_w inp_w, c3_w exp_w)
{
  c3_w act_w = _fein_ob_w(inp_w);

  if ( act_w != exp_w ) {
    fprintf(stderr, "fein: inp=0x%08x exp=0x%08x act=0x%08x\n",
                    inp_w, exp_w, act_w);
    return 0;
  }

  return 1;
}

static c3_i
_test_fein_ob(void)
{
  c3_i ret_i = 1;

  ret_i &= _expect_fein_ob_w(0, 0);
  ret_i &= _expect_fein_ob_w(0xffff, 0xffff);
  ret_i &= _expect_fein_ob_w(0x1b08f, 0x76b920e5);
  ret_i &= _expect_fein_ob_w(0x10000, 0x423e60bf);
  ret_i &= _expect_fein_ob_w(0x10001, 0xd4400acb);
  ret_i &= _expect_fein_ob_w(0x10002, 0xf429043);
  ret_i &= _expect_fein_ob_w(0x10000000, 0xa04bc7fa);
  ret_i &= _expect_fein_ob_w(0x1234abcd, 0x686f6c25);
  ret_i &= _expect_fein_ob_w(0xabcd1234, 0x4a220c8);
  ret_i &= _expect_fein_ob_w(0xdeadbeef, 0x909bc4a9);
  ret_i &= _expect_fein_ob_w(0xfffff, 0x6746b96b);
  ret_i &= _expect_fein_ob_w(0xffffffff, 0xbba4dcce);

  return ret_i;
}

static c3_w
_fynd_ob_w(c3_w inp_w)
{
  u3_atom inp = u3i_word(inp_w);
  u3_atom act = u3qe_fynd_ob(inp);
  c3_w  act_w = u3r_word(0, act);
  u3z(inp); u3z(act);
  return act_w;
}

static c3_i
_expect_fynd_ob_w(c3_w exp_w, c3_w inp_w)
{
  c3_w act_w = _fynd_ob_w(inp_w);

  if ( act_w != exp_w ) {
    fprintf(stderr, "fynd: inp=0x%08x exp=0x%08x act=0x%08x\n",
                    inp_w, exp_w, act_w);
    return 0;
  }

  return 1;
}

static c3_i
_test_fynd_ob(void)
{
  c3_i ret_i = 1;

  ret_i &= _expect_fynd_ob_w(0, 0);
  ret_i &= _expect_fynd_ob_w(0xffff, 0xffff);
  ret_i &= _expect_fynd_ob_w(0x10000, 0x423e60bf);
  ret_i &= _expect_fynd_ob_w(0x10001, 0xd4400acb);
  ret_i &= _expect_fynd_ob_w(0x10002, 0xf429043);
  ret_i &= _expect_fynd_ob_w(0x10000000, 0xa04bc7fa);
  ret_i &= _expect_fynd_ob_w(0x1234abcd, 0x686f6c25);
  ret_i &= _expect_fynd_ob_w(0xabcd1234, 0x4a220c8);
  ret_i &= _expect_fynd_ob_w(0xdeadbeef, 0x909bc4a9);
  ret_i &= _expect_fynd_ob_w(0xfffff, 0x6746b96b);
  ret_i &= _expect_fynd_ob_w(0xffffffff, 0xbba4dcce);

  return ret_i;
}

static c3_i
_exhaust_roundtrip_fein_fynd_ob(void)
{
  c3_i ret_i = 1;
  c3_w fyn_w, i_w;

  {
    u3_atom fen, fyn;

    for ( i_w = 0x10000; i_w < 0x80000000; i_w++ ) {
      fen   = u3qe_fein_ob(i_w);
      fyn   = u3qe_fynd_ob(fen);
      fyn_w = u3r_word(0, fyn);

      if ( i_w != fyn_w ) {
        fprintf(stderr, "fein/fynd: inp=0x%08x fein=0x%08x fynd=0x%08x\n",
                        i_w, u3r_word(0, fen), fyn_w);
        ret_i = 0;
      }
      u3z(fen); u3z(fyn);

      if ( !(i_w % 0x1000000) ) {
        fprintf(stderr, "fein/fynd: 0x%x done\n", i_w);
      }
    }
  }

  {
    c3_w fen_w;

    do {
      fen_w = _fein_ob_w(i_w);
      fyn_w = _fynd_ob_w(fen_w);
      if ( i_w != fyn_w ) {
        fprintf(stderr, "fein/fynd: inp=0x%08x fein=0x%08x fynd=0x%08x\n",
                        i_w, fen_w, fyn_w);
        ret_i = 0;
      }

      if ( !(i_w % 0x1000000) ) {
        fprintf(stderr, "fein/fynd: 0x%x done\n", i_w);
      }
    }
    while ( ++i_w );
  }

  return ret_i;
}

static c3_i
_test_ob(void)
{
  c3_i ret_i = 1;
  ret_i &= _test_fein_ob();
  ret_i &= _test_fynd_ob();
  //  disabled, takes almost ~m15
  //
  // ret_i &= _exhaust_roundtrip_fein_fynd_ob();
  return ret_i;
}

static c3_i
_test_jets(void)
{
  c3_i ret_i = 1;

  if ( !_test_sift_ud() ) {
    fprintf(stderr, "test jets: sift_ud: failed\r\n");
    ret_i = 0;
  }

  if ( !_test_base16() ) {
    fprintf(stderr, "test jets: base16: failed\r\n");
    ret_i = 0;
  }

  if ( !_test_ob() ) {
    fprintf(stderr, "test jets: ob: failed\r\n");
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

  if ( !_test_jets() ) {
    fprintf(stderr, "test jets: failed\r\n");
    exit(1);
  }

  //  GC
  //
  u3m_grab(u3_none);

  fprintf(stderr, "test jets: ok\r\n");
  return 0;
}
