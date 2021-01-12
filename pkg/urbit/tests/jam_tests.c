#include "all.h"
#include "ur/ur.h"

/* _setup(): prepare for tests.
*/
static void
_setup(void)
{
  u3m_init();
  u3m_pave(c3y);
}

static void
_byte_print(c3_d        out_d,
            c3_y*       out_y,
            c3_w        len_w,
            const c3_y* byt_y)
{
  c3_d i_d;

  fprintf(stderr, "  actual: { ");
  for ( i_d = 0; i_d < out_d; i_d++ ) {
    fprintf(stderr, "0x%x, ", out_y[i_d]);
  }
  fprintf(stderr, "}\r\n");
  fprintf(stderr, "  expect: { ");
  for ( i_d = 0; i_d < len_w; i_d++ ) {
    fprintf(stderr, "0x%x, ", byt_y[i_d]);
  }
  fprintf(stderr, "}\r\n");
}

static c3_i
_test_jam_spec(const c3_c* cap_c,
               u3_noun       ref,
               c3_w        len_w,
               const c3_y* byt_y)
{
  c3_i  ret_i = 1;
  c3_d  out_d;
  c3_y* out_y;

  {
    u3s_jam_xeno(ref, &out_d, &out_y);

    if ( 0 != memcmp(out_y, byt_y, len_w) ) {
      fprintf(stderr, "\033[31mjam xeno %s fail\033[0m\r\n", cap_c);
      _byte_print(out_d, out_y, len_w, byt_y);
      ret_i = 0;
    }

    free(out_y);
  }

  {
    u3i_slab sab_u;
    c3_w     bit_w = u3s_jam_fib(&sab_u, ref);

    out_d = ((c3_d)bit_w + 0x7) >> 3;
    //  XX assumes little-endian
    //
    out_y = sab_u.buf_y;

    if ( 0 != memcmp(out_y, byt_y, len_w) ) {
      fprintf(stderr, "\033[31mjam fib %s fail\033[0m\r\n", cap_c);
      _byte_print(out_d, out_y, len_w, byt_y);
      ret_i = 0;
    }

    u3i_slab_free(&sab_u);
  }

  return ret_i;
}

static c3_i
_test_cue_spec(const c3_c* cap_c,
               u3_noun       ref,
               c3_w        len_w,
               const c3_y* byt_y)
{
  c3_i ret_i = 1;

  {
    u3_noun pro = u3m_soft(0, u3s_cue_atom, u3i_bytes(len_w, byt_y));
    u3_noun tag, out;

    u3x_cell(pro, &tag, &out);

    if ( u3_blip != tag ) {
      fprintf(stderr, "\033[31mcue %s fail 1\033[0m\r\n", cap_c);
      ret_i = 0;
    }
    else if ( c3n == u3r_sing(ref, out) ) {
      fprintf(stderr, "\033[31mcue %s fail 2\033[0m\r\n", cap_c);
      u3m_p("ref", ref);
      u3m_p("out", out);
      ret_i = 0;
    }

    u3z(pro);
  }

  {
    u3_noun out;

    if ( u3_none == (out = u3s_cue_xeno(len_w, byt_y)) ) {
      fprintf(stderr, "\033[31mcue %s fail 3\033[0m\r\n", cap_c);
      ret_i = 0;
    }
    else if ( c3n == u3r_sing(ref, out) ) {
      fprintf(stderr, "\033[31mcue %s fail 4\033[0m\r\n", cap_c);
      u3m_p("ref", ref);
      u3m_p("out", out);
      ret_i = 0;
    }

    u3z(out);
  }

  return ret_i;
}

static c3_i
_test_jam_roundtrip(void)
{
  c3_i ret_i = 1;

#     define TEST_CASE(a, b)                                        \
        const c3_c* cap_c = a;                                      \
        u3_noun       ref = b;                                      \
        ret_i &= _test_jam_spec(cap_c, ref, sizeof(res_y), res_y);  \
        ret_i &= _test_cue_spec(cap_c, ref, sizeof(res_y), res_y);  \
        u3z(ref);

  {
    c3_y res_y[1] = { 0x2 };
    TEST_CASE("0", 0);
  }

  {
    c3_y res_y[1] = { 0xc };
    TEST_CASE("1", 1);
  }

  {
    c3_y res_y[1] = { 0x48 };
    TEST_CASE("2", 2);
  }

  {
    c3_y res_y[6] = { 0xc0, 0x37, 0xb, 0x9b, 0xa3, 0x3 };
    TEST_CASE("%fast", c3__fast);
  }

  {
    c3_y res_y[6] = { 0xc0, 0x37, 0xab, 0x63, 0x63, 0x3 };
    TEST_CASE("%full", c3__full);
  }

  {
    c3_y res_y[1] = { 0x29 };
    TEST_CASE("[0 0]", u3nc(0, 0));
  }

  {
    c3_y res_y[2] = { 0x31, 0x3 };
    TEST_CASE("[1 1]", u3nc(1, 1));
  }

  {
    c3_y res_y[2] = { 0x31, 0x12 };
    TEST_CASE("[1 2]", u3nc(1, 2));
  }

  {
    c3_y res_y[2] = { 0x21, 0xd1 };
    TEST_CASE("[2 3]", u3nc(2, 3));
  }

  {
    c3_y res_y[11] = { 0x1, 0xdf, 0x2c, 0x6c, 0x8e, 0xe, 0x7c, 0xb3, 0x3a, 0x36, 0x36 };
    TEST_CASE("[%fast %full]", u3nc(c3__fast, c3__full));
  }

  {
    c3_y res_y[2] = { 0x71, 0xcc };
    TEST_CASE("[1 1 1]", u3nc(1, u3nc(1, 1)));
  }

  {
    c3_y res_y[3] = { 0x71, 0x48, 0x34 };
    TEST_CASE("[1 2 3]", u3nt(1, 2, 3));
  }

  {
    c3_y res_y[12] = { 0x1, 0xdf, 0x2c, 0x6c, 0x8e, 0x1e, 0xf0, 0xcd, 0xea, 0xd8, 0xd8, 0x93 };
    TEST_CASE("[%fast %full %fast]", u3nc(c3__fast, u3nc(c3__full, c3__fast)));
  }

  {
    c3_y res_y[3] = { 0xc5, 0x48, 0x34 };
    TEST_CASE("[[1 2] 3]", u3nc(u3nc(1, 2), 3));
  }

  {
    c3_y res_y[5] = { 0xc5, 0xc8, 0x26, 0x27, 0x1 };
    TEST_CASE("[[1 2] [1 2] 1 2]", u3nt(u3nc(1, 2), u3nc(1, 2), u3nc(1, 2)));
  }

  {
    c3_y res_y[6] = { 0xa5, 0x35, 0x19, 0xf3, 0x18, 0x5 };
    TEST_CASE("[[0 0] [[0 0] 1 1] 1 1]", u3nc(u3nc(0, 0), u3nc(u3nc(u3nc(0, 0), u3nc(1, 1)), u3nc(1, 1))));
  }

  {
    c3_y res_y[14] = { 0x15, 0x17, 0xb2, 0xd0, 0x85, 0x59, 0xb8, 0x61, 0x87, 0x5f, 0x10, 0x54, 0x55, 0x5 };
    TEST_CASE("deep", u3nc(u3nc(u3nc(1, u3nc(u3nc(2, u3nc(u3nc(3, u3nc(u3nc(4, u3nc(u3nt(5, 6, u3nc(7, u3nc(u3nc(8, 0), 0))), 0)), 0)), 0)), 0)), 0), 0));
  }

  {
    c3_y inp_y[33] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 };
    c3_y res_y[35] = { 0, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8 };
    TEST_CASE("wide", u3i_bytes(sizeof(inp_y), inp_y));
  }

  {
    c3_y inp_y[16] = { 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0xc, 0xa8, 0xab, 0x60, 0xef, 0x2d, 0xd, 0x0, 0x0, 0x80 };
    c3_y res_y[19] = { 0x0, 0x2, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x18, 0x50, 0x57, 0xc1, 0xde, 0x5b, 0x1a, 0x0, 0x0, 0x0, 0x1 };
    TEST_CASE("date", u3i_bytes(sizeof(inp_y), inp_y));
  }

  {
    u3_noun a = u3i_string("abcdefjhijklmnopqrstuvwxyz");
    c3_y res_y[32] = {
       0x1, 0xf8,  0xc, 0x13, 0x1b, 0x23, 0x2b, 0x33, 0x53, 0x43, 0x4b,
      0x53, 0x5b, 0x63, 0x6b, 0x73, 0x7b, 0x83, 0x8b, 0x93, 0x9b, 0xa3,
      0xab, 0xb3, 0xbb, 0xc3, 0xcb, 0xd3, 0x87,  0xc, 0x3d,  0x9
    };
    TEST_CASE("alpha", u3nq(u3k(a), 2, 3, a));
  }

  return ret_i;
}

/* main(): run all test cases.
*/
int
main(int argc, char* argv[])
{
  _setup();

  if ( !_test_jam_roundtrip() ) {
    fprintf(stderr, "test jam: failed\r\n");
    exit(1);
  }

  //  GC
  //
  u3m_grab(u3_none);

  fprintf(stderr, "test jam: ok\r\n");
  return 0;
}
