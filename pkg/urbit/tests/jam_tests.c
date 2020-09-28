#include "all.h"
#include "ur/ur.h"

/* _setup(): prepare for tests.
*/
static void
_setup(void)
{
  u3m_init();
  u3m_pave(c3y, c3n);
}

/* _test_jam_spot_a(): spot check jam/cue
*/
static c3_i
_test_jam_spot_a(void)
{
  c3_i ret_i = 1;

  if ( 0xc != u3qe_jam(1) ) {
    fprintf(stderr, "jam: fail (a)\r\n");
    ret_i = 0;
  }

  if ( 1 != u3ke_cue(u3qe_jam(1)) ) {
    fprintf(stderr, "jam: fail (b)\r\n");
    ret_i = 0;
  }

  {
    u3_noun a = u3nc(1, 2);

    if ( 0x1231 != u3qe_jam(a) ) {
      fprintf(stderr, "jam: fail (c)\r\n");
      ret_i = 0;
    }

    if ( c3y != u3r_sing(a, u3ke_cue(u3qe_jam(a))) ) {
      fprintf(stderr, "jam: fail (d)\r\n");
      ret_i = 0;
    }
  }

  {
    u3_noun a = u3nt(1, 2, 3);

    if ( 0x344871 != u3qe_jam(a) ) {
      fprintf(stderr, "jam: fail (e)\r\n");
      ret_i = 0;
    }

    if ( c3y != u3r_sing(a, u3ke_cue(u3qe_jam(a))) ) {
      fprintf(stderr, "jam: fail (f)\r\n");
      ret_i = 0;
    }
  }

  {
    u3_noun a = u3nc(u3nc(1, 2), 3);

    if ( 0x3448c5 != u3qe_jam(a) ) {
      fprintf(stderr, "jam: fail (g)\r\n");
      ret_i = 0;
    }

    if ( c3y != u3r_sing(a, u3ke_cue(u3qe_jam(a))) ) {
      fprintf(stderr, "jam: fail (h)\r\n");
      ret_i = 0;
    }
  }

  {
    u3_noun b = u3nc(1, 2);
    u3_noun a = u3nt(b, b, b);

    if ( c3y != u3r_sing(a, u3ke_cue(u3qe_jam(a))) ) {
      fprintf(stderr, "jam: fail (j)\r\n");
      ret_i = 0;
    }
  }

  {
    u3_noun b = u3i_string("abcdefjhijklmnopqrstuvwxyz");
    u3_noun a = u3nq(b, 2, 3, b);

    if ( c3y != u3r_sing(a, u3ke_cue(u3qe_jam(a))) ) {
      fprintf(stderr, "jam: fail (k)\r\n");
      ret_i = 0;
    }
  }

  {
    u3_noun a = u3nc(u3nc(u3nc(1, u3nc(u3nc(2, u3nc(u3nc(3, u3nc(u3nc(4, u3nc(u3nt(5, 6, u3nc(7, u3nc(u3nc(8, 0), 0))), 0)), 0)), 0)), 0)), 0), 0);

    if ( c3y != u3r_sing(a, u3ke_cue(u3qe_jam(a))) ) {
      fprintf(stderr, "jam: fail (l)\r\n");
      ret_i = 0;
    }
  }

  return ret_i;
}

/* _test_jam_spot_b(): more jam/cue spot-checking, ported from the 64-bit effort
*/
static c3_i
_test_jam_spot_b()
{
  c3_i ret_i = 1;

  // the boot msg from the worker
  {
    u3_noun dat = u3_nul;
    u3_noun in_1 = u3nc(c3__play, dat);
    u3_atom jam_1 = u3ke_jam(in_1);

    u3_noun out_1 = u3ke_cue(jam_1);
    u3_noun head_out =    u3h(out_1);
    u3_noun tail_out =    u3t(out_1);

    if (c3__play != head_out){
      fprintf(stderr, "*** cue_jam 0 out head\r\n");
      ret_i = 0;
    }

    if (u3_nul != tail_out){
      fprintf(stderr, "*** cue_jam 0 out tail\r\n");
      ret_i = 0;
    }
  }

  // the boot msg from the worker, again,
  // but this time torn apart into bytes and rebuilt
  {
    u3_noun dat = u3_nul;
    u3_noun in_1 = u3nc(c3__play, dat);
    u3_atom jam_1 = u3ke_jam(in_1);

    c3_y buf_y[1024];
    memset(buf_y, 0, 1024);
    c3_w len_w = u3r_met(3, jam_1);

    u3r_bytes(0,       // start byte
              len_w,   // len
              buf_y,   // buffer
              jam_1 ); // input noun

    /// zip ....zap ... communicate between serf and king

    u3_noun jam_2 = u3i_bytes(len_w, buf_y);

    if ( c3n == u3r_sing(jam_1, jam_2) ) {
      fprintf(stderr, "*** error in 6 byte message\r\n");
      ret_i = 0;
    }

    u3_noun out_1 = u3ke_cue(jam_2);

    u3_noun head_out =    u3h(out_1);
    u3_noun tail_out =    u3t(out_1);

    if (c3__play != head_out){
      fprintf(stderr, "*** cue_jam 0 out head\r\n");
      ret_i = 0;
    }

    if (u3_nul != tail_out){
      fprintf(stderr, "*** cue_jam 0 out tail\r\n");
      ret_i = 0;
    }
  }

  // 1
  {

    u3_atom in_1 = 1;
    u3_atom jam_1 = u3ke_jam(in_1);

    if (12 != jam_1){
      fprintf(stderr, "*** cue_jam 1a\r\n");
      ret_i = 0;
    }

    u3_noun out_1 = u3ke_cue(jam_1);

    if (1 != out_1){
      fprintf(stderr, "*** cue_jam 1b\r\n");
      ret_i = 0;
    }
  }

  // [ 1 1 ]
  {

    u3_noun in_1 = u3i_cell(1, 1);
    u3_atom jam_1 = u3ke_jam(in_1);

    if (817 != jam_1){
      fprintf(stderr, "*** cue_jam 2 in\r\n");
      ret_i = 0;
    }

    u3_noun out_1 = u3ke_cue(jam_1);


    u3_noun head_out =    u3h(out_1);
    u3_noun tail_out =    u3t(out_1);

    if (1 != head_out){
      fprintf(stderr, "*** cue_jam 2 out head\r\n");
      ret_i = 0;
    }

    if (1 != tail_out){
      fprintf(stderr, "*** cue_jam 2 out tail\r\n");
      ret_i = 0;
    }
  }

  // [ 1 2 ]
  {

    u3_noun in_1 = u3i_cell(1, 2);
    u3_atom jam_1 = u3ke_jam(in_1);

    if (4657 != jam_1){
      fprintf(stderr, "*** cue_jam 2 in\r\n");
      ret_i = 0;
    }

    u3_noun out_1 = u3ke_cue(jam_1);

    u3_noun head_out =    u3h(out_1);
    u3_noun tail_out =    u3t(out_1);

    if (1 != head_out){
      fprintf(stderr, "*** cue_jam 2 out head\r\n");
      ret_i = 0;
    }

    if (2 != tail_out){
      fprintf(stderr, "*** cue_jam 2 out tail\r\n");
      ret_i = 0;
    }
  }

  // medium complicated cell
  //    q
  //   / \
  //  a1  r
  //     / \
  //    b2  s
  //       / \
  //      c3  d4
  {
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = (u3_noun) 0x3;
    u3_noun d = (u3_noun) 0x4;

    u3_noun s = u3i_cell(c, d);
    u3_noun r = u3i_cell(b, s);
    u3_noun q = u3i_cell(a, r);

    u3_atom jam_1 = u3ke_jam(q);
    u3_noun out_1 = u3ke_cue(jam_1);

    u3_noun a2 = u3h(out_1);
    u3_noun r2 = u3t(out_1);
    if (a2 != a){
      fprintf(stderr, "*** _cue_jam: complicated a\r\n");
      ret_i = 0;
    }

    u3_noun b2 = u3h(r2);
    u3_noun s2 = u3t(r2);
    if (b2 != b){
      fprintf(stderr, "*** _cue_jam: complicated b\r\n");
      ret_i = 0;
    }

    u3_noun c2 = u3h(s2);
    u3_noun d2 = u3t(s2);
    if (c2 != c){
      fprintf(stderr, "*** _cue_jam: complicated c\r\n");
      ret_i = 0;
    }

    if (d2 != d){
      fprintf(stderr, "*** _cue_jam: complicated d\r\n");
      ret_i = 0;
    }
  }

  return 1;
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

    out_d = sab_u.byt_d;
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
    ur_dict32_t dic_u = {0};
    u3_noun       out;

    ur_dict32_grow((ur_root_t*)0, &dic_u, ur_fib10, ur_fib11);

    if ( c3n == u3s_cue_xeno_unsafe(&dic_u, len_w, byt_y, &out) ) {
      fprintf(stderr, "\033[31mcue %s fail 1\033[0m\r\n", cap_c);
      ret_i = 0;
    }
    else if ( c3n == u3r_sing(ref, out) ) {
      fprintf(stderr, "\033[31mcue %s fail 2\033[0m\r\n", cap_c);
      u3m_p("ref", ref);
      u3m_p("out", out);
      ret_i = 0;
    }

    u3z(out);
    ur_dict_free((ur_dict_t*)&dic_u);
  }

  {
    u3_noun pro = u3m_soft(0, u3s_cue_atom, u3i_bytes(len_w, byt_y));
    u3_noun tag, out;

    u3x_cell(pro, &tag, &out);

    if ( u3_blip != tag ) {
      fprintf(stderr, "\033[31mcue %s fail 3\033[0m\r\n", cap_c);
      ret_i = 0;
    }
    else if ( c3n == u3r_sing(ref, out) ) {
      fprintf(stderr, "\033[31mcue %s fail 4\033[0m\r\n", cap_c);
      u3m_p("ref", ref);
      u3m_p("out", out);
      ret_i = 0;
    }

    u3z(pro);
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
    c3_y res_y[2] = { 0x21, 0xd1 };
    TEST_CASE("[2 3]", u3nc(2, 3));
  }

  {
    c3_y res_y[11] = { 0x1, 0xdf, 0x2c, 0x6c, 0x8e, 0xe, 0x7c, 0xb3, 0x3a, 0x36, 0x36 };
    TEST_CASE("[%fast %full]", u3nc(c3__fast, c3__full));
  }

  {
    c3_y res_y[2] = {  0x71, 0xcc };
    TEST_CASE("[1 1 1]", u3nc(1, u3nc(1, 1)));
  }

  {
    c3_y res_y[12] = { 0x1, 0xdf, 0x2c, 0x6c, 0x8e, 0x1e, 0xf0, 0xcd, 0xea, 0xd8, 0xd8, 0x93 };
    TEST_CASE("[%fast %full %fast]", u3nc(c3__fast, u3nc(c3__full, c3__fast)));
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

  return ret_i;
}

static c3_i
_test_jam(void)
{
  c3_i ret_i = 1;

  if ( !_test_jam_spot_a() ) {
    fprintf(stderr, "test jam: spot a: failed\r\n");
    ret_i = 0;
  }

  if ( !_test_jam_spot_b() ) {
    fprintf(stderr, "test jam: spot b: failed\r\n");
    ret_i = 0;
  }

  if ( !_test_jam_roundtrip() ) {
    fprintf(stderr, "test jam: roundtrip: failed\r\n");
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

  if ( !_test_jam() ) {
    fprintf(stderr, "test jam: failed\r\n");
    exit(1);
  }

  fprintf(stderr, "test jam: ok\r\n");
  return 0;
}
