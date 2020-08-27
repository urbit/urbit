#include "all.h"

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
