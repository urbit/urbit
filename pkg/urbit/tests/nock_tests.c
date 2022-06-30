#include "all.h"

/* _setup(): prepare for tests.
*/
static void
_setup(void)
{
  u3m_init();
  u3m_pave(c3y);
}

static u3_noun
_nock_fol(u3_noun fol)
{
  return u3n_nock_on(u3_nul, fol);
}

static c3_i
_test_nock_meme(void)
{
  //  (jam !=(=(~ =|(i=@ |-(?:(=(i ^~((bex 32))) ~ [i $(i +(i))]))))))
  //
  const c3_y buf_y[] = {
    0xe1, 0x16, 0x1b,  0x4, 0x1b, 0xe1, 0x20, 0x58, 0x1c, 0x76, 0x4d, 0x96, 0xd8,
    0x31, 0x60,  0x0,  0x0,  0x0,  0x0, 0xd8,  0x8, 0x37, 0xce,  0xd, 0x92, 0x21,
    0x83, 0x68, 0x61, 0x87, 0x39, 0xce, 0x4d,  0xe, 0x92, 0x21, 0x87, 0x19,  0x8
  };
  u3_noun fol = u3s_cue_bytes(sizeof(buf_y), buf_y);
  u3_noun gon;
  c3_w    i_w;
  c3_i  ret_i = 1;

  for ( i_w = 0; i_w < 3; i_w++ ) {
    gon = u3m_soft(0, _nock_fol, u3k(fol));

    if ( c3n == u3r_p(gon, c3__meme, 0) ) {
      u3m_p("nock meme unexpected mote", u3h(gon));
      ret_i = 0;
      u3z(gon);
      break;
    }

    u3z(gon);
  }

  u3z(fol);

  return ret_i;
}

static c3_i
_test_nock(void)
{
  c3_i ret_i = 1;

  if ( !_test_nock_meme() ) {
    fprintf(stderr, "test nock meme: failed\r\n");
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

  if ( !_test_nock() ) {
    fprintf(stderr, "test nock: failed\r\n");
    exit(1);
  }

  //  GC
  //
  u3m_grab(u3_none);

  fprintf(stderr, "test nock: ok\r\n");
  return 0;
}
