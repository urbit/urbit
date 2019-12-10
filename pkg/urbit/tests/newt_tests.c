#include "all.h"
#include "vere/vere.h"

/* _setup(): prepare for tests.
*/
static void
_setup(void)
{
  u3m_init();
  u3m_pave(c3y, c3n);
}

static c3_w pok_w;
static c3_w bal_w;

static void
_moat_poke_cb(void* vod_p, u3_atom a)
{
  pok_w++;
  u3z(a);
}

static void
_moat_bail_cb(void* vod_p, const c3_c* err_c)
{
  bal_w++;
}

/* _test_newt_smol(): various scenarios with small messages
*/
static void
_test_newt_smol(void)
{
  //  =(2 (jam 0))
  //
  u3_atom     a = u3ke_jam(0);
  u3_moat mot_u;
  c3_w    len_w;
  c3_y*   buf_y;

  memset(&mot_u, 0, sizeof(u3_moat));
  mot_u.pok_f = _moat_poke_cb;
  mot_u.bal_f = _moat_bail_cb;

  //  one message one buffer
  //
  {
    pok_w = 0;
    bal_w = 0;

    buf_y = u3_newt_encode(u3k(a), &len_w);
    u3_newt_decode(&mot_u, buf_y, len_w);

    if ( 1 != pok_w ) {
      fprintf(stderr, "newt smol fail (a)\n");
      exit(1);
    }
  }

  //  two messages one buffer
  //
  {
    pok_w = 0;
    bal_w = 0;

    buf_y = u3_newt_encode(u3k(a), &len_w);

    buf_y = c3_realloc(buf_y, 2 * len_w);
    memcpy(buf_y + len_w, buf_y, len_w);
    len_w = 2 * len_w;

    u3_newt_decode(&mot_u, buf_y, len_w);

    if ( 2 != pok_w ) {
      fprintf(stderr, "newt smol fail (b)\n");
      exit(1);
    }
  }

  //  one message two buffers
  //
  {
    c3_y* end_y;
    pok_w = 0;
    bal_w = 0;

    buf_y = u3_newt_encode(u3k(a), &len_w);

    end_y = c3_malloc(1);
    end_y[0] = buf_y[len_w - 1];

    u3_newt_decode(&mot_u, buf_y, len_w - 1);

    if ( 0 != pok_w ) {
      fprintf(stderr, "newt smol fail (c)\n");
      exit(1);
    }

    u3_newt_decode(&mot_u, end_y, 1);

    if ( 1 != pok_w ) {
      fprintf(stderr, "newt smol fail (d)\n");
      exit(1);
    }
  }

  //  two messages two buffers (overlapping length)
  //
  {
    c3_y* haf_y;
    c3_w  haf_w, dub_w;

    pok_w = 0;
    bal_w = 0;

    buf_y = u3_newt_encode(u3k(a), &len_w);

    dub_w = 2 * len_w;
    haf_w = len_w / 2;

    //  buf_y is all of message one, half of message two (not a full length)
    //
    buf_y = c3_realloc(buf_y, dub_w - haf_w);
    memcpy(buf_y + len_w, buf_y, len_w - haf_w);

    //  haf_y is the second half of message two
    //
    haf_y = c3_malloc(haf_w);
    memcpy(haf_y, buf_y + (len_w - haf_w), haf_w);

    u3_newt_decode(&mot_u, buf_y, dub_w - haf_w);

    if ( 1 != pok_w ) {
      fprintf(stderr, "newt smol fail (e)\n");
      exit(1);
    }

    u3_newt_decode(&mot_u, haf_y, haf_w);

    if ( 2 != pok_w ) {
      fprintf(stderr, "newt smol fail (f)\n");
      exit(1);
    }
  }

  u3z(a);
}

/* _test_newt_vast(): various scenarios with larger messages
*/
static void
_test_newt_vast(void)
{
  //  =(53 (met 3 (jam "abcdefghijklmnopqrstuvwxyz")))
  //
  u3_atom     a = u3ke_jam(u3i_tape("abcdefghijklmnopqrstuvwxyz"));
  u3_moat mot_u;
  c3_w    len_w;
  c3_y*   buf_y;

  memset(&mot_u, 0, sizeof(u3_moat));
  mot_u.pok_f = _moat_poke_cb;
  mot_u.bal_f = _moat_bail_cb;

  //  one message one buffer
  //
  {
    pok_w = 0;
    bal_w = 0;

    buf_y = u3_newt_encode(u3k(a), &len_w);
    u3_newt_decode(&mot_u, buf_y, len_w);

    if ( 1 != pok_w ) {
      fprintf(stderr, "newt vast fail (a)\n");
      exit(1);
    }
  }

  //  two messages one buffer
  //
  {
    pok_w = 0;
    bal_w = 0;

    buf_y = u3_newt_encode(u3k(a), &len_w);

    buf_y = c3_realloc(buf_y, 2 * len_w);
    memcpy(buf_y + len_w, buf_y, len_w);
    len_w = 2 * len_w;

    u3_newt_decode(&mot_u, buf_y, len_w);

    if ( 2 != pok_w ) {
      fprintf(stderr, "newt vast fail (b)\n");
      exit(1);
    }
  }

  //  one message many buffers
  //
  {
    pok_w = 0;
    bal_w = 0;

    buf_y = u3_newt_encode(u3k(a), &len_w);

    {
      c3_y* cop_y = c3_malloc(len_w);
      c3_w  haf_w = len_w / 2;
      memcpy(cop_y, buf_y, len_w);

      u3_newt_decode(&mot_u, buf_y, haf_w);

      while ( haf_w < len_w ) {
        c3_y* end_y = c3_malloc(1);
        end_y[0] = cop_y[haf_w];

        if ( 0 != pok_w ) {
          fprintf(stderr, "newt vast fail (c) %u\n", haf_w);
          exit(1);
        }

        u3_newt_decode(&mot_u, end_y, 1);
        haf_w++;
      }

      c3_free(cop_y);
    }

    if ( 1 != pok_w ) {
      fprintf(stderr, "newt vast fail (d)\n");
      exit(1);
    }
  }

  //  two messages two buffers
  //
  {
    c3_y* haf_y;
    c3_w  haf_w, dub_w;

    pok_w = 0;
    bal_w = 0;

    buf_y = u3_newt_encode(u3k(a), &len_w);

    dub_w = 2 * len_w;
    haf_w = len_w / 2;

    //  buf_y is all of message one, half of message two
    //
    buf_y = c3_realloc(buf_y, dub_w - haf_w);
    memcpy(buf_y + len_w, buf_y, len_w - haf_w);

    //  haf_y is the second half of message two
    //
    haf_y = c3_malloc(haf_w);
    memcpy(haf_y, buf_y + (len_w - haf_w), haf_w);

    u3_newt_decode(&mot_u, buf_y, dub_w - haf_w);

    if ( 1 != pok_w ) {
      fprintf(stderr, "newt vast fail (e)\n");
      exit(1);
    }

    u3_newt_decode(&mot_u, haf_y, haf_w);

    if ( 2 != pok_w ) {
      fprintf(stderr, "newt vast fail (f)\n");
      exit(1);
    }
  }

  //  two messages many buffers
  //
  {
    c3_w dub_w;

    pok_w = 0;
    bal_w = 0;

    buf_y = u3_newt_encode(u3k(a), &len_w);

    dub_w = 2 * len_w;

    //  buf_y is two copies of message
    //
    buf_y = c3_realloc(buf_y, dub_w);
    memcpy(buf_y + len_w, buf_y, len_w);

    {
      c3_y* cop_y = c3_malloc(dub_w);
      c3_w  haf_w = len_w + 1;
      memcpy(cop_y, buf_y, dub_w);

      u3_newt_decode(&mot_u, buf_y, haf_w);

      while ( haf_w < dub_w ) {
        c3_y* end_y = c3_malloc(1);
        end_y[0] = cop_y[haf_w];

        if ( 1 != pok_w ) {
          fprintf(stderr, "newt vast fail (g) %u\n", haf_w);
          exit(1);
        }

        u3_newt_decode(&mot_u, end_y, 1);
        haf_w++;
      }

      c3_free(cop_y);
    }

    if ( 2 != pok_w ) {
      fprintf(stderr, "newt vast fail (h)\n");
      exit(1);
    }
  }

  u3z(a);
}

/* main(): run all test cases.
*/
int
main(int argc, char* argv[])
{
  _setup();

  _test_newt_smol();
  _test_newt_vast();

  fprintf(stderr, "test_newt: ok\n");

  return 0;
}
