#include "all.h"

/* _setup(): prepare for tests.
*/
static void
_setup(void)
{
  u3m_init();
  u3m_pave(c3y, c3n);
}

/* _test_jam(): spot check jam/cue
*/
static void
_test_jam(void)
{
  if ( 0xc != u3qe_jam(1) ) {
    fprintf(stderr, "jam: fail (a)\r\n");
    exit(1);
  }

  if ( 1 != u3ke_cue(u3qe_jam(1)) ) {
    fprintf(stderr, "jam: fail (b)\r\n");
    exit(1);
  }

  {
    u3_noun a = u3nc(1, 2);

    if ( 0x1231 != u3qe_jam(a) ) {
      fprintf(stderr, "jam: fail (c)\r\n");
      exit(1);
    }

    if ( c3y != u3r_sing(a, u3ke_cue(u3qe_jam(a))) ) {
      fprintf(stderr, "jam: fail (d)\r\n");
      exit(1);
    }
  }

  {
    u3_noun a = u3nt(1, 2, 3);

    if ( 0x344871 != u3qe_jam(a) ) {
      fprintf(stderr, "jam: fail (e)\r\n");
      exit(1);
    }

    if ( c3y != u3r_sing(a, u3ke_cue(u3qe_jam(a))) ) {
      fprintf(stderr, "jam: fail (f)\r\n");
      exit(1);
    }
  }

  {
    u3_noun a = u3nc(u3nc(1, 2), 3);

    // fprintf(stderr, "%x\n", u3qe_jam(a));

    if ( 0x3448c5 != u3qe_jam(a) ) {
      fprintf(stderr, "jam: fail (g)\r\n");
      exit(1);
    }

    if ( c3y != u3r_sing(a, u3ke_cue(u3qe_jam(a))) ) {
      fprintf(stderr, "jam: fail (h)\r\n");
      exit(1);
    }
  }

  {
    u3_noun b = u3nc(1, 2);
    u3_noun a = u3nt(b, b, b);

    if ( c3y != u3r_sing(a, u3ke_cue(u3qe_jam(a))) ) {
      fprintf(stderr, "jam: fail (j)\r\n");
      exit(1);
    }
  }

  {
    u3_noun b = u3i_string("abcdefjhijklmnopqrstuvwxyz");
    u3_noun a = u3nq(b, 2, 3, b);

    if ( c3y != u3r_sing(a, u3ke_cue(u3qe_jam(a))) ) {
      fprintf(stderr, "jam: fail (k)\r\n");
      exit(1);
    }
  }

  {
    u3_noun a = u3nc(u3nc(u3nc(1, u3nc(u3nc(2, u3nc(u3nc(3, u3nc(u3nc(4, u3nc(u3nt(5, 6, u3nc(7, u3nc(u3nc(8, 0), 0))), 0)), 0)), 0)), 0)), 0), 0);

    if ( c3y != u3r_sing(a, u3ke_cue(u3qe_jam(a))) ) {
      fprintf(stderr, "jam: fail (l)\r\n");
      exit(1);
    }
  }

  fprintf(stderr, "test_jam: ok\n");
}

/* main(): run all test cases.
*/
int
main(int argc, char* argv[])
{
  _setup();

  _test_jam();

  return 0;
}
