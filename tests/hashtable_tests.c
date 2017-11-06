#include "all.h"

static void _setup(void);
static void _test_cache_trimming(void);
static void _test_no_cache(void);

/* main(): run all test cases.
*/
int
main(int argc, char* argv[])
{
  _setup();

  _test_no_cache();
  _test_cache_trimming();

  return 0;
}

/* _setup(): prepare for tests.
*/
static void
_setup(void)
{
  u3m_init(c3y);
  u3m_pave(c3y, c3n);
}

/* _test_no_cache(): test a hashtable without caching.
*/
static void
_test_no_cache(void)
{
  c3_w i_w;
  c3_w max_w = 1000;

  u3p(u3h_root) har_p = u3h_new();

  for ( i_w = 0; i_w < max_w; i_w++ ) {
    u3h_put(har_p, i_w, i_w + max_w);
  }

  for ( i_w = 0; i_w < max_w; i_w++ ) {
    c3_assert(i_w + max_w == u3h_get(har_p, i_w));
  }
  printf("test_no_cache: ok\n");
}

/* _test_cache_trimming(): ensure a caching hashtable removes stale items.
*/
static void
_test_cache_trimming(void)
{
  c3_w max_w = 10000;
  c3_w i_w, hit_w = 0, mis_w = 0;

  //u3p(u3h_root) har_p = u3h_new_cache(max_w / 2);
  u3p(u3h_root) har_p = u3h_new_cache(max_w / 10 );
  u3h_root*     har_u = u3to(u3h_root, har_p);

  for ( i_w = 0; i_w < max_w; i_w++ ) {
    u3h_put(har_p, i_w, i_w + max_w);
  }

  c3_assert( ( max_w + max_w - 1) == u3h_get(har_p, max_w - 1) );
  c3_assert( ( max_w / 10 ) == har_u->use_w );
  printf("test_cache_trimming: ok\n");
}
