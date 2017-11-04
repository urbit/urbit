#include "all.h"

static void _ch_setup(void);
static void _ch_test_cache_trimming(void);
static void _ch_test_no_cache(void);

/* main(): run all test cases.
*/
int
main(int argc, char* argv[])
{
  _ch_setup();

  //_ch_test_no_cache();
  _ch_test_cache_trimming();

  return 0;
}

/* _ch_setup(): prepare for tests.
*/
static void
_ch_setup(void)
{
  u3m_init(c3y);
  u3m_pave(c3y, c3n);
}

/* _ch_test_no_cache(): test a hashtable without caching.
*/
static void
_ch_test_no_cache(void)
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
}

/* _ch_test_cache_trimming(): ensure a caching hashtable removes stale items.
*/
static void
_ch_test_cache_trimming(void)
{
  c3_w max_w = 30;
  c3_w i_w;

  //u3p(u3h_root) har_p = u3h_new_cache(max_w / 2);
  u3p(u3h_root) har_p = u3h_new_cache(1);

  for ( i_w = 0; i_w < max_w; i_w++ ) {
    fprintf(stderr, "****putting %d: %d******\r\n", i_w, i_w + max_w);
    u3h_put(har_p, i_w, i_w + max_w);
    fprintf(stderr, "********************************\r\n");
  }

  fprintf(stderr, "%d: %d\r\n", max_w - 1, u3h_get(har_p, max_w - 1));
}
