#include "all.h"

// defined in noun/hashtable.c
c3_w _ch_skip_slot(c3_w mug_w, c3_w lef_w);

/* _setup(): prepare for tests.
*/
static void
_setup(void)
{
  u3m_init();
  u3m_pave(c3y);
}

/* _test_bit_manipulation():
*/
static c3_i
_test_bit_manipulation()
{
  c3_i ret_i = 1;

  if ( sizeof(u3_noun) != sizeof(u3h_slot) ) {
    fprintf(stderr, "bit manipulation: wrong size\r\n");
    ret_i = 0;
  }

  u3h_slot a = 0;

  if (u3h_slot_is_null(a) != c3y) {
    fprintf(stderr, "bit manipulation: nullity\r\n");
    ret_i = 0;
  }

  a = u3h_noun_be_warm(a);
  if (u3h_slot_is_warm(a) != c3y) {
    fprintf(stderr, "bit manipulation: warmth\r\n");
    ret_i = 0;
  }

  if (u3h_slot_is_null(a) != c3n) {
    fprintf(stderr, "bit manipulation: nullity 2\r\n");
    ret_i = 0;
  }

  a = u3h_noun_be_cold(a);
  if (u3h_slot_is_warm(a) != c3n) {
    fprintf(stderr, "bit manipulation: coldness\r\n");
    ret_i = 0;
  }

  return ret_i;
}

/* _test_no_cache(): test a hashtable without caching.
*/
static c3_i
_test_no_cache(void)
{
  c3_i ret_i = 1;
  c3_w max_w = 1000;
  c3_w   i_w;

  u3p(u3h_root) har_p = u3h_new();

  for ( i_w = 0; i_w < max_w; i_w++ ) {
    u3h_put(har_p, i_w, i_w + max_w);
  }

  for ( i_w = 0; i_w < max_w; i_w++ ) {
    if ( (i_w + max_w) != u3h_get(har_p, i_w) ) {
      fprintf(stderr, "bit test_no_cache: get failed\r\n");
      ret_i = 0;
    }
  }

  u3h_free(har_p);
  return ret_i;
}

/* _test_skip_slot():
*/
static c3_i
_test_skip_slot(void)
{
  c3_i ret_i = 1;

  //  root table
  {
    c3_w mug_w = 0x17 << 25;
    c3_w res_w = _ch_skip_slot(mug_w, 25);

    if ( (0x18 << 25) != res_w ) {
      fprintf(stderr, "bit skip_slot (a): failed\r\n");
      ret_i = 0;
    }
  }

  {
    c3_w mug_w = 63 << 25; //  6 bits, all ones
    c3_w res_w = _ch_skip_slot(mug_w, 25);

    if ( 0 != res_w ) {
      fprintf(stderr, "bit skip_slot (b): failed\r\n");
      ret_i = 0;
    }
  }

  //  child nodes
  {
    c3_w mug_w = 17 << 20;
    c3_w res_w = _ch_skip_slot(mug_w, 20);

    if ( (18 << 20) != res_w ) {
      fprintf(stderr, "bit skip_slot (c): failed\r\n");
      ret_i = 0;
    }
  }

  {
    c3_w mug_w = 31 << 20; //  5 bits, all ones
    c3_w res_w = _ch_skip_slot(mug_w, 20);
    c3_assert((1 << 25) == res_w);

    if ( (1 << 25) != res_w ) {
      fprintf(stderr, "bit skip_slot (d): failed\r\n");
      ret_i = 0;
    }
  }

  return ret_i;
}

/* _test_cache_trimming(): ensure a caching hashtable removes stale items.
*/
static c3_i
_test_cache_trimming(void)
{
  c3_i ret_i = 1;
  c3_w max_w = 620;
  c3_w   i_w;

  //u3p(u3h_root) har_p = u3h_new_cache(max_w / 2);
  u3p(u3h_root) har_p = u3h_new_cache(max_w / 10 );
  u3h_root*     har_u = u3to(u3h_root, har_p);

  for ( i_w = 0; i_w < max_w; i_w++ ) {
    u3h_put(har_p, i_w, i_w + max_w);
  }

  if ( ( max_w + max_w - 1) != u3h_get(har_p, max_w - 1) ) {
    fprintf(stderr, "cache_trimming (a): fail\r\n");
    ret_i = 0;
  }
  if ( ( max_w / 10 ) != har_u->use_w ) {
    fprintf(stderr, "cache_trimming (b): fail\r\n");
    ret_i = 0;
  }

  u3h_free(har_p);
  return ret_i;
}

/* _test_cache_replace_value():
*/
static c3_i
_test_cache_replace_value(void)
{
  c3_i ret_i = 1;
  c3_w max_w = 100;
  c3_w   i_w;

  u3p(u3h_root) har_p = u3h_new_cache(max_w);
  u3h_root*     har_u = u3to(u3h_root, har_p);

  for ( i_w = 0; i_w < max_w; i_w++ ) {
    u3h_put(har_p, i_w, i_w + max_w);
  }

  for ( i_w = 0; i_w < max_w; i_w++ ) {
    u3h_put(har_p, i_w, i_w + max_w + 1);
  }

  if ( (2 * max_w) != u3h_get(har_p, max_w - 1) ) {
    fprintf(stderr, "cache_replace (a): fail\r\n");
    ret_i = 0;
  }
  if ( max_w != har_u->use_w ) {
    fprintf(stderr, "cache_replace (b): fail\r\n");
    ret_i = 0;
  }

  u3h_free(har_p);
  return ret_i;
}

static c3_i
_test_hashtable(void)
{
  c3_i ret_i = 1;

  ret_i &= _test_bit_manipulation();
  ret_i &= _test_no_cache();
  ret_i &= _test_skip_slot();
  ret_i &= _test_cache_trimming();
  ret_i &= _test_cache_replace_value();

  return ret_i;
}

/* main(): run all test cases.
*/
int
main(int argc, char* argv[])
{
  _setup();

  if ( !_test_hashtable() ) {
    fprintf(stderr, "test_hashtable: failed\r\n");
    exit(1);
  }

  //  GC
  //
  u3m_grab(u3_none);

  fprintf(stderr, "test_hashtable: ok\r\n");

  return 0;
}
