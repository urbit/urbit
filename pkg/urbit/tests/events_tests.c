#include "all.h"
#include "noun/events.h"

/* _setup(): prepare for tests.
*/
static void
_setup(void)
{
  //  NB: no loom
  //
  u3P.pag_w = u3a_pages;
}

static c3_w
_check_north_clean(void)
{
  c3_w i_w, pag_w, blk_w, bit_w;

  for ( i_w = 0; i_w < u3P.pag_w; i_w++ ) {
    pag_w = i_w;
    blk_w = pag_w >> 5;
    bit_w = pag_w & 31;

    if ( u3P.dit_w[blk_w] & (1 << bit_w) ) {
      break;
    }
  }

  return i_w;
}

static c3_w
_check_north_dirty(c3_w pgs_w, c3_w max_w)
{
  c3_w i_w, pag_w, blk_w, bit_w;

  for ( i_w = 0; i_w < max_w; i_w++ ) {
    pag_w = i_w + pgs_w;
    blk_w = pag_w >> 5;
    bit_w = pag_w & 31;

    if ( !(u3P.dit_w[blk_w] & (1 << bit_w)) ) {
      break;
    }
  }

  return i_w;
}

static c3_w
_check_south_clean(void)
{
  c3_w i_w, pag_w, blk_w, bit_w;

  for ( i_w = 0; i_w < u3P.pag_w; i_w++ ) {
    pag_w = u3P.pag_w - (i_w + 1);
    blk_w = pag_w >> 5;
    bit_w = pag_w & 31;

    if ( u3P.dit_w[blk_w] & (1 << bit_w) ) {
      break;
    }
  }

  return i_w;
}

static c3_w
_check_south_dirty(c3_w pgs_w, c3_w max_w)
{
  c3_w i_w, pag_w, blk_w, bit_w;

  for ( i_w = 0; i_w < max_w; i_w++ ) {
    pag_w = u3P.pag_w - (i_w + pgs_w + 1);
    blk_w = pag_w >> 5;
    bit_w = pag_w & 31;

    if ( !(u3P.dit_w[blk_w] & (1 << bit_w)) ) {
      break;
    }
  }

  return i_w;
}

void
_ce_loom_track_north(c3_w pgs_w, c3_w dif_w);
void
_ce_loom_track_south(c3_w pgs_w, c3_w dif_w);

static c3_i
_test_tracking(void)
{
  c3_w ret_w;

  _ce_loom_track_north(0, u3P.pag_w);

  if ( u3P.pag_w != (ret_w = _check_north_dirty(0, u3P.pag_w)) ) {
    fprintf(stderr, "test events track north dirty all %u\r\n", ret_w);
    return 0;
  }

  if ( 0 != (ret_w = _check_north_clean()) ) {
    fprintf(stderr, "test events track north init %u\r\n", ret_w);
    return 0;
  }

  if ( 0 != (ret_w = _check_south_clean()) ) {
    fprintf(stderr, "test events track south init %u\r\n", ret_w);
    return 0;
  }

  _ce_loom_track_north(100, 0);
  _ce_loom_track_south(1, 0);

  if ( 100 != (ret_w = _check_north_clean()) ) {
    fprintf(stderr, "test events track north clean a %u\r\n", ret_w);
    return 0;
  }

  if ( 1 != (ret_w = _check_south_clean()) ) {
    fprintf(stderr, "test events track south clean a %u\r\n", ret_w);
    return 0;
  }

  _ce_loom_track_north(75, 25);
  _ce_loom_track_south(2, 0);

  if ( 75 != (ret_w = _check_north_clean()) ) {
    fprintf(stderr, "test events track north clean b %u\r\n", ret_w);
    return 0;
  }

  if ( 25 != (ret_w = _check_north_dirty(75, 25)) ) {
    fprintf(stderr, "test events track north dirty b %u\r\n", ret_w);
    return 0;
  }

  if ( 2 != (ret_w = _check_south_clean()) ) {
    fprintf(stderr, "test events track south clean b %u\r\n", ret_w);
    return 0;
  }

  _ce_loom_track_north(55, 20);
  _ce_loom_track_south(1, 1);

  if ( 55 != (ret_w = _check_north_clean()) ) {
    fprintf(stderr, "test events track north clean c %u\r\n", ret_w);
    return 0;
  }

  if ( 20 != (ret_w = _check_north_dirty(55, 20)) ) {
    fprintf(stderr, "test events track north dirty c %u\r\n", ret_w);
    return 0;
  }

  if ( 1 != (ret_w = _check_south_clean()) ) {
    fprintf(stderr, "test events track south clean c %u\r\n", ret_w);
    return 0;
  }

  if ( 1 != (ret_w = _check_south_dirty(1, 1)) ) {
    fprintf(stderr, "test events track north dirty c %u\r\n", ret_w);
    return 0;
  }

  _ce_loom_track_north(255, 0);
  _ce_loom_track_south(48, 0);

  if ( 255 != (ret_w = _check_north_clean()) ) {
    fprintf(stderr, "test events track north clean d %u\r\n", ret_w);
    return 0;
  }

  if ( 48 != (ret_w = _check_south_clean()) ) {
    fprintf(stderr, "test events track south clean d %u\r\n", ret_w);
    return 0;
  }

  _ce_loom_track_north(213, 42);
  _ce_loom_track_south(15, 33);

  if ( 213 != (ret_w = _check_north_clean()) ) {
    fprintf(stderr, "test events track north clean e %u\r\n", ret_w);
    return 0;
  }

  if ( 42 != (ret_w = _check_north_dirty(213, 42)) ) {
    fprintf(stderr, "test events track north dirty e %u\r\n", ret_w);
    return 0;
  }

  if ( 15 != (ret_w = _check_south_clean()) ) {
    fprintf(stderr, "test events track south clean e %u\r\n", ret_w);
    return 0;
  }

  if ( 33 != (ret_w = _check_south_dirty(15, 33)) ) {
    fprintf(stderr, "test events track north dirty e %u\r\n", ret_w);
    return 0;
  }

  _ce_loom_track_north(200, 13);
  _ce_loom_track_south(10, 5);

  if ( 200 != (ret_w = _check_north_clean()) ) {
    fprintf(stderr, "test events track north clean f %u\r\n", ret_w);
    return 0;
  }

  if ( 13 != (ret_w = _check_north_dirty(200, 13)) ) {
    fprintf(stderr, "test events track north dirty f %u\r\n", ret_w);
    return 0;
  }

  if ( 10 != (ret_w = _check_south_clean()) ) {
    fprintf(stderr, "test events track south clean f %u\r\n", ret_w);
    return 0;
  }

  if ( 5 != (ret_w = _check_south_dirty(10, 5)) ) {
    fprintf(stderr, "test events track north dirty f %u\r\n", ret_w);
    return 0;
  }

  _ce_loom_track_north(0, u3P.pag_w);

  if ( u3P.pag_w != (ret_w = _check_north_dirty(0, u3P.pag_w)) ) {
    fprintf(stderr, "test events track north dirty all %u\r\n", ret_w);
    return 0;
  }

  return 1;
}

/* main(): run all test cases.
*/
int
main(int argc, char* argv[])
{
   _setup();

  if ( !_test_tracking() ) {
    fprintf(stderr, "test_events: tracking: failed\r\n");
    exit(1);
  }

  fprintf(stderr, "test_events: ok\n");

  return 0;
}
