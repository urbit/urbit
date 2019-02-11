/* w/test.c
**
*/
#define C3_GLOBAL

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <dirent.h>
#include <stdint.h>
#include <uv.h>
#include <ncurses/curses.h>
#include <termios.h>
#include <ncurses/term.h>
#include <errno.h>

#include "all.h"

#if 1
/* u3_walk_load(): load file or bail.
*/
static u3_noun
u3_walk_load(c3_c* pas_c)
{
  struct stat buf_b;
  c3_i        fid_i = open(pas_c, O_RDONLY, 0644);
  c3_w        fln_w, red_w;
  c3_y*       pad_y;

  if ( (fid_i < 0) || (fstat(fid_i, &buf_b) < 0) ) {
    fprintf(stderr, "%s: %s\r\n", pas_c, strerror(errno));
    return u3m_bail(c3__fail);
  }
  fln_w = buf_b.st_size;
  pad_y = c3_malloc(buf_b.st_size);

  red_w = read(fid_i, pad_y, fln_w);
  close(fid_i);

  if ( fln_w != red_w ) {
    free(pad_y);
    return u3m_bail(c3__fail);
  }
  else {
    u3_noun pad = u3i_bytes(fln_w, (c3_y *)pad_y);
    free(pad_y);

    return pad;
  }
}
#endif

#if 0
static c3_w*
_test_walloc(c3_w siz_w)
{
  c3_w *ptr_w = u3a_walloc(siz_w);
  c3_w i_w;

  c3_assert(siz_w >= 1);
  *ptr_w = siz_w;

  for ( i_w = 1; i_w < siz_w; i_w++ ) {
    ptr_w[i_w] = u3r_mug((0xffff & (c3_p)(ptr_w)) + i_w);
  }
  return ptr_w;
}

static void
_test_free(c3_w* ptr_w)
{
  c3_w i_w, siz_w = *ptr_w;

  for ( i_w = 1; i_w < siz_w; i_w++ ) {
    c3_assert(ptr_w[i_w] == u3r_mug((0xffff & (c3_p)(ptr_w)) + i_w));
  }
  u3a_free(ptr_w);
}

#define NUM 16384

// Simple allocation test.
//
void
test(void)
{
  c3_w* one_w[NUM];
  c3_w* two_w[NUM];
  c3_w  i_w;

  for ( i_w = 0; i_w < NUM; i_w++ ) {
    c3_w siz_w = c3_max(1, u3r_mug(i_w) & 0xff);

    one_w[i_w] = _test_walloc(siz_w);
    two_w[i_w] = _test_walloc(siz_w);
  }
  _road_sane();

  for ( i_w = 0; i_w < NUM; i_w++ ) {
    _test_free(two_w[NUM - (i_w + 1)]);
    _road_sane();
  }
  for ( i_w = 0; i_w < NUM; i_w++ ) {
    c3_w siz_w = c3_max(1, u3r_mug(i_w + 1) & 0xff);

    two_w[i_w] = _test_walloc(siz_w);
    _road_sane();
  }

  for ( i_w = 0; i_w < NUM; i_w++ ) {
    _test_free(one_w[NUM - (i_w + 1)]);
    _road_sane();
  }
  for ( i_w = 0; i_w < NUM; i_w++ ) {
    c3_w siz_w = c3_max(1, u3r_mug(i_w + 2) & 0xff);

    one_w[i_w] = _test_walloc(siz_w);
    _road_sane();
  }

  for ( i_w = 0; i_w < NUM; i_w++ ) {
    _test_free(one_w[NUM - (i_w + 1)]);
    _road_sane();
  }
  for ( i_w = 0; i_w < NUM; i_w++ ) {
    _test_free(two_w[NUM - (i_w + 1)]);
    _road_sane();
  }

  printf("allocations %d, iterations %d\n", ALL_w, ITE_w);
}
#endif

#if 1
static void
_test_hash(void)
{
  // u3m_dump();
  {
    u3h_root* har_u = u3h_new();
    c3_w        i_w;
    c3_w        max_w = (1 << 20);

    for ( i_w = 0; i_w < max_w; i_w++ ) {
      u3_noun key = u3nc(0, i_w);

      u3h_put(har_u, key, (i_w + 1));
      u3z(key);
    }
    for ( i_w = 0; i_w < max_w; i_w++ ) {
      u3_noun key = u3nc(0, i_w);
      u3_noun val = u3h_get(har_u, key);

      if ( val != (i_w + 1) ) {
        if ( u3_none == val ) {
          printf("at %d, nothing\n", i_w);
        }
        else printf("at %d, oddly, is %d\n", i_w, val);
        c3_assert(0);
      }
      u3z(key);
    }
    u3h_free(har_u);
  }
  // u3m_dump();
}
#endif

#if 1
static void
_test_jam(void)
{
  // u3m_dump();
  {
    u3_noun pil = u3_walk_load("urb/urbit.pill");
    u3_noun cue, jam;

    printf("cueing pill - %d bytes\n", u3r_met(3, pil));
    cue = u3ke_cue(pil);
    printf("cued - mug %x\n", u3r_mug(cue));

#if 1
    jam = u3ke_jam(cue);
    printf("jammed - %d bytes\n", u3r_met(3, jam));
    cue = u3ke_cue(jam);
    printf("cued - mug %x\n", u3r_mug(cue));
#endif

    u3z(cue);
  }
  // u3m_dump();
}
#endif

static void
_test_leap(void)
{
#if 1
  // u3m_dump();
  {
    u3_noun pil; 
    u3_noun cue, jam;
    c3_w gof_w = u3m_golf();

    pil = u3_walk_load("urb/urbit.pill"); 
    u3m_leap(0);
    printf("cueing pill - %d bytes\n", u3r_met(3, pil));
    cue = u3ke_cue(pil);
    printf("cued - %p, mug %x\n", u3a_to_ptr(cue), u3r_mug(cue));
    u3m_fall();

    cue = u3a_take(cue);
    printf("taken - %p, mug %x\n", u3a_to_ptr(cue), u3r_mug(cue));
    u3m_flog(gof_w);
    u3z(pil);

#if 1
    jam = u3ke_jam(cue);
    printf("jammed - %d bytes\n", u3r_met(3, jam));
    cue = u3ke_cue(jam);
    printf("cued - mug %x\n", u3r_mug(cue));
#endif

    u3z(cue);
  }
  // u3m_dump();
#endif
}

static void
_test_test(void)
{
  u3_noun fol = u3ke_cue(u3_walk_load("pill/west.pill"));
  u3_noun val; 
 
  printf("test_test: formula mug %x\n", u3r_mug(fol));
  val = u3n_nock_on(u3nc(42, 17), fol);
  printf("val %d\n", val);
  u3z(val);
}

int FOO;

// A simple memory tester.
//
int
main(int argc, char *argv[])
{
  printf("hello, world: len %dMB\n", (1 << U3_OS_LoomBits) >> 18);
  // _test_words();

  u3m_init();
  u3m_pave(c3y, c3n);
  // u3j_boot();

  // u3m_dump();

  printf("booted.\n");

  {
    _test_leap();
    // _test_hash();
    // _test_jam();
  }
  // u3m_clear();

  // u3m_dump();
}
