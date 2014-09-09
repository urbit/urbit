/* w/test.c
**
** This file is in the public domain.
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
#include <curses.h>
#include <termios.h>
#include <term.h>
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
    return u3_cm_bail(c3__fail);
  }
  fln_w = buf_b.st_size;
  pad_y = c3_malloc(buf_b.st_size);

  red_w = read(fid_i, pad_y, fln_w);
  close(fid_i);

  if ( fln_w != red_w ) {
    free(pad_y);
    return u3_cm_bail(c3__fail);
  }
  else {
    u3_noun pad = u3_ci_bytes(fln_w, (c3_y *)pad_y);
    free(pad_y);

    return pad;
  }
}
#endif

#if 0
static c3_w*
_test_walloc(c3_w siz_w)
{
  c3_w *ptr_w = u3_ca_walloc(siz_w);
  c3_w i_w;

  c3_assert(siz_w >= 1);
  *ptr_w = siz_w;

  for ( i_w = 1; i_w < siz_w; i_w++ ) {
    ptr_w[i_w] = u3_cr_mug((0xffff & (c3_p)(ptr_w)) + i_w);
  }
  return ptr_w;
}

static void
_test_free(c3_w* ptr_w)
{
  c3_w i_w, siz_w = *ptr_w;

  for ( i_w = 1; i_w < siz_w; i_w++ ) {
    c3_assert(ptr_w[i_w] == u3_cr_mug((0xffff & (c3_p)(ptr_w)) + i_w));
  }
  u3_ca_free(ptr_w);
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
    c3_w siz_w = c3_max(1, u3_cr_mug(i_w) & 0xff);

    one_w[i_w] = _test_walloc(siz_w);
    two_w[i_w] = _test_walloc(siz_w);
  }
  _road_sane();

  for ( i_w = 0; i_w < NUM; i_w++ ) {
    _test_free(two_w[NUM - (i_w + 1)]);
    _road_sane();
  }
  for ( i_w = 0; i_w < NUM; i_w++ ) {
    c3_w siz_w = c3_max(1, u3_cr_mug(i_w + 1) & 0xff);

    two_w[i_w] = _test_walloc(siz_w);
    _road_sane();
  }

  for ( i_w = 0; i_w < NUM; i_w++ ) {
    _test_free(one_w[NUM - (i_w + 1)]);
    _road_sane();
  }
  for ( i_w = 0; i_w < NUM; i_w++ ) {
    c3_w siz_w = c3_max(1, u3_cr_mug(i_w + 2) & 0xff);

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

#if 0
static void
_test_hash(void)
{
  _road_dump();
  {
    u3_ch_root* har_u = u3_ch_new();
    c3_w        i_w;
    c3_w        max_w = (1 << 20);

    for ( i_w = 0; i_w < max_w; i_w++ ) {
      u3_noun key = u3nc(0, i_w);

      u3_ch_put(har_u, key, (i_w + 1));
      u3z(key);
    }
    for ( i_w = 0; i_w < max_w; i_w++ ) {
      u3_noun key = u3nc(0, i_w);
      u3_noun val = u3_ch_get(har_u, key);

      if ( val != (i_w + 1) ) {
        if ( u3_none == val ) {
          printf("at %d, nothing\n", i_w);
        }
        else printf("at %d, oddly, is %d\n", i_w, val);
        c3_assert(0);
      }
      u3z(key);
    }
    u3_ch_free(har_u);
  }
  _road_dump();
}
#endif

#if 0
static void
_test_jam(void)
{
  _road_dump();
  {
    u3_noun pil = u3_walk_load("pill/easy1.pill");
    u3_noun cue, jam;

    printf("cueing pill - %d bytes\n", u3_cr_met(3, pil));
    cue = u3_cke_cue(pil);
    printf("cued - mug %x\n", u3_cr_mug(cue));

#if 1
    jam = u3_cke_jam(cue);
    printf("jammed - %d bytes\n", u3_cr_met(3, jam));
    cue = u3_cke_cue(jam);
    printf("cued - mug %x\n", u3_cr_mug(cue));
#endif

    u3z(cue);
  }
  _road_dump();
}
#endif

static void
_test_easy0(void)
{
  u3_noun cor = u3_cke_cue(u3_walk_load("pill/easy0.pill"));
  u3_noun val; 
 
  printf("test_easy1: core mug %x\n", u3_cr_mug(cor));
  val = u3_cn_slam_on(cor, u3nc(42, 17));
  printf("val %d\n", val);
}

static void
_test_test(void)
{
  u3_noun fol = u3_cke_cue(u3_walk_load("pill/west.pill"));
  u3_noun val; 
 
  printf("test_test: formula mug %x\n", u3_cr_mug(fol));
  val = u3_cn_nock_on(u3nc(42, 17), fol);
  printf("val %d\n", val);
  u3z(val);
}

// A simple memory tester.
//
int c3_cooked() { u3_cm_bail(c3__oops); return 0; }
int
main(int argc, char *argv[])
{
  printf("hello, world: len %dMB\n", (1 << U2_OS_LoomBits) >> 18);
  // _test_words();

  u3_cm_boot(U2_OS_LoomBase, (1 << U2_OS_LoomBits));
  u3_cm_dump();

  u3_cj_boot();
  printf("booted.\n");

  {
    // _test_hash();
    // _test_jam();
    // _test_easy0();
    _test_test();
  }
  u3_cm_clear();
  u3_cm_dump();
}
