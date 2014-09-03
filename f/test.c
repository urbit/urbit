/* f/test.c
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

#include "f/meme.h"

#if 1
/* u2_walk_load(): load file or bail.
*/
static u2_noun
u2_walk_load(c3_c* pas_c)
{
  struct stat buf_b;
  c3_i        fid_i = open(pas_c, O_RDONLY, 0644);
  c3_w        fln_w, red_w;
  c3_y*       pad_y;

  if ( (fid_i < 0) || (fstat(fid_i, &buf_b) < 0) ) {
    fprintf(stderr, "%s: %s\r\n", pas_c, strerror(errno));
    return u2_cm_bail(c3__fail);
  }
  fln_w = buf_b.st_size;
  pad_y = c3_malloc(buf_b.st_size);

  red_w = read(fid_i, pad_y, fln_w);
  close(fid_i);

  if ( fln_w != red_w ) {
    free(pad_y);
    return u2_cm_bail(c3__fail);
  }
  else {
    u2_noun pad = u2_ci_bytes(fln_w, (c3_y *)pad_y);
    free(pad_y);

    return pad;
  }
}
#endif

static void
_road_dump(void)
{
  c3_w hat_w;
  c3_w fre_w = 0;
  c3_w i_w;

  hat_w = u2_so(u2_co_is_north) ? u2R->hat_w - u2R->rut_w 
                                : u2R->hat_w - u2R->rut_w;

  for ( i_w = 0; i_w < u2_cc_fbox_no; i_w++ ) {
    u2_cs_fbox* fre_u = u2R->all.fre_u[i_w];
    
    while ( fre_u ) {
      fre_w += fre_u->box_u.siz_w;
      fre_u = fre_u->nex_u;
    }
  }
  printf("dump: hat_w %x, fre_w %x, allocated %x\n",
          hat_w, fre_w, (hat_w - fre_w));

  if ( 0 != (hat_w - fre_w) ) {
    c3_w* box_w = u2R->rut_w;
    c3_w  mem_w = 0;

    while ( box_w < u2R->hat_w ) {
      u2_cs_box* box_u = (void *)box_w;

      if ( 0 != box_u->use_w ) {
        mem_w += box_u->siz_w;
      }
      box_w += box_u->siz_w;
    }

    printf("second count: %x\n", mem_w);
  }
}

static void
_test_jam(void)
{
  _road_dump();
  {
    u2_noun pil = u2_walk_load("urb/urbit.pill");
    u2_noun cue, jam;

    printf("cueing pill - %d bytes\n", u2_cr_met(3, pil));
    cue = u2_cke_cue(pil);
    printf("cued - mug %x\n", u2_cr_mug(cue));

#if 1
    jam = u2_cke_jam(cue);
    printf("jammed - %d bytes\n", u2_cr_met(3, jam));
    cue = u2_cke_cue(jam);
    printf("cued - mug %x\n", u2_cr_mug(cue));
#endif

    u2z(cue);
  }
  _road_dump();
}

#if 0
static c3_w*
_test_walloc(c3_w siz_w)
{
  c3_w *ptr_w = u2_ca_walloc(siz_w);
  c3_w i_w;

  c3_assert(siz_w >= 1);
  *ptr_w = siz_w;

  for ( i_w = 1; i_w < siz_w; i_w++ ) {
    ptr_w[i_w] = u2_cr_mug((0xffff & (c3_p)(ptr_w)) + i_w);
  }
  return ptr_w;
}

static void
_test_free(c3_w* ptr_w)
{
  c3_w i_w, siz_w = *ptr_w;

  for ( i_w = 1; i_w < siz_w; i_w++ ) {
    c3_assert(ptr_w[i_w] == u2_cr_mug((0xffff & (c3_p)(ptr_w)) + i_w));
  }
  u2_ca_free(ptr_w);
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
    c3_w siz_w = c3_max(1, u2_cr_mug(i_w) & 0xff);

    one_w[i_w] = _test_walloc(siz_w);
    two_w[i_w] = _test_walloc(siz_w);
  }
  _road_sane();

  for ( i_w = 0; i_w < NUM; i_w++ ) {
    _test_free(two_w[NUM - (i_w + 1)]);
    _road_sane();
  }
  for ( i_w = 0; i_w < NUM; i_w++ ) {
    c3_w siz_w = c3_max(1, u2_cr_mug(i_w + 1) & 0xff);

    two_w[i_w] = _test_walloc(siz_w);
    _road_sane();
  }

  for ( i_w = 0; i_w < NUM; i_w++ ) {
    _test_free(one_w[NUM - (i_w + 1)]);
    _road_sane();
  }
  for ( i_w = 0; i_w < NUM; i_w++ ) {
    c3_w siz_w = c3_max(1, u2_cr_mug(i_w + 2) & 0xff);

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

static void
_test_hash(void)
{
  _road_dump();
  {
    u2_ch_root* har_u = u2_ch_new();
    c3_w        i_w;
    c3_w        max_w = (1 << 20);

    for ( i_w = 0; i_w < max_w; i_w++ ) {
      u2_noun key = u2nc(0, i_w);

      u2_ch_put(har_u, key, (i_w + 1));
      u2z(key);
    }
    for ( i_w = 0; i_w < max_w; i_w++ ) {
      u2_noun key = u2nc(0, i_w);
      u2_noun val = u2_ch_get(har_u, key);

      if ( val != (i_w + 1) ) {
        if ( u2_none == val ) {
          printf("at %d, nothing\n", i_w);
        }
        else printf("at %d, oddly, is %d\n", i_w, val);
        c3_assert(0);
      }
      u2z(key);
    }
    u2_ch_free(har_u);
  }
  _road_dump();
}

// A simple memory tester.
//
int c3_cooked() { u2_cm_bail(c3__oops); return 0; }
int
main(int argc, char *argv[])
{
  printf("hello, world: len %dMB\n", (1 << U2_OS_LoomBits) >> 18);
  // _test_words();

  u2_cm_boot(U2_OS_LoomBase, (1 << U2_OS_LoomBits));
  printf("booted.\n");

  _test_hash();
}

#if 0

/* Finalization mix for better avalanching.
*/
static c3_w 
_mur_fmix(c3_w h_w)
{
  h_w ^= h_w >> 16;
  h_w *= 0x85ebca6b;
  h_w ^= h_w >> 13;
  h_w *= 0xc2b2ae35;
  h_w ^= h_w >> 16;

  return h_w;
}

/* _mur_words(): raw MurmurHash3 on raw words.
*/
static c3_w
_mur_words(c3_w syd_w, const c3_w* key_w, c3_w len_w)
{
  c3_w goc_w = syd_w;
  c3_w lig_w = 0xcc9e2d51;
  c3_w duf_w = 0x1b873593;
  c3_w i_w;

  for ( i_w = 0; i_w < len_w; i_w++ ) {
    c3_w kop_w = key_w[i_w];

    kop_w *= lig_w;
    kop_w = c3_rotw(15, kop_w);
    kop_w *= duf_w;

    goc_w ^= kop_w;
    goc_w = c3_rotw(13, goc_w); 
    goc_w = (goc_w * 5) + 0xe6546b64;
  }
  goc_w ^= len_w;
  goc_w = _mur_fmix(goc_w);

  return goc_w;
}

/* u2_mur_words(): 31-bit nonzero MurmurHash3 on raw words.
*/
c3_w
u2_mur_words(const c3_w* key_w, c3_w len_w)
{
  c3_w syd_w = 0xcafebabe;

  while ( 1 ) {
    c3_w haz_w = _mur_words(syd_w, key_w, len_w);
    c3_w ham_w = (haz_w >> 31) ^ (haz_w & 0x7fffffff);

    if ( 0 != ham_w ) return ham_w;
    else syd_w++;
  }
}

/* u2_mur_both():
**
**   Join two murs.
*/
c3_w
u2_mur_both(c3_w lef_w, c3_w rit_w)
{
  c3_w ham_w = lef_w ^ (0x7fffffff ^ rit_w);

  return u2_mur_words(&ham_w, (0 == ham_w) ? 0 : 1);
}

/* u2_mur(): MurmurHash3 on a noun.
*/
c3_w
u2_mur(u2_noun veb)
{
  if ( u2_fly_is_cat(veb) ) {
    return u2_mur_words(&veb, (0 == veb) ? 0 : 1);
  }
  else {
    c3_w mur_w;

    if ( (mur_w=*u2_at_dog_mur(veb)) ) {
      return mur_w;
    }

    if ( u2_dog_is_pom(veb) ) {
      mur_w = u2_mur_both(u2_mur(u2h(veb)), u2_mur(u2t(veb)));
    }
    else {
      c3_w  len_w = u2_met(5, veb);
      c3_w* buf_w = malloc(4 * len_w);

      u2_words(0, len_w, buf_w, veb);
      mur_w = u2_mur_words(buf_w, len_w);

      free(buf_w);
    }

    *u2_at_dog_mur(veb) = mur_w;
    return mur_w;
  }
}

/* u2_mur_string():
**
**   Compute the mur of `a`, LSB first.
*/
c3_w
u2_mur_string(const c3_c *a_c)
{
  c3_w  len_w = strlen(a_c);
  c3_w  wor_w = ((len_w + 3) >> 2);
  c3_w* buf_w = alloca(4 * wor_w);
  c3_w  i_w;

  for ( i_w = 0; i_w < wor_w; i_w++ ) { buf_w[i_w] = 0; }

  for ( i_w = 0; i_w < len_w; i_w++ ) {
    c3_w inx_w = (i_w >> 2);
    c3_w byt_w = (i_w & 3);

    buf_w[inx_w] |= (a_c[i_w] << (8 * byt_w));
  }
  return u2_mur_words(buf_w, wor_w);
}

/* u2_mur_cell():
**
**   Compute the mur of the cell `[hed tel]`.
*/
c3_w
u2_mur_cell(u2_noun hed,
            u2_noun tel)
{
  c3_w   lus_w = u2_mur(hed);
  c3_w   biq_w = u2_mur(tel);

  return u2_mur_both(lus_w, biq_w);
}

/* u2_mur_trel():
**
**   Compute the mur of `[a b c]`.
*/
c3_w
u2_mur_trel(u2_noun a,
            u2_noun b,
            u2_noun c)
{
  return u2_mur_both(u2_mur(a), u2_mur_both(u2_mur(b), u2_mur(c)));
}

/* u2_mur_qual():
**
**   Compute the mur of `[a b c d]`.
*/
c3_w
u2_mur_qual(u2_noun a,
            u2_noun b,
            u2_noun c,
            u2_noun d)
{
  return u2_mur_both(u2_mur(a),
                     u2_mur_both(u2_mur(b),
                                 u2_mur_both(u2_mur(c), u2_mur(d))));
}
#endif
