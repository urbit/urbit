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

static void
_road_dump(void)
{
  c3_w hat_w;
  c3_w fre_w = 0;
  c3_w i_w;

  hat_w = u3_so(u3_co_is_north) ? u3R->hat_w - u3R->rut_w 
                                : u3R->hat_w - u3R->rut_w;

  for ( i_w = 0; i_w < u3_cc_fbox_no; i_w++ ) {
    u3_cs_fbox* fre_u = u3R->all.fre_u[i_w];
    
    while ( fre_u ) {
      fre_w += fre_u->box_u.siz_w;
      fre_u = fre_u->nex_u;
    }
  }
  printf("dump: hat_w %x, fre_w %x, allocated %x\n",
          hat_w, fre_w, (hat_w - fre_w));

  if ( 0 != (hat_w - fre_w) ) {
    c3_w* box_w = u3R->rut_w;
    c3_w  mem_w = 0;

    while ( box_w < u3R->hat_w ) {
      u3_cs_box* box_u = (void *)box_w;

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
    u3_noun pil = u3_walk_load("urb/urbit.pill");
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

// A simple memory tester.
//
int c3_cooked() { u3_cm_bail(c3__oops); return 0; }
int
main(int argc, char *argv[])
{
  printf("hello, world: len %dMB\n", (1 << U2_OS_LoomBits) >> 18);
  // _test_words();

  u3_cm_boot(U2_OS_LoomBase, (1 << U2_OS_LoomBits));
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

/* u3_mur_words(): 31-bit nonzero MurmurHash3 on raw words.
*/
c3_w
u3_mur_words(const c3_w* key_w, c3_w len_w)
{
  c3_w syd_w = 0xcafebabe;

  while ( 1 ) {
    c3_w haz_w = _mur_words(syd_w, key_w, len_w);
    c3_w ham_w = (haz_w >> 31) ^ (haz_w & 0x7fffffff);

    if ( 0 != ham_w ) return ham_w;
    else syd_w++;
  }
}

/* u3_mur_both():
**
**   Join two murs.
*/
c3_w
u3_mur_both(c3_w lef_w, c3_w rit_w)
{
  c3_w ham_w = lef_w ^ (0x7fffffff ^ rit_w);

  return u3_mur_words(&ham_w, (0 == ham_w) ? 0 : 1);
}

/* u3_mur(): MurmurHash3 on a noun.
*/
c3_w
u3_mur(u3_noun veb)
{
  if ( u3_fly_is_cat(veb) ) {
    return u3_mur_words(&veb, (0 == veb) ? 0 : 1);
  }
  else {
    c3_w mur_w;

    if ( (mur_w=*u3_at_dog_mur(veb)) ) {
      return mur_w;
    }

    if ( u3_dog_is_pom(veb) ) {
      mur_w = u3_mur_both(u3_mur(u3h(veb)), u3_mur(u3t(veb)));
    }
    else {
      c3_w  len_w = u3_met(5, veb);
      c3_w* buf_w = malloc(4 * len_w);

      u3_words(0, len_w, buf_w, veb);
      mur_w = u3_mur_words(buf_w, len_w);

      free(buf_w);
    }

    *u3_at_dog_mur(veb) = mur_w;
    return mur_w;
  }
}

/* u3_mur_string():
**
**   Compute the mur of `a`, LSB first.
*/
c3_w
u3_mur_string(const c3_c *a_c)
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
  return u3_mur_words(buf_w, wor_w);
}

/* u3_mur_cell():
**
**   Compute the mur of the cell `[hed tel]`.
*/
c3_w
u3_mur_cell(u3_noun hed,
            u3_noun tel)
{
  c3_w   lus_w = u3_mur(hed);
  c3_w   biq_w = u3_mur(tel);

  return u3_mur_both(lus_w, biq_w);
}

/* u3_mur_trel():
**
**   Compute the mur of `[a b c]`.
*/
c3_w
u3_mur_trel(u3_noun a,
            u3_noun b,
            u3_noun c)
{
  return u3_mur_both(u3_mur(a), u3_mur_both(u3_mur(b), u3_mur(c)));
}

/* u3_mur_qual():
**
**   Compute the mur of `[a b c d]`.
*/
c3_w
u3_mur_qual(u3_noun a,
            u3_noun b,
            u3_noun c,
            u3_noun d)
{
  return u3_mur_both(u3_mur(a),
                     u3_mur_both(u3_mur(b),
                                 u3_mur_both(u3_mur(c), u3_mur(d))));
}
#endif
