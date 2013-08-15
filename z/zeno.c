/* z/zeno.c
**
** This file is in the public domain.
*/
#include "all.h"

/* _zi_boot_l(): boot the l (loom) layer.
*/
static void
_zi_boot_l(u3_z z)
{
  z->l.cap_ray += (c3_wiseof(struct u3_zeno) - 
                   c3_wiseof(struct u3_loom));
}

/* _zi_boot_n(): boot the n (nock) layer.
*/
static void
_zi_boot_n(u3_z z)
{
  z->n.lab_ray = 0;
}

#if 0
/* _zi_boot_qdec(): boot decrement for testing.
*/
static u3_flag
_zi_boot_qdec(u3_z z)
{
  u3_rat naz = u3_b_load(&z->l, u3_cm_dec);

  if ( u3_none == naz ) {
    return u3_no;
  }
  else {
    u3_rat gaq = u3_b_watt(&z->l, naz);

    if ( u3_none == gaq ) {
      return u3_no;
    } else {
      /* fip: initial mold
      ** gub: [mold form] of kernel gene
      */
      u3_fox fip = u3_ln_trel
        (z, u3_cm_crib, 
            u3_ln_cell(z, 'a', u3_cm_atom),
            0);
      u3_rat gub = u3_b_mill(&z->l, gaq, fip);
 
      if ( u3_none == gub ) {
        return u3_no;
      }
      else {
        z->q.dec = u3_t(z, gub);

        return u3_yes;
      }
    }
  }
}

/* _zi_boot_q(): boot the q (watt) layer.
*/
static u3_flag
_zi_boot_q(u3_z z)
{
  u3_rat poc = u3_b_load(&z->l, u3_cm_reck);

  if ( u3_none == poc ) {
    return u3_no;
  }
  else {
    /* zum: kernel gene
    */
    u3_rat zum = u3_b_watt(&z->l, poc);

    if ( u3_none == zum ) {
      return u3_no;
    }
    else {
      /* fip: initial mold
      ** gub: [mold form] of kernel gene
      */
      u3_fox fip = u3_ln_cell(z, u3_cm_cube, 0);
      u3_rat gub = u3_b_mill(&z->l, zum, fip);
 
      if ( u3_none == gub ) {
        return u3_no;
      }
      else {
        /* har: kernel mold
        ** fet: kernel form
        ** cug: kernel grit
        */
        u3_fox har = u3_h(z, gub);
        u3_fox fet = u3_t(z, gub);
        u3_fox cug;

        if ( (0 != u3_z_run(z, &cug, 0, fet, 0)) ) {
          return u3_no;
        }
        else {
          z->q.tef = u3_ln_cell(z, har, cug);
          return u3_yes;
        }
      }
    }
  }
}
#endif

/* u3_z_do():
**
**   Execute (nock a b), asserting on failure.
*/
u3_fox
u3_z_do(u3_z   z,
        u3_fox a,
        u3_fox b)
{
  u3_fox fev;

  if ( 0 == u3_z_run(z, &fev, a, b, 0) ) {
    return fev;
  }
  else {
    c3_assert(0); return 0;
  }
}

/* u3_z_new():
**
**   Create a zeno core, mallocing (1 << y_a) words of memory.
**   Return 0 if malloc fails.  Free with free().
*/
u3_z
u3_z_new(c3_y y_a)
{
  u3_l l;

  if ( !(l = u3_lm_new(y_a) ) ) {
    return 0;
  }
  else {
    u3_z z = (void *)l;

    _zi_boot_l(z);
    _zi_boot_n(z);

#if 0
    if ( u3_no == _zi_boot_q(z) ) {
      free(z); return 0;
    }
    if ( u3_no == _zi_boot_qdec(z) ) {
      free(z); return 0;
    }
    if ( u3_no == u3_zj_boot(z, 255) ) {
      free(z); return 0;
    }
#endif
    return z;
  }
}
