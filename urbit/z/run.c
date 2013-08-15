/* z/run.c
**
** This file is in the public domain.
*/
#include "all.h"

/* _zn_depart(): depart to temporary construction.
*/
static inline u3_ray
_zn_depart(u3_z z)
{
  return u3_lm_flap(z);
}

/* _zn_retreat(): retreat from temporary construction.
**
**   mat: saved mat, from _zn_depart().
*/
static inline void
_zn_retreat(u3_z   z,
            u3_ray mat_ray)
{
  u3_lm_flop(z, mat_ray);
}

/* _zn_free(): free memory in a mane.  Caller must know length.
*/
static inline void
_zn_free(u3_z z,
         u3_ray man_ray,
         c3_w   wor_w)
{
}

/* _zn_complete(): complete a calculation.
**
**   lid: destination of calculation
**   lam: result
*/
static inline void
_zn_complete(u3_z   z,
             u3_ray lid_ray,
             u3_fox lam)
{
  {
    c3_assert(u3_ray_beam(lid_ray) == u3_ray_beam(z->l.cap_ray));
    c3_assert(u3_ray_beam(lid_ray) == u3_ray_beam(z->l.mat_ray));
    c3_assert(lid_ray <= z->l.cap_ray);
    c3_assert(lid_ray >= z->l.mat_ray);
  }

  z->l.cap_ray = lid_ray;
  _zn_push_word(z, lam);
}


#define U3_ZN_FORGE
# include "op/bask.c"
# include "op/cons.c"
# include "op/nock.c"
# include "op/drop.c"
# include "op/fine.c"
# include "op/hint.c"
# include "op/jet.c"
# include "op/flac.c"
# include "op/mate.c"
# include "op/trol.c"
# include "op/gant.c"
# include "op/root.c"
# include "op/tail.c"
#undef U3_ZN_FORGE

/* uz_z_mung():
**
**   As uz_z_run(), but [b] is gate and [c] is sample.
*/
u3_fox
u3_z_mung(u3_z              z,
          u3_fox            *a,
          u3_fox            b,
          u3_fox            c,
          struct u3_z_bench *d)
{
  u3_fox gat = b;
  u3_fox sam = c;

  if ( u3_no == u3_lr_dust(z, gat) ) {
    return c3__exit;
  } else {
    u3_fox ham = u3_h(z, gat);
    u3_fox bat = u3_t(z, gat);

    if ( u3_no == u3_lr_dust(z, ham) ) {
      return c3__exit;
    } else {
      // XX: memory behavior not quite perfect.
      //
      u3_fox con = u3_t(z, ham);
      u3_fox sub = u3_ln_cell(z, u3_ln_cell(z, sam, con), bat);
      u3_fox fol = bat;
      
      return u3_z_run(z, a, sub, fol, d);
    }
  }
}

/* u3_z_run():
**
**   Execute (nock b c) and set *a to the result.  If this succeeds,
**   return 0.  Otherwise, return c3__exit or c3__fail.
**
**   If (d) is nonzero, set profiling data.
*/
u3_fox
u3_z_run(u3_z z,
         u3_fox            *a,
         u3_fox            b,
         u3_fox            c,
         struct u3_z_bench *d)
{
  /* Initialize benchmarks.
  */
  if ( d ) {
    d->ruy_d = 0;
    u3_lm_water(z, &d->vil_w, &d->tew_w);
    d->maz_w = d->vil_w;
    d->buc_w = d->tew_w;
  }

  /* Install initial instruction - or fire direct jet.
  */
  {
    enum u3_zj_code sax_code;

    if ( u3_zj_code_none != (sax_code = u3_zj_look(z, c)) ) {
      u3_mote bem = u3_zj_fire(z, a, sax_code, b);
      
      if ( bem != c3__punt ) {
        return bem;
      }
    } 
  }
  *a = u3_none;
  _zn_forge_fine(z, z->l.cap_ray);
  _zn_forge_nock(z, z->l.cap_ray, b, c);

  while ( 1 ) {
    /* Preamble - miscellaneous noncomputational operations.
    */
    {
      /* Test for memory exhaustion.  No agent can allocate more
      ** than 256 words without another check.
      **
      ** For faster performance, this should be per-operation.
      */
      if ( u3_no == u3_lm_open(z, 256) ) {
        c3_w maz_w, buc_w;

        u3_lm_water(z, &maz_w, &buc_w);
        printf("not open: maz %d, buc %d\n", maz_w, buc_w);
        return c3__fail;
      }

      /* Update benchmark statistics.
      */
      if ( d ) {
        c3_w nox_w, zur_w;

        u3_lm_water(z, &nox_w, &zur_w);
        d->maz_w = c3_max(d->maz_w, nox_w);
        d->buc_w = c3_max(d->buc_w, zur_w);

        d->ruy_d++;
      }
    }

    /* Body - main body of the loop.
    */
    {
      /* bip: agent.
      ** ger: operator.
      */
      u3_ray     bip_ray = z->n.lab_ray;
      u3_ray     poq_ray;
      u3_zn_oper ger_op;

      /* Unload opcode and following agent.
      */
        ger_op = *_zn_anvil(z, bip_ray, any, f.c.ger_op);
        poq_ray = *_zn_anvil(z, bip_ray, any, f.c.poq_ray);

      /* Remove this agent from the agenda.
      ** Pop it off the cap.
      */
        z->n.lab_ray = poq_ray;
        z->l.cap_ray = bip_ray;

      /* Load and execute the operation.
      */
      switch ( ger_op ) {
        default: c3_assert(0);

#       define U3_ZN_OP
#         include "op/bask.c"
#         include "op/cons.c"
#         include "op/nock.c"
#         include "op/drop.c"
#         include "op/fine.c"
#         include "op/hint.c"
#         include "op/jet.c"
#         include "op/flac.c"
#         include "op/mate.c"
#         include "op/trol.c"
#         include "op/gant.c"
#         include "op/root.c"
#         include "op/tail.c"
#       undef U3_ZN_OP 
      }
    }
  }
}
