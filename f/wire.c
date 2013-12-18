/* f/wire.c
**
** This file is in the public domain.
*/
#include "all.h"
#include <sys/stat.h>
#include <fcntl.h>

/* u2_wr_init():
**
**   Install an empty wire within `hat_r` and `mat_r` in the loom,
**   with memory model `hip`.
**
**   Returns ray to wire, which always equalls the passed `mat_r`.
*/
u2_ray
u2_wr_init(c3_m   hip_m,
           u2_ray hat_r,
           u2_ray mat_r)
{
  u2_ray wir_r;

  wir_r = u2_rl_init(hip_m, hat_r, mat_r);
  u2_rail_hat_r(wir_r) += (c3_wiseof(u2_loom_wire) - c3_wiseof(u2_loom_rail));

#if 1
  u2_wire_bas_r(wir_r) = 0;
#endif

  u2_wire_kit_r(wir_r) = 0;

  u2_cs_init(u2_wire_des_r(wir_r));

  /* Trace stack, garbage.
  */
  {
    u2_wire_tax(wir_r) = u2_nul;
    u2_wire_lan(wir_r) = u2_yes;
  }

  /* Permanent basket = 1/16 of address space.
  */
  {
    u2_ray bas_r;

    bas_r = u2_rl_leap_part(wir_r, c3__sand, 1, 16, 0);
    u2_wire_bas_r(wir_r) = bas_r;

#if 0
    fprintf(stderr, "bas_r %d, hat %d, mat %d, cap %d, rut %d\n",
        bas_r >> LoomPageWords,
        u2_rail_hat_r(bas_r) >> LoomPageWords,
        u2_rail_mat_r(bas_r) >> LoomPageWords,
        u2_rail_cap_r(bas_r) >> LoomPageWords,
        u2_rail_rut_r(bas_r) >> LoomPageWords);

    fprintf(stderr, "wir_r %d, hat %d, mat %d, cap %d, rut %d\n",
        wir_r >> LoomPageWords,
        u2_rail_hat_r(wir_r) >> LoomPageWords,
        u2_rail_mat_r(wir_r) >> LoomPageWords,
        u2_rail_cap_r(wir_r) >> LoomPageWords,
        u2_rail_rut_r(wir_r) >> LoomPageWords);
#endif

    // u2_ba_init(wir_r, 0);
  }

#if 1
  /* Host control.
  */
  {
    u2_ho_push();
  }
#endif

  /* Basic performance tracking.
  */
  {
    u2_wire_bex_r(wir_r) = u2_rl_ralloc(wir_r, c3_wiseof(u2_loom_benx));

    u2_bx_boot(wir_r);
  }

  /* New performance tracking.
  */
  {
    u2_wire_rac_r(wir_r) = u2_tx_init(wir_r);
  }

  /* Global namespace.
  */
  {
    u2_wire_hev_r(wir_r) = u2_hv_init(wir_r);
  }

  /* OS kernel.
  */
  {
    u2_wire_arv_r(wir_r) = u2_rl_ralloc(wir_r, c3_wiseof(u2_reck));
  }

  return wir_r;
}

/* _wr_open(): open checkpoint file, or return null.
*/
static c3_i
_wr_open(c3_c* cpu_c, c3_c* fil_c, c3_c* suf_c, c3_w len_w)
{
  c3_c ful_c[8193];
  c3_i fid_i;

  snprintf(ful_c, 8193, "%s", cpu_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8193, "%s/chk", cpu_c);
  mkdir(ful_c, 0700);

  snprintf(ful_c, 8193, "%s/chk/%s.%s", cpu_c, fil_c, suf_c);
  fid_i = open(ful_c, O_RDWR | O_CREAT, 0666);
  if ( -1 == fid_i ) {
    perror(ful_c); exit(1);
  }
  if ( len_w &&
       (-1 == ftruncate(fid_i, (len_w * (1 << (LoomPageWords + 2))))) )
  {
    perror(ful_c); exit(1);
  }
  return fid_i;
}

/* u2_wr_check_init(): initialize checkpoint segments and/or files.
*/
void
u2_wr_check_init(c3_c* cpu_c)
{
  //  Segment A, low memory.
  //
  {
    LoomSegmentA.bot_w = 2048;
    LoomSegmentA.len_w = 30720;
    LoomSegmentA.pgs_w = 0;
    LoomSegmentA.ctl_i = _wr_open(cpu_c, "a", "ctl", 0);
    LoomSegmentA.dat_i = _wr_open(cpu_c, "a", "dat", LoomSegmentA.len_w);
  }

  //  Segment B, high memory.
  //
  {
    LoomSegmentB.bot_w = LoomHalfPages;
    LoomSegmentB.len_w = 30719;
    LoomSegmentB.pgs_w = 0;
    LoomSegmentB.ctl_i = _wr_open(cpu_c, "b", "ctl", 0);
    LoomSegmentB.dat_i = _wr_open(cpu_c, "b", "dat", LoomSegmentB.len_w);
  }

  //  Segment C, the basket control block.  Ugly.
  {
    LoomSegmentC.bot_w = 63487;
    LoomSegmentC.len_w = 1;
    LoomSegmentC.pgs_w = 0;
    LoomSegmentC.ctl_i = _wr_open(cpu_c, "c", "ctl", 0);
    LoomSegmentC.dat_i = _wr_open(cpu_c, "c", "dat", LoomSegmentC.len_w);
  }

  //  Segment D, the actual basket.
  {
    LoomSegmentD.bot_w = 0;
    LoomSegmentD.len_w = 2048;
    LoomSegmentD.pgs_w = 0;
    LoomSegmentD.ctl_i = _wr_open(cpu_c, "d", "ctl", 0);
    LoomSegmentD.dat_i = _wr_open(cpu_c, "d", "dat", LoomSegmentD.len_w);
  }
}

static void
_wr_check_cheg(u2_cheg* ceg_u, u2_ray top_r)
{
  c3_w top_w = (top_r + ((1 << LoomPageWords) - 1)) >> LoomPageWords;

  c3_assert(top_w >= ceg_u->bot_w);
  c3_assert(top_w < (ceg_u->bot_w + ceg_u->len_w));

  if ( ceg_u->pgs_w > (top_w - ceg_u->bot_w) ) {
    ceg_u->pgs_w = (top_w - ceg_u->bot_w);
  }
}

/* u2_wr_check_save(): checkpoint wire in global structure.
*/
void
u2_wr_check_save()
{
  u2_ray mat_r = u2_rail_mat_r(u2_Wire);
  u2_ray hat_r = u2_rail_hat_r(u2_Wire);

  if ( hat_r >= HalfSize ) {
    _wr_check_cheg(&LoomSegmentB, hat_r);
    _wr_check_cheg(&LoomSegmentA, mat_r);
  } else {
    _wr_check_cheg(&LoomSegmentA, hat_r);
    _wr_check_cheg(&LoomSegmentB, mat_r);
  }
#if 0
  _wr_check_cheg(&LoomSegmentC, u2_rail_hat_r(bas_r));
#endif
}

/* u2_wr_ice(): u2_rl_ice(), with u2_bx_copy().
*/
u2_weak
u2_wr_ice(u2_ray  wir_r,
          u2_noun fiz)
{
  u2_ray  hat_r = u2_rail_hat_r(wir_r);
  u2_noun buz;
  c3_w    cop_w;

  buz = u2_rl_ice(wir_r, fiz);

  cop_w = u2_rail_hat_r(wir_r) - hat_r;
  if ( cop_w ) {
    u2_bx_copy(wir_r, cop_w);
  }
  return buz;
}

/* u2_wr_mark():
**
**   Mark all roots in a wire and return their allocation size.
*/
c3_w
u2_wr_mark(u2_ray wir_r)
{
  c3_w siz_w = 0;

  c3_assert(c3__rock == u2_rail_hip_m(wir_r));
  {
    {
      u2_ray kit_r = u2_wire_kit_r(wir_r);

      while ( kit_r ) {
        siz_w += u2_rl_gc_mark_ptr(wir_r, u2_wire_kit_r(wir_r));
        siz_w += u2_rl_gc_mark_noun(wir_r, u2_kite_tax(kit_r));
        siz_w += u2_rl_gc_mark_noun(wir_r, u2_kite_don(kit_r));

        kit_r = u2_kite_par_r(kit_r);
      }
    }
    siz_w += u2_rl_gc_mark_ptr(wir_r, u2_wire_bex_r(wir_r));
    siz_w += u2_rl_gc_mark_ptr(wir_r, u2_wire_rac_r(wir_r));

    siz_w += u2_rl_gc_mark_ptr(wir_r, u2_wire_hev_r(wir_r));
    siz_w += u2_rl_gc_mark_ptr(wir_r, u2_wire_arv_r(wir_r));
    u2_hv_mark();
  }
  siz_w += u2_rl_gc_mark(wir_r);

  return siz_w;
}
