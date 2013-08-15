/* f/cash.c
** 
** This file is in the public domain.
*/
#include "all.h"

  /** Forward declarations.
  **/
    static u2_bean
    _cs_save_in(u2_rail, u2_ray, c3_w, c3_w, c3_m, u2_noun, u2_noun);

    static u2_bean
    _cs_save_b(u2_rail, u2_ray, c3_w, c3_w, c3_m, u2_noun, u2_noun);

/* _cs_find_sap(): check if sample matches list; if not, add.
*/
static u2_bean
_cs_find_sap(u2_rail ral_r,
             u2_ray  lot_r,
             u2_noun sam)                                         //  retain
{
  u2_noun sap = u2_slot_a_sap(lot_r);
  {
    u2_noun pos = sap;

    while ( u2_nul != pos ) {
      if ( u2_yes == u2_fing(sam, u2_h(pos)) ) {
        return u2_yes;
      }
      pos = u2_t(pos);
    }
  }
  if ( u2_yes == u2_sing(sam, u2_h(sap)) ) {
    if ( u2_no == u2_rl_junior(ral_r, sam) ) {
      u2_slot_a_sap(lot_r) = u2_rc(ral_r, u2_rx(ral_r, sam), sap);
    }
    return u2_yes;
  }
  else return u2_no;
}

/* _cs_find_sap_cell(): _cs_find_sap() for cell.
*/
static u2_bean
_cs_find_sap_cell(u2_rail ral_r,
                  u2_ray  lot_r,
                  u2_noun a_sam,                                  //  retain
                  u2_noun b_sam)                                  //  retain
{
  u2_noun sap = u2_slot_a_sap(lot_r);
  {
    u2_noun pos = sap;

    while ( u2_nul != pos ) {
      if ( u2_yes == u2_fing_cell(a_sam, b_sam, u2_h(pos)) ) {
        return u2_yes;
      }
      pos = u2_t(pos);
    }
  }
  if ( u2_yes == u2_sing_cell(a_sam, b_sam, u2_h(sap)) ) {
    if ( (u2_no == u2_rl_junior(ral_r, a_sam)) &&
         (u2_no == u2_rl_junior(ral_r, b_sam)) )
    {
      u2_noun sam = u2_rc(ral_r, u2_rx(ral_r, a_sam),
                                 u2_rx(ral_r, b_sam));

      if ( u2_none != sam ) {
        u2_slot_a_sap(lot_r) = u2_rc(ral_r, sam, sap);
      }
    }
    return u2_yes;
  }
  else return u2_no;
}

/* _cs_find_sap_mixt(): _cs_find_sap() for mixed cell.
*/
static u2_bean
_cs_find_sap_mixt(u2_rail     ral_r,
                  u2_ray      lot_r,
                  const c3_c* a_sam_c,                            //  retain
                  u2_noun     b_sam)                              //  retain
{
  u2_noun sap = u2_slot_a_sap(lot_r);
  {
    u2_noun pos = sap;

    while ( u2_nul != pos ) {
      if ( u2_yes == u2_fing_mixt(a_sam_c, b_sam, u2_h(pos)) ) {
        return u2_yes;
      }
      pos = u2_t(pos);
    }
  }
  if ( u2_yes == u2_sing_mixt(a_sam_c, b_sam, u2_h(sap)) ) {
    if ( u2_no == u2_rl_junior(ral_r, b_sam) ) {
      u2_noun sam = u2_rc(ral_r, u2_rl_string(ral_r, a_sam_c),
                                 u2_rx(ral_r, b_sam));

      if ( u2_none != sam ) {
        u2_slot_a_sap(lot_r) = u2_rc(ral_r, sam, sap);
      }
    }
    return u2_yes;
  }
  else return u2_no;
}

/* _cs_find_sap_trel(): _cs_find_sap() for cell.
*/
static u2_bean
_cs_find_sap_trel(u2_rail ral_r,
                  u2_ray  lot_r,
                  u2_noun a_sam,                                  //  retain
                  u2_noun b_sam,                                  //  retain
                  u2_noun c_sam)                                  //  retain
{
  u2_noun sap = u2_slot_a_sap(lot_r);
  {
    u2_noun pos = sap;

    while ( u2_nul != pos ) {
      if ( u2_yes == u2_fing_trel(a_sam, b_sam, c_sam, u2_h(pos)) ) {
        return u2_yes;
      }
      pos = u2_t(pos);
    }
  }
  if ( u2_yes == u2_sing_trel(a_sam, b_sam, c_sam, u2_h(sap)) ) {
    if ( (u2_no == u2_rl_junior(ral_r, a_sam)) &&
         (u2_no == u2_rl_junior(ral_r, b_sam)) &&
         (u2_no == u2_rl_junior(ral_r, c_sam)) )
    {
      u2_noun sam = u2_rt(ral_r, u2_rx(ral_r, a_sam),
                                 u2_rx(ral_r, b_sam),
                                 u2_rx(ral_r, c_sam));

      if ( u2_none != sam ) {
        u2_slot_a_sap(lot_r) = u2_rc(ral_r, sam, sap);
      }
    }
    return u2_yes;
  }
  else return u2_no;
}

/* _cs_find_sap_qual(): _cs_find_sap() for cell.
*/
static u2_bean
_cs_find_sap_qual(u2_rail ral_r,
                  u2_ray  lot_r,
                  u2_noun a_sam,                                  //  retain
                  u2_noun b_sam,                                  //  retain
                  u2_noun c_sam,                                  //  retain
                  u2_noun d_sam)                                  //  retain
{
  u2_noun sap = u2_slot_a_sap(lot_r);
  {
    u2_noun pos = sap;

    while ( u2_nul != pos ) {
      if ( u2_yes == u2_fing_qual(a_sam, b_sam, c_sam, d_sam, u2_h(pos)) ) {
        return u2_yes;
      }
      pos = u2_t(pos);
    }
  }
  if ( u2_yes == u2_sing_qual(a_sam, b_sam, c_sam, d_sam, u2_h(sap)) ) {
    if ( (u2_no == u2_rl_junior(ral_r, a_sam)) &&
         (u2_no == u2_rl_junior(ral_r, b_sam)) &&
         (u2_no == u2_rl_junior(ral_r, c_sam)) &&
         (u2_no == u2_rl_junior(ral_r, d_sam)) )
    {
      u2_noun sam = u2_rq(ral_r, u2_rx(ral_r, a_sam),
                                 u2_rx(ral_r, b_sam),
                                 u2_rx(ral_r, c_sam),
                                 u2_rx(ral_r, d_sam));

      if ( u2_none != sam ) {
        u2_slot_a_sap(lot_r) = u2_rc(ral_r, sam, sap);
      }
    }
    return u2_yes;
  }
  else return u2_no;
}

/* _cs_find_1()::
*/
static u2_weak                                                    //  retain
_cs_find_1(u2_rail ral_r,
           u2_ray  lot_r,
           c3_w    key_w,
           c3_w    sif_w,
           c3_m    sel_m,
           u2_noun sam)                                           //  retain
{
  top: {
    if ( u2_slot_is_a(lot_r) ) {
      if ( (sel_m == u2_slot_a_sel(lot_r)) &&
           (u2_yes == _cs_find_sap(ral_r, lot_r, sam)) )
      {
        return u2_slot_a_pro(lot_r);
      } else {
        return u2_none;
      }
    }
    else if ( u2_slot_is_c(lot_r) ) {
      return u2_none;
    }
    else {
      c3_w gun_w = u2_slot_b_gun(lot_r);
      c3_w i_w = (key_w >> sif_w) & 15;

      if ( u2_slot_gunk_is_coll(gun_w) ) {
        c3_w j_w = i_w;

        do {
          u2_ray tol_r = u2_slot_b_sid_i(lot_r, j_w);

          if ( u2_slot_is_a(tol_r) &&
              (sel_m == u2_slot_a_sel(tol_r)) &&
              (u2_yes == _cs_find_sap(ral_r, tol_r, sam)) ) 
          {
            return u2_slot_a_pro(tol_r);
          }

          j_w = ((j_w + 1) & 15);
        }
        while ( j_w != i_w );

        return u2_none;
      } 
      else {
        lot_r = u2_slot_b_sid_i(lot_r, i_w);
        sif_w += 4;
        goto top;
      }
    }
  }
}

/* _cs_find_2()::
*/
static u2_weak                                                    //  retain
_cs_find_2(u2_rail ral_r,
           u2_ray  lot_r,
           c3_w    key_w,
           c3_w    sif_w,
           c3_m    sel_m,
           u2_noun a,                                             //  retain
           u2_noun b)                                             //  retain
{
  top: {
    if ( u2_slot_is_a(lot_r) ) {
      if ( (sel_m == u2_slot_a_sel(lot_r)) &&
           (u2_yes == _cs_find_sap_cell(ral_r, lot_r, a, b)) )
      {
        return u2_slot_a_pro(lot_r);
      } else {
        return u2_none;
      }
    }
    else if ( u2_slot_is_c(lot_r) ) {
      return u2_none;
    }
    else {
      c3_w gun_w = u2_slot_b_gun(lot_r);
      c3_w i_w = (key_w >> sif_w) & 15;

      if ( u2_slot_gunk_is_coll(gun_w) ) {
        c3_w j_w = i_w;

        do {
          u2_ray tol_r = u2_slot_b_sid_i(lot_r, j_w);

          if ( u2_slot_is_a(tol_r) &&
              (sel_m == u2_slot_a_sel(tol_r)) &&
              (u2_yes == _cs_find_sap_cell(ral_r, tol_r, a, b)) ) 
          {
            return u2_slot_a_pro(tol_r);
          }
          
          j_w = ((j_w + 1) & 15);
        }
        while ( j_w != i_w );

        return u2_none;
      } 
      else {
        lot_r = u2_slot_b_sid_i(lot_r, i_w);
        sif_w += 4;
        goto top;
      }
    }
  }
}

/* _cs_find_2m()::
*/
static u2_weak                                                    //  retain
_cs_find_2m(u2_rail     ral_r,
            u2_ray      lot_r,
            c3_w        key_w,
            c3_w        sif_w,
            c3_m        sel_m,
            const c3_c* a_c,                                      //  retain
            u2_noun     b)                                        //  retain
{
  top: {
    if ( u2_slot_is_a(lot_r) ) {
      if ( (sel_m == u2_slot_a_sel(lot_r)) &&
           (u2_yes == _cs_find_sap_mixt(ral_r, lot_r, a_c, b)) )
      {
        return u2_slot_a_pro(lot_r);
      } else {
        return u2_none;
      }
    }
    else if ( u2_slot_is_c(lot_r) ) {
      return u2_none;
    }
    else {
      c3_w gun_w = u2_slot_b_gun(lot_r);
      c3_w i_w = (key_w >> sif_w) & 15;

      if ( u2_slot_gunk_is_coll(gun_w) ) {
        c3_w j_w = i_w;

        do {
          u2_ray tol_r = u2_slot_b_sid_i(lot_r, j_w);

          if ( u2_slot_is_a(tol_r) &&
              (sel_m == u2_slot_a_sel(tol_r)) &&
              (u2_yes == _cs_find_sap_mixt(ral_r, tol_r, a_c, b)) )
          {
            return u2_slot_a_pro(tol_r);
          }
          
          j_w = ((j_w + 1) & 15);
        }
        while ( j_w != i_w );

        return u2_none;
      } 
      else {
        lot_r = u2_slot_b_sid_i(lot_r, i_w);
        sif_w += 4;
        goto top;
      }
    }
  }
}

/* _cs_find_3()::
*/
static u2_weak                                                    //  retain
_cs_find_3(u2_rail ral_r,
           u2_ray  lot_r,
           c3_w    key_w,
           c3_w    sif_w,
           c3_m    sel_m,
           u2_noun a,                                             //  retain
           u2_noun b,                                             //  retain
           u2_noun c)                                             //  retain
{
  top: {
    if ( u2_slot_is_a(lot_r) ) {
      if ( (sel_m == u2_slot_a_sel(lot_r)) &&
           (u2_yes == _cs_find_sap_trel(ral_r, lot_r, a, b, c) ) )
      {
        return u2_slot_a_pro(lot_r);
      } else {
        return u2_none;
      }
    }
    else if ( u2_slot_is_c(lot_r) ) {
      return u2_none;
    }
    else {
      c3_w gun_w = u2_slot_b_gun(lot_r);
      c3_w i_w = (key_w >> sif_w) & 15;

      if ( u2_slot_gunk_is_coll(gun_w) ) {
        c3_w j_w = i_w;

        do {
          u2_ray tol_r = u2_slot_b_sid_i(lot_r, j_w);

          if ( u2_slot_is_a(tol_r) &&
              (sel_m == u2_slot_a_sel(tol_r)) &&
              (u2_yes == _cs_find_sap_trel(ral_r, tol_r, a, b, c) ) )
          {
            return u2_slot_a_pro(tol_r);
          }
          
          j_w = ((j_w + 1) & 15);
        }
        while ( j_w != i_w );

        return u2_none;
      } 
      else {
        lot_r = u2_slot_b_sid_i(lot_r, i_w);
        sif_w += 4;
        goto top;
      }
    }
  }
}

/* _cs_find_4()::
*/
static u2_weak                                                    //  retain
_cs_find_4(u2_rail ral_r,
           u2_ray  lot_r,
           c3_w    key_w,
           c3_w    sif_w,
           c3_m    sel_m,
           u2_noun a,                                             //  retain
           u2_noun b,                                             //  retain
           u2_noun c,                                             //  retain
           u2_noun d)                                             //  retain
{
  top: {
    if ( u2_slot_is_a(lot_r) ) {
      if ( (sel_m == u2_slot_a_sel(lot_r)) &&
           (u2_yes == _cs_find_sap_qual(ral_r, lot_r, a, b, c, d) ) )
      {
        return u2_slot_a_pro(lot_r);
      } else {
        return u2_none;
      }
    }
    else if ( u2_slot_is_c(lot_r) ) {
      return u2_none;
    }
    else {
      c3_w gun_w = u2_slot_b_gun(lot_r);
      c3_w i_w = (key_w >> sif_w) & 15;

      if ( u2_slot_gunk_is_coll(gun_w) ) {
        c3_w j_w = i_w;

        do {
          u2_ray tol_r = u2_slot_b_sid_i(lot_r, j_w);

          if ( u2_slot_is_a(tol_r) &&
              (sel_m == u2_slot_a_sel(tol_r)) &&
              (u2_yes == _cs_find_sap_qual(ral_r, tol_r, a, b, c, d) ) )
          {
            return u2_slot_a_pro(tol_r);
          }
          
          j_w = ((j_w + 1) & 15);
        }
        while ( j_w != i_w );

        return u2_none;
      } 
      else {
        lot_r = u2_slot_b_sid_i(lot_r, i_w);
        sif_w += 4;
        goto top;
      }
    }
  }
}

/* u2_cs_find():
**
**   Find `sam` for `sel`, or return `u2_none`.
*/
u2_weak                                                           //  retain
u2_cs_find(u2_rail ral_r,
           u2_ray  lot_r,
           c3_m    sel_m,
           u2_noun sam)                                           //  retain
{
  c3_w key_w = u2_mug(sel_m) ^ u2_mug(sam);

  return _cs_find_1(ral_r, lot_r, key_w, 0, sel_m, sam);
}

/* u2_cs_find_cell():
**
**   Find `[a b]` for `sel`, or return `u2_none`.
*/
u2_weak                                                           //  retain
u2_cs_find_cell(u2_rail ral_r,
                u2_ray  lot_r,
                c3_m    sel_m,
                u2_noun a,                                        //  retain
                u2_noun b)                                        //  retain
{
  c3_w key_w = u2_mug(sel_m) ^ u2_mug_cell(a, b);

  return _cs_find_2(ral_r, lot_r, key_w, 0, sel_m, a, b);
}

/* u2_cs_find_mixt():
**
**   Find `[a b]` for `sel`, or return `u2_none`.
*/
u2_weak                                                     //  retain
u2_cs_find_mixt(u2_rail     ral_r,
                u2_ray      lot_r,
                c3_m        sel_m,
                const c3_c* a_c,                            //  retain
                u2_noun     b)                              //  retain
{
  c3_w mug_w = u2_mug_both(u2_mug_string(a_c), u2_mug(b));
  c3_w key_w = u2_mug(sel_m) ^ mug_w;

  return _cs_find_2m(ral_r, lot_r, key_w, 0, sel_m, a_c, b);
}

/* u2_cs_find_trel():
**
**   Find `[a b]` for `sel`, or return `u2_none`.
*/
u2_weak                                                           //  retain
u2_cs_find_trel(u2_rail ral_r,
                u2_ray  lot_r,
                c3_m    sel_m,
                u2_noun a,                                        //  retain
                u2_noun b,                                        //  retain
                u2_noun c)                                        //  retain
{
  c3_w key_w = u2_mug(sel_m) ^ u2_mug_trel(a, b, c);

  return _cs_find_3(ral_r, lot_r, key_w, 0, sel_m, a, b, c);
}

/* u2_cs_find_qual():
**
**   Find `[a b]` for `sel`, or return `u2_none`.
*/
u2_weak                                                           //  retain
u2_cs_find_qual(u2_rail ral_r,
                u2_ray  lot_r,
                c3_m    sel_m,
                u2_noun a,                                        //  retain
                u2_noun b,                                        //  retain
                u2_noun c,                                        //  retain
                u2_noun d)                                        //  retain
{
  c3_w key_w = u2_mug(sel_m) ^ u2_mug_qual(a, b, c, d);

  return _cs_find_4(ral_r, lot_r, key_w, 0, sel_m, a, b, c, d);
}

/* _cs_save_c(): add to slot of type c.
*/
static u2_bean
_cs_save_c(u2_rail ral_r,
           u2_ray  lot_r,
           c3_m    sel_m,
           u2_noun sap,                                           //  retain
           u2_noun pro)                                           //  retain
{
  u2_slot_a_sel(lot_r) = sel_m;
  u2_slot_a_sap(lot_r) = u2_rx(ral_r, sap);
  u2_slot_a_pro(lot_r) = u2_rx(ral_r, pro);

  return u2_yes;
}

/* _cs_more_b(): convert slot of type b from collision to radix.
*/
static void
_cs_more_b(u2_rail ral_r,
           u2_ray  lot_r,
           c3_w    sif_w)
{
  u2_ray dis_r = u2_rl_ralloc(ral_r, (16 * c3_wiseof(u2_cash_slot_a)));
  u2_ray sid_r = u2_slot_b_sid(lot_r);
  c3_w   i_w;

  if ( 0 == dis_r ) {
    return;
  }
  u2_slot_b_gun(lot_r) = u2_slot_gunk_radx;
  u2_slot_b_rag(lot_r) = 0;
  u2_slot_b_sid(lot_r) = dis_r;

  for ( i_w = 0; i_w < 16; i_w++ ) {
    u2_ray tol_r = (dis_r + ((i_w) * c3_wiseof(u2_cash_slot_a)));

    u2_slot_c_emt(tol_r) = u2_slot_emty;
  }

  for ( i_w = 0; i_w < 16; i_w++ ) {
    u2_ray tol_r = (sid_r + ((i_w) * c3_wiseof(u2_cash_slot_a)));

    if ( u2_slot_is_a(tol_r) ) {
      c3_m    sel_m = u2_slot_a_sel(tol_r);
      u2_noun sap   = u2_slot_a_sap(tol_r);
      u2_noun pro   = u2_slot_a_pro(tol_r);
      c3_w    key_w = u2_mug(sel_m) ^ u2_mug(u2_h(sap));

      _cs_save_b(ral_r, lot_r, key_w, sif_w, sel_m, sap, pro);

      u2_rz(ral_r, sap);
      u2_rz(ral_r, pro);
    }
  }
  u2_rl_rfree(ral_r, sid_r);
}

/* _cs_save_b(): add to slot of type b.
*/
static u2_bean
_cs_save_b(u2_rail ral_r,
           u2_ray  lot_r,
           c3_w    key_w,
           c3_w    sif_w,
           c3_m    sel_m,
           u2_noun sap,                                           //  retain
           u2_noun pro)                                           //  retain
{
  top: {
    c3_w gun_w = u2_slot_b_gun(lot_r);
    c3_w i_w = (key_w >> sif_w) & 15;

    if ( u2_slot_gunk_is_coll(gun_w) ) {
      if ( (u2_slot_b_rag(lot_r) >= u2_tune_cash_hi) &&
           (sif_w != 32) ) 
      {
        _cs_more_b(ral_r, lot_r, sif_w);
        goto top;
      }
      else {
        c3_w j_w = i_w;

        do {
          u2_ray tol_r = u2_slot_b_sid_i(lot_r, j_w);

          if ( u2_slot_is_a(tol_r) ) {
            if ( (sel_m == u2_slot_a_sel(tol_r)) &&
                 (u2_yes == _cs_find_sap(ral_r, tol_r, u2_h(sap))) )
            {
              if ( u2_no == u2_sing(pro, u2_slot_a_pro(tol_r)) ) {
                u2_rz(ral_r, u2_slot_a_pro(tol_r));
                u2_slot_a_pro(tol_r) = u2_rx(ral_r, pro);
              }
              return u2_no;
            }
          }
          else if ( u2_slot_is_c(tol_r) ) {
            u2_slot_b_rag(lot_r) += 1;

            return _cs_save_c(ral_r, tol_r, sel_m, sap, pro);
          }
          j_w = ((j_w + 1) & 15);
        }
        while ( j_w != i_w );

        return u2_no;
      }
    } else {
      u2_bean ave = _cs_save_in(ral_r, 
                                u2_slot_b_sid_i(lot_r, i_w), 
                                key_w, 
                                (sif_w + 4), 
                                sel_m, sap, pro);
      if ( u2_yes == ave ) {
        u2_slot_b_rag(lot_r) += 1;
      }
      return ave;
    }
  }
}

/* _cs_save_a(): add to slot of type a.
*/
static u2_bean
_cs_save_a(u2_rail ral_r,
           u2_ray  lot_r,
           c3_w    key_w,
           c3_w    sif_w,
           c3_m    sel_m,
           u2_noun sap,                                           //  retain
           u2_noun pro)                                           //  retain
{
  if ( (sel_m == u2_slot_a_sel(lot_r)) &&
       (u2_yes == _cs_find_sap(ral_r, lot_r, u2_h(sap)) ) ) 
  {
    if ( u2_no == u2_sing(pro, u2_slot_a_pro(lot_r)) ) {
      u2_rz(ral_r, u2_slot_a_pro(lot_r));
      u2_slot_a_pro(lot_r) = u2_rx(ral_r, pro);
    }
    return u2_no;
  } else {
    u2_noun sid_r = u2_rl_ralloc(ral_r, 16 * c3_wiseof(u2_cash_slot_a));

    if ( 0 == sid_r ) {
      return u2_no;
    } else {
      c3_m lus_m  = u2_slot_a_sel(lot_r);
      u2_noun pes = u2_slot_a_sap(lot_r);
      u2_noun rop = u2_slot_a_pro(lot_r);
      c3_w yek_w  = u2_mug(lus_m) ^ u2_mug(u2_h(pes));
      u2_bean ave;

      u2_slot_b_gun(lot_r) = u2_slot_gunk_coll;
      u2_slot_b_rag(lot_r) = 0;
      u2_slot_b_sid(lot_r) = sid_r;

      {
        c3_w i_w;

        for ( i_w = 0; i_w < 16; i_w++ ) {
          u2_ray tol_r = u2_slot_b_sid_i(lot_r, i_w);

          u2_slot_c_emt(tol_r) = u2_slot_emty;
          u2_slot_b_rag(tol_r) = 0;
          u2_slot_b_sid(tol_r) = 0;
        }
      }

      {
        ave = _cs_save_b(ral_r, lot_r, yek_w, sif_w, lus_m, pes, rop);
        c3_assert(u2_yes == ave);

        u2_rz(ral_r, pes);
        u2_rz(ral_r, rop);
      }

      return _cs_save_b(ral_r, lot_r, key_w, sif_w, sel_m, sap, pro);
    }
  }
}

/* _cs_save_in():
**
**   As u2_cs_save(), but `u2_yes` iff table adds a sample.
*/
static u2_bean
_cs_save_in(u2_rail ral_r,
            u2_ray  lot_r,
            c3_w    key_w,
            c3_w    sif_w,
            c3_m    sel_m,
            u2_noun sap,                                          //  retain
            u2_noun pro)                                          //  retain
{
  if ( u2_slot_is_a(lot_r) ) {
    return _cs_save_a(ral_r, lot_r, key_w, sif_w, sel_m, sap, pro);
  }
  else if ( u2_slot_is_b(lot_r) ) {
    return _cs_save_b(ral_r, lot_r, key_w, sif_w, sel_m, sap, pro);
  }
  else {
    return _cs_save_c(ral_r, lot_r, sel_m, sap, pro);
  }
}

/* u2_cs_save():
**
**   Save `sam` as `pro` for `sel`.  Replace existing `pro`, if any.
*/
u2_noun                                                           //  transfer
u2_cs_save(u2_rail ral_r,
           u2_ray  lot_r,
           c3_m    sel_m,
           u2_noun sam,                                           //  retain
           u2_noun pro)                                           //  transfer
{
  c3_w key_w = u2_mug(sel_m) ^ u2_mug(sam);
  c3_w sif_w = 0;
  u2_noun sap = u2_rc(ral_r, u2_rx(ral_r, sam), u2_nul);

  _cs_save_in(ral_r, lot_r, key_w, sif_w, sel_m, sap, pro);

  u2_rz(ral_r, sap);
  return pro;
}

/* u2_cs_save_mixt():
**
**   Save `[a b]` as `pro` for `sel`.
*/
u2_noun                                                           //  transfer
u2_cs_save_mixt(u2_ray      ral_r,
                u2_ray      lot_r,
                c3_m        sel_m,
                const c3_c* a_c,                                  //  retain
                u2_noun     b,                                    //  retain
                u2_noun     pro)                                  //  transfer
{
  u2_noun sam = u2_rc(ral_r, u2_rl_string(ral_r, a_c), u2_rx(ral_r, b));
  u2_noun ret = u2_cs_save(ral_r, lot_r, sel_m, sam, pro);

  u2_rz(ral_r, sam);
  return ret;
}

/* u2_cs_save_cell():
**
**   Save `[a b]` as `pro` for `sel`.
*/
u2_noun                                                           //  transfer
u2_cs_save_cell(u2_rail ral_r,
                u2_ray  lot_r,
                c3_m    sel_m,
                u2_noun a,                                        //  retain
                u2_noun b,                                        //  retain
                u2_noun pro)                                      //  transfer
{
  u2_noun sam = u2_rc(ral_r, u2_rx(ral_r, a), u2_rx(ral_r, b));
  u2_noun ret = u2_cs_save(ral_r, lot_r, sel_m, sam, pro);

  u2_rz(ral_r, sam);
  return ret;
}

/* u2_cs_save_trel():
**
**   Save `[a b c]` as `pro` for `sel`.
*/
u2_noun                                                           //  transfer
u2_cs_save_trel(u2_rail ral_r,
                u2_ray  lot_r,
                c3_m    sel_m,
                u2_noun a,                                        //  retain
                u2_noun b,                                        //  retain
                u2_noun c,                                        //  retain
                u2_noun pro)                                      //  transfer
{
  u2_noun sam = u2_rt(ral_r, u2_rx(ral_r, a), u2_rx(ral_r, b), u2_rx(ral_r, c));
  u2_noun ret = u2_cs_save(ral_r, lot_r, sel_m, sam, pro);

  u2_rz(ral_r, sam);
  return ret;
}

/* u2_cs_save_qual():
**
**   Save `[a b c d]` as `pro` for `sel`.
*/
u2_noun                                                           //  transfer
u2_cs_save_qual(u2_rail ral_r,
                u2_ray  lot_r,
                c3_m    sel_m,
                u2_noun a,                                        //  retain
                u2_noun b,                                        //  retain
                u2_noun c,                                        //  retain
                u2_noun d,                                        //  retain
                u2_noun pro)                                      //  transfer
{
  u2_noun sam = u2_rq(ral_r, u2_rx(ral_r, a), 
                             u2_rx(ral_r, b), 
                             u2_rx(ral_r, c),
                             u2_rx(ral_r, d));
  u2_noun ret = u2_cs_save(ral_r, lot_r, sel_m, sam, pro);

  u2_rz(ral_r, sam);
  return ret;
}

/* u2_cs_free():
**
**   Release an old hashtable.
*/
void
u2_cs_free(u2_rail ral_r,
           u2_ray  lot_r)                                         //  submit
{
  u2_cs_lose(ral_r, lot_r);
  u2_rl_rfree(ral_r, lot_r);
}

/* u2_cs_mark():
**
**   Mark traverse of slot.
*/
c3_w
u2_cs_mark(u2_ray ral_r,
           u2_ray lot_r)
{
  c3_w siz_w = 0;

  if ( u2_slot_is_a(lot_r) ) {
    siz_w += u2_rl_gc_mark_noun(ral_r, u2_slot_a_sap(lot_r));
    siz_w += u2_rl_gc_mark_noun(ral_r, u2_slot_a_pro(lot_r));
  }
  else if ( u2_slot_is_b(lot_r) ) {
    u2_ray sid_r = u2_slot_b_sid(lot_r);
    c3_w   i_w;

    for ( i_w = 0; i_w < 16; i_w++ ) {
      u2_ray tol_r = (sid_r + ((i_w) * c3_wiseof(u2_cash_slot_a)));

      siz_w += u2_cs_mark(ral_r, tol_r);
    }
    siz_w += u2_rl_gc_mark_ptr(ral_r, sid_r);
  }
  return siz_w;
}

/* u2_cs_init():
**
**  Initialize slot to empty.
*/
void
u2_cs_init(u2_ray lot_r)
{
  u2_slot_c_emt(lot_r) = u2_slot_emty;
}

/* u2_cs_lose():
**
**   Release all resources in and under slot (but not slot itself).
*/
void
u2_cs_lose(u2_rail ral_r,
           u2_ray lot_r)                                          //  submit
{
  if ( u2_slot_is_a(lot_r) ) {
    u2_rz(ral_r, u2_slot_a_sap(lot_r));
    u2_rz(ral_r, u2_slot_a_pro(lot_r));
  }
  else if ( u2_slot_is_b(lot_r) ) {
    u2_ray sid_r = u2_slot_b_sid(lot_r);
    c3_w   i_w;

    for ( i_w = 0; i_w < 16; i_w++ ) {
      u2_ray tol_r = (sid_r + ((i_w) * c3_wiseof(u2_cash_slot_a)));

      u2_cs_lose(ral_r, tol_r);
    }
    u2_rl_rfree(ral_r, sid_r);
  }
  u2_cs_init(lot_r);
  c3_assert(u2_slot_is_c(lot_r));
}

/* u2_cs_make():
**
**  Create a new hashtable.
*/
u2_ray                                                            //  produce
u2_cs_make(u2_rail ral_r)
{
  u2_ray lot_r = u2_rl_ralloc(ral_r, c3_wiseof(u2_cash_slot_a));

  if ( 0 == lot_r ) {
    return 0;
  } else {
    u2_cs_init(lot_r);
    return lot_r;
  }
}
