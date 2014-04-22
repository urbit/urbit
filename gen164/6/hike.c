/* j/6/hike.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* internal tools
*/
  /* _lily_hike_belt_root(): convert (pac) to a list of root tools.
  */
  static u2_list
  _lily_hike_belt_root(u2_ray  wir_r,
                       u2_list pac)
  {
    if ( (u2_nul == pac) ) {
      return u2_nul;
    }
    else {
      u2_axis axis     = u2_h(u2_h(pac));
      u2_tool tool     = u2_t(u2_h(pac));
      u2_list list_tool = _lily_hike_belt_root(wir_r, u2_t(pac));

      if ( u2_yes == u2_sing(_1, axis) ) {
        return u2_bc(wir_r, u2_rx(wir_r, tool),
                            list_tool);
      }
      else return list_tool;
    }
  }

  /* _lily_hike_belt_l(): factor (pac) left.
  */
  static u2_list
  _lily_hike_belt_l(u2_ray  wir_r,
                    u2_list pac)
  {
    if ( (u2_nul == pac) ) {
      return u2_nul;
    }
    else {
      u2_axis axis       = u2_h(u2_h(pac));
      u2_tool tool       = u2_t(u2_h(pac));
      u2_list belt_l = _lily_hike_belt_l(wir_r, u2_t(pac));

      {
        if ( (_1 != axis) &&
             (u2_yes == u2_sing(_2, j2_mbc(Pt3, cap)(wir_r, axis))) )
        {
          u2_axis axis_tap = j2_mbc(Pt3, mas)(wir_r, axis);

          return u2_bc(wir_r,
                       u2_bc(wir_r,
                             u2_rx(wir_r, axis_tap),
                             u2_rx(wir_r, tool)),
                       belt_l);
        }
        else return belt_l;
      }
    }
  }

  /* _lily_hike_belt_r(): factor (pac) right.
  */
  static u2_list                                                  //  transfer
  _lily_hike_belt_r(u2_ray  wir_r,
                    u2_list pac)                                  //  retain
  {
    if ( (u2_nul == pac) ) {
      return u2_nul;
    }
    else {
      u2_axis axis       = u2_h(u2_h(pac));
      u2_tool tool       = u2_t(u2_h(pac));
      u2_list belt_r = _lily_hike_belt_r(wir_r, u2_t(pac));

      {
        if ( (_1 != axis) &&
             (u2_yes == u2_sing(_3, j2_mbc(Pt3, cap)(wir_r, axis))) )
        {
          u2_axis axis_tap = j2_mbc(Pt3, mas)(wir_r, axis);

          return u2_bc(wir_r,
                       u2_bc(wir_r, u2_rx(wir_r, axis_tap),
                                    u2_rx(wir_r, tool)),
                       belt_r);
        }
        else return belt_r;
      }
    }
  }

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pt6, hike)(u2_wire wir_r,
                    u2_noun axe,                                  //  retain
                    u2_noun pac)                                  //  retain
  {
    if ( (u2_nul == pac) ) {
      return u2_bc(wir_r, u2_nock_0, u2_rx(wir_r, axe));
    }
    else {
      u2_noun zet = _lily_hike_belt_root(wir_r, pac);

      if ( u2_nul != zet ) {
        u2_noun fol = u2_rx(wir_r, u2_h(zet));

        u2_rl_lose(wir_r, zet);
        return fol;
      }
      else {
        u2_noun tum = _lily_hike_belt_l(wir_r, pac);
        u2_noun gam = _lily_hike_belt_r(wir_r, pac);
        u2_noun hax = j2_mbc(Pt3, peg)(wir_r, axe, 2);
        u2_noun moz = j2_mbc(Pt3, peg)(wir_r, axe, 3);
        u2_noun zip = j2_mby(Pt6, hike)(wir_r, hax, tum);
        u2_noun dof = j2_mby(Pt6, hike)(wir_r, moz, gam);
        u2_noun fol = j2_mby(Pt6, cons)(wir_r, zip, dof);

        u2_rl_lose(wir_r, tum);
        u2_rl_lose(wir_r, gam);
        u2_rl_lose(wir_r, hax);
        u2_rl_lose(wir_r, moz);
        u2_rl_lose(wir_r, zip);
        u2_rl_lose(wir_r, dof);

        return fol;
      }
    }
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt6, hike)(u2_wire wir_r,
                   u2_noun cor)                                   //  retain
  {
    u2_noun axe, pac;

    if ( (u2_no == u2_mean(cor, u2_cv_sam_2, &axe, u2_cv_sam_3, &pac, 0)) ||
         (u2_no == u2_stud(axe)) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mby(Pt6, hike)(wir_r, axe, pac);
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mbj(Pt6, hike)[] = {
    { ".2", c3__hevy, j2_mb(Pt6, hike), Tier6_a, u2_none, u2_none },
    { }
  };
