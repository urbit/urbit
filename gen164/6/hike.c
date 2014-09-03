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
  static u2_noun
  _lily_hike_belt_root(
                       u2_noun pac)
  {
    if ( (u2_nul == pac) ) {
      return u2_nul;
    }
    else {
      u2_atom axis     = u2h(u2h(pac));
      u2_noun tool     = u2t(u2h(pac));
      u2_noun list_tool = _lily_hike_belt_root(u2t(pac));

      if ( u2_yes == u2_cr_sing(1, axis) ) {
        return u2nc(u2k(tool),
                            list_tool);
      }
      else return list_tool;
    }
  }

  /* _lily_hike_belt_l(): factor (pac) left.
  */
  static u2_noun
  _lily_hike_belt_l(
                    u2_noun pac)
  {
    if ( (u2_nul == pac) ) {
      return u2_nul;
    }
    else {
      u2_atom axis       = u2h(u2h(pac));
      u2_noun tool       = u2t(u2h(pac));
      u2_noun belt_l = _lily_hike_belt_l(u2t(pac));

      {
        if ( (1 != axis) &&
             (u2_yes == u2_cr_sing(2, u2_cqc_cap(axis))) )
        {
          u2_atom axis_tap = u2_cqc_mas(axis);

          return u2nc(
                       u2nc(
                             u2k(axis_tap),
                             u2k(tool)),
                       belt_l);
        }
        else return belt_l;
      }
    }
  }

  /* _lily_hike_belt_r(): factor (pac) right.
  */
  static u2_noun                                                  //  transfer
  _lily_hike_belt_r(
                    u2_noun pac)                                  //  retain
  {
    if ( (u2_nul == pac) ) {
      return u2_nul;
    }
    else {
      u2_atom axis       = u2h(u2h(pac));
      u2_noun tool       = u2t(u2h(pac));
      u2_noun belt_r = _lily_hike_belt_r(u2t(pac));

      {
        if ( (1 != axis) &&
             (u2_yes == u2_cr_sing(3, u2_cqc_cap(axis))) )
        {
          u2_atom axis_tap = u2_cqc_mas(axis);

          return u2nc(
                       u2nc(u2k(axis_tap),
                                    u2k(tool)),
                       belt_r);
        }
        else return belt_r;
      }
    }
  }

/* functions
*/
  u2_noun                                                         //  transfer
  u2_cqf_hike(
                    u2_noun axe,                                  //  retain
                    u2_noun pac)                                  //  retain
  {
    if ( (u2_nul == pac) ) {
      return u2nc(0, u2k(axe));
    }
    else {
      u2_noun zet = _lily_hike_belt_root(pac);

      if ( u2_nul != zet ) {
        u2_noun fol = u2k(u2h(zet));

        u2z(zet);
        return fol;
      }
      else {
        u2_noun tum = _lily_hike_belt_l(pac);
        u2_noun gam = _lily_hike_belt_r(pac);
        u2_noun hax = u2_cqc_peg(axe, 2);
        u2_noun moz = u2_cqc_peg(axe, 3);
        u2_noun zip = u2_cqf_hike(hax, tum);
        u2_noun dof = u2_cqf_hike(moz, gam);
        u2_noun fol = u2_cqf_cons(zip, dof);

        u2z(tum);
        u2z(gam);
        u2z(hax);
        u2z(moz);
        u2z(zip);
        u2z(dof);

        return fol;
      }
    }
  }
  u2_noun                                                         //  transfer
  u2_cwf_hike(
                   u2_noun cor)                                   //  retain
  {
    u2_noun axe, pac;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &axe, u2_cv_sam_3, &pac, 0)) ||
         (u2_no == u2ud(axe)) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return u2_cqf_hike(axe, pac);
    }
  }
