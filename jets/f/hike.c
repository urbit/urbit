/* j/6/hike.c
**
*/
#include "all.h"


/* internal tools
*/
  /* _lily_hike_belt_root(): convert (pac) to a list of root tools.
  */
  static u3_noun
  _lily_hike_belt_root(u3_noun pac)
  {
    if ( (u3_nul == pac) ) {
      return u3_nul;
    }
    else {
      u3_atom axis      = u3h(u3h(pac));
      u3_noun tool      = u3t(u3h(pac));
      u3_noun list_tool = _lily_hike_belt_root(u3t(pac));

      if ( c3y == u3r_sing(1, axis) ) {
        return u3nc(u3k(tool),
                    list_tool);
      }
      else return list_tool;
    }
  }

  /* _lily_hike_belt_l(): factor (pac) left.
  */
  static u3_noun
  _lily_hike_belt_l(u3_noun pac)
  {
    if ( (u3_nul == pac) ) {
      return u3_nul;
    }
    else {
      u3_atom axis       = u3h(u3h(pac));
      u3_noun tool       = u3t(u3h(pac));
      u3_noun belt_l     = _lily_hike_belt_l(u3t(pac));

      {
        if ( (1 != axis) &&
             (c3y == u3r_sing(2, u3qc_cap(axis))) )
        {
          u3_atom axis_tap = u3qc_mas(axis);

          return u3nc(u3nc(u3k(axis_tap),
                           u3k(tool)),
                      belt_l);
        }
        else return belt_l;
      }
    }
  }

  /* _lily_hike_belt_r(): factor (pac) right.
  */
  static u3_noun
  _lily_hike_belt_r(u3_noun pac)
  {
    if ( (u3_nul == pac) ) {
      return u3_nul;
    }
    else {
      u3_atom axis       = u3h(u3h(pac));
      u3_noun tool       = u3t(u3h(pac));
      u3_noun belt_r     = _lily_hike_belt_r(u3t(pac));

      {
        if ( (1 != axis) &&
             (c3y == u3r_sing(3, u3qc_cap(axis))) )
        {
          u3_atom axis_tap = u3qc_mas(axis);

          return u3nc(u3nc(u3k(axis_tap),
                           u3k(tool)),
                      belt_r);
        }
        else return belt_r;
      }
    }
  }

/* functions
*/
  u3_noun
  u3qf_hike(u3_noun axe,
            u3_noun pac)
  {
    if ( (u3_nul == pac) ) {
      return u3nc(0, u3k(axe));
    }
    else {
      u3_noun zet = _lily_hike_belt_root(pac);

      if ( u3_nul != zet ) {
        u3_noun fol = u3k(u3h(zet));

        u3z(zet);
        return fol;
      }
      else {
        u3_noun tum = _lily_hike_belt_l(pac);
        u3_noun gam = _lily_hike_belt_r(pac);
        u3_noun hax = u3qc_peg(axe, 2);
        u3_noun moz = u3qc_peg(axe, 3);
        u3_noun zip = u3qf_hike(hax, tum);
        u3_noun dof = u3qf_hike(moz, gam);
        u3_noun fol = u3qf_cons(zip, dof);

        u3z(tum);
        u3z(gam);
        u3z(hax);
        u3z(moz);
        u3z(zip);
        u3z(dof);

        return fol;
      }
    }
  }
  u3_noun
  u3wf_hike(u3_noun cor)
  {
    u3_noun axe, pac;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &axe, u3x_sam_3, &pac, 0)) ||
         (c3n == u3ud(axe)) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qf_hike(axe, pac);
    }
  }
