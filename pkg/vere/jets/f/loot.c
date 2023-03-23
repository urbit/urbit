/* j/6/loot.c
**
*/
#include "all.h"


/* internals
*/
  static u3_noun
  _loot_in(u3_noun cog,
           u3_noun dom,
           u3_atom axe)
  {
    if ( u3_nul == dom ) {
      return u3_nul;
    }
    else {
      u3_noun n_dom, l_dom, r_dom;

      u3r_trel(dom, &n_dom, &l_dom, &r_dom);
      if ( c3n == u3du(n_dom) ) {
        return u3m_bail(c3__fail);
      }
      else {
        u3_noun qqn_dom = u3t(u3t(n_dom));
        u3_noun yep = u3qf_look(cog, qqn_dom);

        if ( (u3_nul == l_dom) && (u3_nul == r_dom) ) {
          if ( u3_nul == yep ) {
            return u3_nul;
          } else {
            u3_noun u_yep = u3t(yep);
            u3_noun pro;

            pro = u3nt(u3_nul, u3qc_peg(axe, u3h(u_yep)), u3k(u3t(u_yep)));
            u3z(yep);
            return pro;
          }
        }
        else if ( (u3_nul == l_dom) ) {
          if ( u3_nul == yep ) {
            u3_noun nax = u3qc_peg(axe, 3);
            u3_noun pro;

            pro = _loot_in(cog, r_dom, nax);
            u3z(nax);
            return pro;
          }
          else {
            u3_noun u_yep = u3t(yep);
            u3_noun nax   = u3qc_peg(axe, 2);
            u3_noun pro;

            pro = u3nt(u3_nul, u3qc_peg(nax, u3h(u_yep)), u3k(u3t(u_yep)));
            u3z(nax);
            u3z(yep);
            return pro;
          }
        }
        else if ( (u3_nul == r_dom) ) {
          if ( u3_nul == yep ) {
            u3_noun nax = u3qc_peg(axe, 3);
            u3_noun pro;

            pro = _loot_in(cog, l_dom, nax);
            u3z(nax);
            return pro;
          }
          else {
            u3_noun u_yep = u3t(yep);
            u3_noun nax   = u3qc_peg(axe, 2);
            u3_noun pro;

            pro = u3nt(u3_nul, u3qc_peg(nax, u3h(u_yep)), u3k(u3t(u_yep)));
            u3z(nax);
            u3z(yep);
            return pro;
          }
        }
        else {
          if ( u3_nul == yep ) {
            u3_noun nax = u3qc_peg(axe, 6);
            u3_noun pey;

            pey = _loot_in(cog, l_dom, nax);
            u3z(nax);

            if ( u3_nul != pey ) {
              return pey;
            }
            else {
              u3_noun nax = u3qc_peg(axe, 7);
              u3_noun pro;

              pro = _loot_in(cog, r_dom, nax);
              u3z(nax);
              return pro;
            }
          }
          else {
            u3_noun u_yep = u3t(yep);
            u3_noun nax   = u3qc_peg(axe, 2);
            u3_noun pro;

            pro = u3nt(u3_nul, u3qc_peg(nax, u3h(u_yep)), u3k(u3t(u_yep)));
            u3z(nax);
            u3z(yep);
            return pro;
          }
        }
      }
    }
  }

/* functions
*/
  u3_noun
  u3qf_loot(u3_noun cog,
            u3_noun dom)
  {
    return _loot_in(cog, dom, 1);
  }
  u3_noun
  u3wf_loot(u3_noun cor)
  {
    u3_noun cog, dom;

    if ( c3n == u3r_mean(cor, u3x_sam_2, &cog, u3x_sam_3, &dom, 0) ) {
      return u3m_bail(c3__fail);
    } else {
      return u3qf_loot(cog, dom);
    }
  }
