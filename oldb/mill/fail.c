/* mill/fail.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _mill_fail(): failage.
*/
u4_noun
_mill_fail(u4_milr m,
           const u4_cl *cl_msg)
{
  _mill_trap(m, cl_msg);
  {
    while ( !u4_n_zero(m->meb) ) {
      u4_noun i_meb = u4_ch(m->meb);
      {
        u4_noun hal = u4_ch(i_meb);
        u4_noun nof = u4_cth(i_meb);
        u4_noun jup = u4_ctt(i_meb);

        if ( u4_n_zero(u4_ct(m->meb)) ) {
          u4_sb sb_hal = u4_a_bin(hal, 3);
          u4_cl *cl_hal = alloca(sb_hal + 1);

          u4_a_bytes(hal, (u4_xb *)cl_hal, 0, sb_hal + 1);
          printf("fail: %s\n", cl_hal);

          if ( !u4_n_zero(nof) ) {
            u4_burp(m->lane, "home", u4_prep_textual(m->lane, nof));
          }
          if ( !u4_n_zero(jup) ) {
            u4_atom pp_jup = u4_ch(u4_ch(jup));
            u4_atom qp_jup = u4_ct(u4_ch(jup));
            u4_atom pq_jup = u4_ch(u4_ct(jup));
            u4_atom qq_jup = u4_ct(u4_ct(jup));
            {
              u4_xw xw_flin = u4_a_wtrip(pp_jup);
              u4_xw xw_fcol = u4_a_wtrip(qp_jup);
              u4_xw xw_llin = u4_a_wtrip(pq_jup);
              u4_xw xw_lcol = u4_a_wtrip(qq_jup);

              printf("spot: %d:%d - %d:%d\n\n", 
                     xw_flin, xw_fcol, xw_llin, xw_lcol);
            }
          }
        }
      }
      m->meb = u4_ct(m->meb);
    }
  }
  return u4_exit;
}
