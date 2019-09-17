/* j/6/ut_nest.c
**
*/
#include "all.h"


  u3_noun
  u3wfu_nest_dext(u3_noun dext_core)
  {
    u3_noun nest_in_core, nest_core;
    u3_noun sut, ref, van, seg, reg, gil;

    if ( (u3_none == (nest_in_core = u3r_at(3, dext_core))) ||
         (c3n == u3r_mean(nest_in_core, u3x_sam_2, &seg,
                                        u3x_sam_6, &reg,
                                        u3x_sam_7, &gil,
                                        7, &nest_core,
                                        0)) ||
         (c3n == u3r_mean(nest_core, u3x_sam_3, &ref,
                                     u3x_con, &van,
                                     0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      c3_m    fun_m = 141 + c3__dext + ((!!u3r_at(u3qfu_van_vet, van)) << 8);
      u3_noun pro   = u3z_find_2(fun_m, sut, ref);

      if ( u3_none != pro ) {
        return pro;
      }
      else {
        pro = u3n_nock_on(u3k(dext_core), u3k(u3x_at(u3x_bat, dext_core)));

        if ( ((c3y == pro) && (u3_nul == reg)) ||
             ((c3n == pro) && (u3_nul == seg)) )
        {
          return u3z_save_2(fun_m, sut, ref, pro);
        }
        else return pro;
      }
    }
  }
