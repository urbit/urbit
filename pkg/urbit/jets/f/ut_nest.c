/* j/6/ut_nest.c
**
*/
#include "all.h"

u3_noun
u3wfu_nest_dext(u3_noun dext_core)
{
  u3_noun nest_in_core, nest_core;
  u3_noun sut, ref, van, seg, reg, gil;
  static c3_w zav_w = 0, vaz_w = 0;

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
  }
  else {
    c3_m  fun_m = 141 + c3__dext + ((!!u3r_at(u3qfu_van_vet, van)) << 8);
    u3_noun key = u3z_key_2(fun_m, sut, ref);
    u3_weak pro = u3z_find(key);

    if ( u3_none != pro ) {
      u3z(key);
      vaz_w++;
      return pro;
    }
    else {
      pro = u3n_nock_on(u3k(dext_core), u3k(u3x_at(u3x_bat, dext_core)));

      if ( ((c3y == pro) && (u3_nul == reg)) ||
           ((c3n == pro) && (u3_nul == seg)) )
      {
        if ( zav_w++ % 50000 == 1) {
          u3l_log("nest: %d %d %f\r\n", zav_w, vaz_w, (double)vaz_w / (zav_w + vaz_w) );
        }
        return u3z_save(key, pro);
      }
      else {
        u3z(key);
        return pro;
      }
    }
  }
}
