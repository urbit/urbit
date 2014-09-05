/* j/6/fink.c
**
** This file is in the public domain.
*/
#include "all.h"


/* logic
*/
  u2_noun
  _cqfu_fink(
                        u2_noun van,
                        u2_noun sut,
                        u2_noun dep,
                        u2_noun way,
                        u2_noun cog)
  {
    // u2_noun dun = u2_cqfu_dunq(van, "type", sut);
    u2_noun nuc = (u2_blip == cog)
      ? u2_cqfu_shew
          (van,
                  u2nc
                    (u2nc('c', u2_ci_string("find-limb")),
                            '$'))
      :  u2_cqfu_shep
          (van, "find-limb", 'a', u2k(cog));
    u2_noun pro;

    // u2_ct_push(u2nc(c3__mean, dun));
    u2_ct_push(u2nc(c3__mean, nuc));
    {
      u2_noun hoq = u2_cqfu_find(van, sut, dep, way, cog);
      u2_noun fin = u2t(hoq);

      if ( u2_nul == fin ) {
        return u2_cm_error("find-none");
      }
      else {
        pro = u2k(u2t(fin));
        u2z(hoq);
      }
    }
    u2_ct_drop();
    // u2_ct_drop();

    return pro;
  }

/* boilerplate
*/
  u2_noun
  u2_cwfu_fink(u2_noun cor)
  {
    u2_noun sut, dep, way, cog, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &dep,
                                u2_cv_sam_6, &way,
                                u2_cv_sam_7, &cog,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_cm_bail(c3__fail);
    } else {
      return _cqfu_fink(van, sut, dep, way, cog);
    }
  }

  u2_noun
  u2_cqfu_fink(
                        u2_noun van,
                        u2_noun sut,
                        u2_noun dep,
                        u2_noun way,
                        u2_noun cog)
  {
    return _cqfu_fink(van, sut, dep, way, cog);
  }
