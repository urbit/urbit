/* j/6/fink.c
**
** This file is in the public domain.
*/
#include "all.h"


/* logic
*/
  u3_noun
  _cqfu_fink(
                        u3_noun van,
                        u3_noun sut,
                        u3_noun dep,
                        u3_noun way,
                        u3_noun cog)
  {
    // u3_noun dun = u3_cqfu_dunq(van, "type", sut);
    u3_noun nuc = (u3_blip == cog)
      ? u3_cqfu_shew
          (van,
                  u3nc
                    (u3nc('c', u3_ci_string("find-limb")),
                            '$'))
      :  u3_cqfu_shep
          (van, "find-limb", 'a', u3k(cog));
    u3_noun pro;

    // u3_ct_push(u3nc(c3__mean, dun));
    u3_ct_push(u3nc(c3__mean, nuc));
    {
      u3_noun hoq = u3_cqfu_find(van, sut, dep, way, cog);
      u3_noun fin = u3t(hoq);

      if ( u3_nul == fin ) {
        return u3_cm_error("find-none");
      }
      else {
        pro = u3k(u3t(fin));
        u3z(hoq);
      }
    }
    u3_ct_drop();
    // u3_ct_drop();

    return pro;
  }

/* boilerplate
*/
  u3_noun
  u3_cwfu_fink(u3_noun cor)
  {
    u3_noun sut, dep, way, cog, van;

    if ( (u3_no == u3_cr_mean(cor, u3_cv_sam_2, &dep,
                                u3_cv_sam_6, &way,
                                u3_cv_sam_7, &cog,
                                u3_cv_con, &van,
                                0)) ||
         (u3_none == (sut = u3_cr_at(u3_cv_sam, van))) )
    {
      return u3_cm_bail(c3__fail);
    } else {
      return _cqfu_fink(van, sut, dep, way, cog);
    }
  }

  u3_noun
  u3_cqfu_fink(
                        u3_noun van,
                        u3_noun sut,
                        u3_noun dep,
                        u3_noun way,
                        u3_noun cog)
  {
    return _cqfu_fink(van, sut, dep, way, cog);
  }
