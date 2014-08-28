/* j/6/fink.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* logic
*/
  u2_noun                                                         //  transfer
  j2_mcx(Pt6, ut, fink)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dep,                              //  retain
                        u2_noun way,                              //  retain
                        u2_noun cog)                              //  retain
  {
    // u2_noun dun = j2_mcy(Pt6, ut, dunq)(van, "type", sut);
    u2_noun nuc = (u2_blip == cog)
      ? j2_mcy(Pt6, ut, shew)
          (van,
                  u2nc
                    (u2nc('c', u2_ci_string("find-limb")),
                            '$'))
      :  j2_mcy(Pt6, ut, shep)
          (van, "find-limb", 'a', u2k(cog));
    u2_noun pro;

    // u2_ct_push(u2nc(c3__mean, dun));
    u2_ct_push(u2nc(c3__mean, nuc));
    {
      u2_noun hoq = j2_mcy(Pt6, ut, find)(van, sut, dep, way, cog);
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
  u2_ho_jet
  j2_mcj(Pt6, ut, fink)[];

  u2_noun                                                         //  transfer
  j2_mc(Pt6, ut, fink)(u2_noun cor)                               //  retain
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
      return j2_mcx(Pt6, ut, fink)(van, sut, dep, way, cog);
    }
  }

  u2_noun                                                         //  transfer
  j2_mcy(Pt6, ut, fink)(
                        u2_noun van,                              //  retain
                        u2_noun sut,                              //  retain
                        u2_noun dep,
                        u2_noun way,                              //  retain
                        u2_noun cog)                              //  retain
  {
    return j2_mcx(Pt6, ut, fink)(van, sut, dep, way, cog);
  }

  u2_weak
  j2_mck(Pt6, ut, fink)(u2_noun cor)
  {
    u2_noun sut, dep, way, cog, van;

    if ( (u2_no == u2_cr_mean(cor, u2_cv_sam_2, &dep,
                                u2_cv_sam_6, &way,
                                u2_cv_sam_7, &cog,
                                u2_cv_con, &van,
                                0)) ||
         (u2_none == (sut = u2_cr_at(u2_cv_sam, van))) )
    {
      return u2_none;
    } else {
      return u2nq
        (u2k(sut),
                u2k(dep),
                u2k(way),
                u2k(cog));
    }
  }

/* structures
*/
  u2_ho_jet
  j2_mcj(Pt6, ut, fink)[] = {
    { ".2", c3__hevy,
        j2_mc(Pt6, ut, fink),
        Tier6_b,
        u2_none, u2_none,
        j2_mck(Pt6, ut, fink), c3__fink,
    },
    { }
  };
