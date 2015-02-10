/* j/6/fink.c
**
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
    // u3_noun dun = u3qfu_dunq(van, "type", sut);
    u3_noun nuc = (u3_blip == cog)
      ? u3qfu_shew
          (van,
                  u3nc
                    (u3nc('c', u3i_string("find-limb")),
                            '$'))
      :  u3qfu_shep
          (van, "find-limb", 'a', u3k(cog));
    u3_noun pro;

    // u3t_push(u3nc(c3__mean, dun));
    u3t_push(u3nc(c3__mean, nuc));
    {
      u3_noun hoq = u3qfu_find(van, sut, dep, way, cog);
      u3_noun fin = u3t(hoq);

      if ( u3_nul == fin ) {
        u3m_p("cog", cog);
        return u3m_error("find-none");
      }
      else {
        pro = u3k(u3t(fin));
        u3z(hoq);
      }
    }
    // u3t_drop();
    u3t_drop();

    return pro;
  }

/* boilerplate
*/
  u3_noun
  u3wfu_fink(u3_noun cor)
  {
    u3_noun sut, dep, way, cog, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &dep,
                                u3x_sam_6, &way,
                                u3x_sam_7, &cog,
                                u3x_con, &van,
                                0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_fink(van, sut, dep, way, cog);
    }
  }

  u3_noun
  u3qfu_fink(
                        u3_noun van,
                        u3_noun sut,
                        u3_noun dep,
                        u3_noun way,
                        u3_noun cog)
  {
    return _cqfu_fink(van, sut, dep, way, cog);
  }
