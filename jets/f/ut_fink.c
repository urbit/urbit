/* j/6/fink.c
**
*/
#include "all.h"


/* logic
*/
  static u3_noun
  _fink_flee(u3_noun poy)
  {
    u3_noun p_poy = u3h(poy);
    u3_noun q_poy = u3t(poy);

    switch ( u3h(q_poy) ) {
      default: return u3m_bail(c3__fail);
      case 0: {
        u3_noun pq_poy = u3t(q_poy);

        return u3nt(u3k(p_poy), c3y, u3k(pq_poy));
      }
      case 1: {
        u3_noun pq_poy = u3h(u3t(q_poy));
        u3_noun qq_poy = u3t(u3t(q_poy));

        return u3nq(u3k(p_poy), c3n, u3k(pq_poy), u3k(qq_poy));
      }
      case 2: {
        u3_noun pq_poy = u3h(u3t(q_poy));
        // u3_noun qq_poy = u3t(u3t(q_poy));
        // u3_noun ppq_poy = u3h(pq_poy);
        // u3_noun qpq_poy = u3h(u3t(pq_poy));
        u3_noun rpq_poy = u3h(u3t(u3t(pq_poy)));
        u3_noun spq_poy = u3t(u3t(u3t(pq_poy)));

        return u3nt(u3qc_peg(p_poy, rpq_poy), c3y, u3k(spq_poy));
      }
    }
  }

  u3_noun
  _cqfu_fink(u3_noun van,
             u3_noun sut,
             u3_noun dep,
             u3_noun way,
             u3_noun cug)
  {
    // u3_noun dun = u3qfu_dunq(van, "type", sut);
    u3_noun nuc = (u3_blip == cug)
      ?  u3qfu_shew(van,
                    u3nc(u3nc('c', 
                             u3i_string("find-limb")),
                         '$'))
      :  u3qfu_shep(van, "find-limb", 'a', u3k(cug));
    u3_noun pro;

    // u3t_push(u3nc(c3__mean, dun));
    u3t_push(u3nc(c3__mean, nuc));
    {
      u3_noun hoq = u3qfu_fino(van, sut, dep, way, cug);
      u3_noun fin = u3t(hoq);

      if ( u3_nul == fin ) {
        // u3m_p("cug", cug);
        return u3m_error("find-none");
      }
      else {
        pro = _fink_flee(u3t(fin));
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
    u3_noun sut, dep, way, cug, van;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &dep,
                               u3x_sam_6, &way,
                               u3x_sam_7, &cug,
                               u3x_con, &van,
                               0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_fink(van, sut, dep, way, cug);
    }
  }

  u3_noun
  u3qfu_fink(u3_noun van,
             u3_noun sut,
             u3_noun dep,
             u3_noun way,
             u3_noun cug)
  {
    return _cqfu_fink(van, sut, dep, way, cug);
  }
