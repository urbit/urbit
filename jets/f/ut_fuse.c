/* j/6/fuse.c
**
*/
#include "all.h"

/* logic
*/
  static u3_noun
  _fuse_in(u3_noun, u3_noun, u3_noun, u3_noun);

  static u3_noun
  _fuse_repo(u3_noun van,
             u3_noun sut,
             u3_noun ref,
             u3_noun bix)
  {
    u3_noun rep = u3qfu_repo(van, sut);
    u3_noun ret = _fuse_in(van, rep, ref, bix);

    if ( c3y == u3r_sing(ret, rep) ) {
      if ( c3__void == rep ) {
        return c3__void;
      } else {
        u3z(rep);
        u3z(ret);
        return u3k(sut);
      }
    } else {
      u3z(rep);
      return ret;
    }
  }

  static u3_noun
  _fuse_in_frog(u3_noun van, u3_noun p_sut, u3_noun ref, u3_noun bix)
  {
    if ( u3_nul == p_sut ) {
      return u3_nul;
    } 
    else {
      return u3nc(_fuse_in(van, u3h(p_sut), ref, bix),
                  _fuse_in_frog(van, u3t(p_sut), ref, bix));
    }
  }

  static u3_noun
  _fuse_in(u3_noun van,
           u3_noun sut,
           u3_noun ref,
           u3_noun bix)
  {
    u3_noun p_sut, q_sut;

    if ( c3y == u3r_sing(sut, ref) || (c3__noun == ref) ) {
      return u3k(sut);
    }
    if ( c3n == u3du(sut) ) switch ( sut ) {
      default: return u3m_bail(c3__fail);

      case c3__noun:
      {
        return u3k(ref);
      }
      case c3__void:
      {
        return c3__void;
      }
    }
    else switch ( u3h(sut) ) {
      default: return u3m_bail(c3__fail);

      case c3__atom: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        if ( c3y == u3du(ref) ) {
          if ( c3__atom == u3h(ref) ) {
            u3_noun p_ref, q_ref;

            u3x_cell(u3t(ref), &p_ref, &q_ref);
            {
              u3_noun foc = (c3y == u3qf_fitz(p_ref, p_sut))
                              ? u3k(p_sut)
                              : u3k(p_ref);

              if ( c3y == u3du(q_sut) ) {
                if ( c3y == u3du(q_ref) ) {
                  if ( c3y == u3r_sing(q_ref, q_sut) ) {
                    return u3nt(c3__atom, foc, u3k(q_sut));
                  }
                  else { 
                    u3z(foc);
                    return c3__void;
                  }
                }
                else {
                  return u3nt(c3__atom, foc, u3k(q_sut));
                }
              } else {
                return u3nt(c3__atom, foc, u3k(q_ref));
              }
            }
          }
          else if ( c3__cell == u3h(ref) ) {
            return c3__void;
          }
        }
        return _fuse_in(van, ref, sut, bix);
      }
      case c3__cell: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun p_ref, q_ref;

        if ( c3y == u3r_pq(ref, c3__cell, &p_ref, &q_ref) ) {
          u3_noun hed = _fuse_in(van, p_sut, p_ref, bix);
          u3_noun tal = _fuse_in(van, q_sut, q_ref, bix);
          u3_noun ret = u3qf_cell(hed, tal);

          u3z(hed);
          u3z(tal);
          return ret;
        }
        else return _fuse_in(van, ref, sut, bix);
      }
      case c3__core:
      {
        return _fuse_repo(van, sut, ref, bix);
      }
      case c3__face: u3x_cell(u3t(sut), &p_sut, &q_sut);
      {
        u3_noun vot = _fuse_in(van, q_sut, ref, bix);
        u3_noun ret = u3qf_face(p_sut, vot);

        u3z(vot);
        return ret;
      }
      case c3__fork: p_sut = u3t(sut);
      {
        u3_noun yed = u3qdi_tap(p_sut, u3_nul);
        u3_noun ret = u3kf_frog(_fuse_in_frog(van, yed, ref, bix));

        u3z(yed);
        return ret;
      }
      case c3__frog: p_sut = u3t(sut);
      {
        return u3kf_frog(_fuse_in_frog(van, p_sut, ref, bix));
      }
      case c3__hold:
      {
        u3_noun hud = u3nc(u3k(sut), u3k(ref));

        if ( c3y == u3qdi_has(bix, hud) ) {
          //  u3_noun dun = u3qfu_dunq(van, "type", sut);
          //  u3_noun niz = u3qfu_dunq(van, "over", ref);

          //  u3t_push(u3nc(c3__mean, dun));
          //  u3t_push(u3nc(c3__mean, niz));

          return u3m_error("fuse-loop");
        } else {
          u3_noun bux = u3qdi_put(bix, hud);
          u3_noun ret = _fuse_repo(van, sut, ref, bux);

          u3z(hud);
          u3z(bux);
          return ret;
        }
      }
    }
  }

  u3_noun
  _cqfu_fuse(u3_noun van,
             u3_noun sut,
             u3_noun ref)
  {
    return _fuse_in(van, sut, ref, u3_nul);
  }

/* boilerplate
*/
  u3_noun
  u3wfu_fuse(u3_noun cor)
  {
    u3_noun sut, ref, van;

    if ( (c3n == u3r_mean(cor, u3x_sam, &ref, u3x_con, &van, 0)) ||
         (u3_none == (sut = u3r_at(u3x_sam, van))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return _cqfu_fuse(van, sut, ref);
    }
  }

  u3_noun
  u3qfu_fuse(u3_noun van,
             u3_noun sut,
             u3_noun ref)
  {
    c3_m    fun_m = c3__fuse + !!u3r_at(u3qfu_van_vet, van);
    u3_noun pro   = u3z_find_2(fun_m, sut, ref);

    if ( u3_none != pro ) {
      return pro;
    }
    else {
      pro = _cqfu_fuse(van, sut, ref);

      return u3z_save_2(fun_m, sut, ref, pro);
    }
  }
