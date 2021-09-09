/* j/5/scr.c
**
*/
#include "all.h"
#include <urcrypt.h>

/* functions
*/
  static u3_weak
  _cqes_hs(u3_atom p, c3_w pwd_w,
           u3_atom s, c3_w sal_w,
           u3_atom n,
           u3_atom r,
           u3_atom z,
           u3_atom d)
  {
    u3_noun chk;
    c3_w out_w;

    if ( !u3r_word_fit(&out_w, d) ) {
      return u3m_bail(c3__fail);
    }
    if ( 0 == r || 0 == z ) {
      return u3m_bail(c3__exit);
    }
    chk = u3qc_bex(31);
    if ( (c3n == u3qa_lth(pwd_w, chk)) ||
         (c3n == u3qa_lth(sal_w, chk)) ) {
      return u3m_bail(c3__exit);
    }
    u3z(chk);
    chk = u3kc_bex(u3ka_dec(u3qc_xeb(n)));
    if ( c3n == u3r_sing(n, chk) ) {
      return u3m_bail(c3__exit);
    }
    u3z(chk);
    if ( c3n == u3ka_lte(
          u3ka_mul(u3qa_mul(128, r), u3ka_dec(u3qa_add(n, z))),
          u3qc_bex(30)) ) {
      return u3m_bail(c3__exit);
    }

    if ( (u3r_met(6, n) > 1) ||
         (u3r_met(5, r) > 1) ||
         (u3r_met(5, z) > 1) ) {
      return u3_none;
    }
    else {
      u3_noun pro;
      c3_d    n_d = u3r_chub(0, n);
      c3_w    r_w = u3r_word(0, r),
              z_w = u3r_word(0, z);
      c3_y   *pwd_y = u3a_malloc(pwd_w),
             *sal_y = u3a_malloc(sal_w),
             *out_y = u3a_malloc(d);
      u3r_bytes(0, pwd_w, pwd_y, p);
      u3r_bytes(0, sal_w, sal_y, s);
      pro = ( 0 == urcrypt_scrypt(pwd_y, pwd_w,
                                  sal_y, sal_w,
                                  n_d, r_w, z_w,
                                  out_w, out_y) )
        ? u3i_bytes(out_w, out_y)
        : u3_none;
      u3a_free(pwd_y);
      u3a_free(sal_y);
      u3a_free(out_y);
      return pro;
    }
  }

  static u3_weak
  _cqes_hsl(u3_atom p, u3_atom pl,
            u3_atom s, u3_atom sl,
            u3_atom n,
            u3_atom r,
            u3_atom z,
            u3_atom d)
  {
    c3_w pwd_w, sal_w;
    if ( !(u3r_word_fit(&pwd_w, pl) &&
           u3r_word_fit(&sal_w, sl)) ) {
      return u3m_bail(c3__fail);
    }
    else {
      return _cqes_hs(p, pwd_w, s, sal_w, n, r, z, d);
    }
  }

  u3_noun
  u3wes_hsl(u3_noun cor)
  {
    u3_noun p, pl, s, sl, n, r, z, d;
    u3_noun q;

    u3x_quil(u3x_at(u3x_sam, cor), &p, &pl, &s, &sl, &q);
    u3x_qual(q, &n, &r, &z, &d);

    if ( !(_(u3a_is_atom(p)) && _(u3a_is_atom(pl)) &&
           _(u3a_is_atom(s)) && _(u3a_is_atom(sl)) &&
           _(u3a_is_atom(n)) && _(u3a_is_atom(r))  &&
           _(u3a_is_atom(z)) && _(u3a_is_atom(d))) ) {
      return u3m_bail(c3__exit);
    }
    else {
     return u3l_punt("scr-hsl", _cqes_hsl(p, pl, s, sl, n, r, z, d));
    }
  }

  static u3_weak
  _cqes_hsh(u3_atom p,
            u3_atom s,
            u3_atom n,
            u3_atom r,
            u3_atom z,
            u3_atom d)
  {
     return _cqes_hs(p, u3r_met(3, p),
                     s, u3r_met(3, s),
                     n, r, z, d);
  }

  u3_noun
  u3wes_hsh(u3_noun cor)
  {
    u3_noun p, s, n, r, z, d;
    u3_noun q;

    u3x_quil(u3x_at(u3x_sam, cor), &p, &s, &n, &r, &q);
    u3x_cell(q, &z, &d);

    if ( !(_(u3a_is_atom(p)) && _(u3a_is_atom(s))  &&
           _(u3a_is_atom(n)) && _(u3a_is_atom(r))  &&
           _(u3a_is_atom(z)) && _(u3a_is_atom(d))) ) {
      return u3m_bail(c3__exit);
    }
    else {
      return u3l_punt("scr-hsh", _cqes_hsh(p, s, n, r, z, d));
    }
  }

  static u3_atom
  _cqes_pb(u3_atom p, c3_w pwd_w,
           u3_atom s, c3_w sal_w,
           u3_atom c,
           u3_atom d)
  {
    if ( (c > (1 << 28)) ||
         (d > (1 << 30)) ) {
      // max key length 1gb
      // max iterations 2^28
      return u3m_bail(c3__exit);
    }
    else {
      u3_noun pro;
      c3_w    out_w;
      c3_y   *pwd_y = u3a_malloc(pwd_w),
             *sal_y = u3a_malloc(sal_w),
             *out_y = u3a_malloc(d);
      u3r_bytes(0, pwd_w, pwd_y, p);
      u3r_bytes(0, sal_w, sal_y, s);
      urcrypt_scrypt_pbkdf_sha256(pwd_y, pwd_w, sal_y, sal_w, c, d, out_y);
      pro = u3i_bytes(d, out_y);
      u3a_free(pwd_y);
      u3a_free(sal_y);
      u3a_free(out_y);
      return pro;
    }
  }

  static u3_noun
  _cqes_pbl(u3_atom p, u3_atom pl,
            u3_atom s, u3_atom sl,
            u3_atom c,
            u3_atom d)
  {
    c3_w pwd_w, sal_w;
    if ( !(u3r_word_fit(&pwd_w, pl) &&
           u3r_word_fit(&sal_w, sl)) ) {
      return u3m_bail(c3__fail);
    }
    else {
      return _cqes_pb(p, pwd_w, s, sal_w, c, d);
    }
  }

  u3_noun
  u3wes_pbl(u3_noun cor)
  {
    u3_noun p, pl, s, sl, c, d;
    u3_noun q;

    u3x_quil(u3x_at(u3x_sam, cor), &p, &pl, &s, &sl, &q);
    u3x_cell(q, &c, &d);

    if ( !(_(u3a_is_atom(p))  && _(u3a_is_atom(s))  &&
           _(u3a_is_atom(pl)) && _(u3a_is_atom(sl)) &&
           _(u3a_is_atom(c))  && _(u3a_is_atom(d))) ) {
      return u3m_bail(c3__exit);
    }
    else {
      return _cqes_pbl(p, pl, s, sl, c, d);
    }
  }

  static u3_atom
  _cqes_pbk(u3_atom p, u3_atom s, u3_atom c, u3_atom d)
  {
    return _cqes_pb(p, u3r_met(3, p),
                    s, u3r_met(3, s),
                    c, d);
  }

  u3_noun
  u3wes_pbk(u3_noun cor)
  {
    u3_noun p, s, c, d;

    u3x_qual(u3x_at(u3x_sam, cor), &p, &s, &c, &d);

    if ( !(_(u3a_is_atom(p)) && _(u3a_is_atom(s)) &&
           _(u3a_is_atom(c)) && _(u3a_is_atom(d))) ) {
      return u3m_bail(c3__exit);
    }
    else {
      return _cqes_pbk(p, s, c, d);
    }
  }
