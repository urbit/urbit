/* j/e/fl.c
**
*/
#include "all.h"

/* structures
*/
  typedef struct _flOptions {
    c3_w precision;
    mpz_t minExp;
    mpz_t expWidth;
    c3_w rMode;
    c3_w eMode;
  } flOptions;

  typedef struct _ea {
    mpz_t e;
    mpz_t a;
  } ea;

/* functions
*/
  static void
  _satom_to_mp(mpz_t a_mp,
               u3_atom b)
  {
    if ( _(u3a_is_cat(b)) ) {
      c3_ws c = (b + 1) >> 1;
      if ( (b & 1) ) {
        c = -c;
      }
      mpz_init_set_si(a_mp, c);
    }
    else {
      u3r_mp(a_mp, b);
      c3_t x = mpz_odd_p(a_mp);
      mpz_add_ui(a_mp, a_mp, 1);
      mpz_tdiv_q_2exp(a_mp, a_mp, 1);
      if ( x ) {
        mpz_neg(a_mp, a_mp);
      }
    }
  }

  static u3_noun
  _mp_to_satom(mpz_t a_mp)
  {
    c3_ws b = mpz_sgn(a_mp);
    switch ( b ) {
      default: return u3m_bail(c3__fail);
      case  0: {
        mpz_clear(a_mp);
        return 0;
      }
      case  1: {
        mpz_mul_2exp(a_mp, a_mp, 1);
        return u3i_mp(a_mp);
      }
      case -1: {
        mpz_abs(a_mp, a_mp);
        mpz_mul_2exp(a_mp, a_mp, 1);
        mpz_sub_ui(a_mp, a_mp, 1);
        return u3i_mp(a_mp);
      }
    }
  }

  static void
  _noun_to_flOptions(flOptions* a,
                     u3_noun    b)
  {
    u3_noun c;
    u3_atom d, e, f, g, h;
    u3x_trel(b, &c, &d, &e);
    u3x_trel(c, &f, &g, &h);

    mpz_t i;
    u3r_mp(i, f);
    if ( !mpz_fits_uint_p(i) ) {
      mpz_clear(i);
      u3m_bail(c3__exit);
    }
    a->precision = mpz_get_ui(i);
    mpz_clear(i);

    if ( a->precision < 2 ) u3m_bail(c3__exit);

    _satom_to_mp(a->minExp, g);
    u3r_mp(a->expWidth, h);

    if ( !(_(u3a_is_cat(d)) && _(u3a_is_cat(e))) ) {
      mpz_clear(a->minExp);
      mpz_clear(a->expWidth);
      u3m_bail(c3__exit);
    }
    a->rMode = d;
    a->eMode = e;
  }

  static void
  _noun_to_ea(ea*     a,
              u3_noun b)
  {
    u3_atom c, d;
    u3x_cell(b, &c, &d);

    if ( !(_(u3a_is_cat(c))) ) {
      u3m_bail(c3__exit);
    }

    _satom_to_mp(a->e, c);
    u3r_mp(a->a, d);
  }

  static u3_noun
  _ea_to_noun(ea* a)
  {
    u3_atom b = _mp_to_satom(a->e);
    u3_atom c = u3i_mp(a->a);

    return u3i_cell(u3k(b), u3k(c));
  }

  static void
  _xpd(ea*        a,
       flOptions* b)
  {
    size_t z = mpz_sizeinbase(a->a, 2);
    if ( z >= b->precision ) return;
    c3_w c = b->precision - z;

    if ( b->eMode != c3__i ) {
      mpz_t i;
      mpz_init_set(i, a->e);
      mpz_sub(i, i, b->minExp);
      if ( mpz_sgn(i) < 0 ) {
        c = 0;
      }
      else if ( mpz_fits_uint_p(i) )
      {
        c3_w d = mpz_get_ui(i);
        c = c3_min(c, d);
      }
      mpz_clear(i);
    }

    mpz_mul_2exp(a->a, a->a, c);
    mpz_sub_ui(a->e, a->e, c);
  }

  /* a: floating point number, b: flOptions, i: rounding mode, j: sticky bit */
  u3_noun
  u3qef_lug(u3_noun a,
            u3_noun b,
            u3_atom i,
            u3_atom j)
  {
    mpz_t v, g, h;
    ea c;
    flOptions d;
    _noun_to_ea(&c, a);
    _noun_to_flOptions(&d, b);
    if ( mpz_sgn(c.a) == 0 ) {
      mpz_clear(d.minExp); mpz_clear(d.expWidth);
      mpz_clear(c.a); mpz_clear(c.e);
      return u3m_bail(c3__exit);
    }
    size_t m = mpz_sizeinbase(c.a, 2);
    if ( !_(j) && (m <= d.precision) ) {
      mpz_clear(d.minExp); mpz_clear(d.expWidth);
      mpz_clear(c.a); mpz_clear(c.e);
      return u3m_bail(c3__exit);
    }
    c3_w q = 0;
    c3_w f = (m > d.precision) ? m - d.precision : 0;
    mpz_init(g);
    if ( (d.eMode != c3__i) &&
         (mpz_cmp(c.e, d.minExp) < 0) ) {
      mpz_sub(g, d.minExp, c.e);
      if ( !mpz_fits_uint_p(g) ) {
        mpz_clear(g);
        mpz_clear(d.minExp); mpz_clear(d.expWidth);
        mpz_clear(c.a); mpz_clear(c.e);
        return u3m_bail(c3__exit);
      }
      q = mpz_get_ui(g);
    }
    q = c3_max(f, q);
    mpz_init(v);
    mpz_tdiv_r_2exp(v, c.a, q);
    mpz_tdiv_q_2exp(c.a, c.a, q);
    mpz_add_ui(c.e, c.e, q);
    mpz_init_set_ui(h, 1);
    if ( q > 0 ) mpz_mul_2exp(h, h, q - 1);

    if ( mpz_sgn(c.a) == 0 ) {
      c3_t y;
      switch ( i ) {
        default:
          mpz_clear(v); mpz_clear(h); mpz_clear(g);
          mpz_clear(d.minExp); mpz_clear(d.expWidth);
          mpz_clear(c.a); mpz_clear(c.e);
          return u3m_bail(c3__exit);
        case c3__fl:
        case c3__sm:
          mpz_set_ui(c.a, 0);
          mpz_set_ui(c.e, 0);
          mpz_clear(v); mpz_clear(h); mpz_clear(g);
          break;
        case c3__ce:
        case c3__lg:
          mpz_set_ui(c.a, 1);
          mpz_set(c.e, d.minExp);
          mpz_clear(v); mpz_clear(h); mpz_clear(g);
          break;
        case c3__ne:
        case c3__nt:
        case c3__na:
          if ( (i != c3__na) && _(j) ) {
            y = (mpz_cmp(v, h) <= 0);
          } else {
            y = (mpz_cmp(v, h) < 0);
          }
          if ( y ) {
            mpz_set_ui(c.a, 0);
            mpz_set_ui(c.e, 0);
          } else {
            mpz_set_ui(c.a, 1);
            mpz_set(c.e, d.minExp);
          }
          mpz_clear(v); mpz_clear(h); mpz_clear(g);
          break;
      }
      goto end;
    }
    _xpd(&c, &d);
    switch ( i ) {
      c3_ws x;
      default:
        mpz_clear(v); mpz_clear(h); mpz_clear(g);
        mpz_clear(d.minExp); mpz_clear(d.expWidth);
        mpz_clear(c.a); mpz_clear(c.e);
        return u3m_bail(c3__exit);
      case c3__fl:
        break;
      case c3__lg:
        mpz_add_ui(c.a, c.a, 1);
        break;
      case c3__sm:
        if ( (mpz_sgn(v) != 0) || !_(j) ) break;
        if ( (mpz_cmp(c.e, d.minExp) == 0) && (d.eMode != c3__i) ) {
          mpz_sub_ui(c.a, c.a, 1);
          break;
        }
        mpz_mul_2exp(g, c.a, 1);
        mpz_sub_ui(g, g, 1);
        if ( mpz_sizeinbase(g, 2) <= d.precision ) {
          mpz_sub_ui(c.e, c.e, 1);
          mpz_set(c.a, g);
        } else {
          mpz_sub_ui(c.a, c.a, 1);
        }
        break;
      case c3__ce:
        if ( (mpz_sgn(v) != 0) || !_(j) ) {
          mpz_add_ui(c.a, c.a, 1);
        }
        break;
      case c3__ne:
        if ( mpz_sgn(v) == 0 ) break;
        x = mpz_cmp(v, h);
        if ( (x == 0) && _(j) ) {
          if ( mpz_odd_p(c.a) ) {
            mpz_add_ui(c.a, c.a, 1);
          }
        }
        else if ( x >= 0 ) {
          mpz_add_ui(c.a, c.a, 1);
        }
        break;
      case c3__na:
      case c3__nt:
        if ( mpz_sgn(v) == 0 ) break;
        x = mpz_cmp(v, h);
        if ( (x < 0) ) break;
        if ( (i == c3__nt) && (x == 0) ) {
          if (!_(j)) mpz_add_ui(c.a, c.a, 1);
        } else {
          mpz_add_ui(c.a, c.a, 1);
        }
        break;
    }
    if ( mpz_sizeinbase(c.a, 2) == (d.precision + 1) ) {
      mpz_tdiv_q_2exp(c.a, c.a, 1);
      mpz_add_ui(c.e, c.e, 1);
    }
    if ( mpz_sgn(c.a) == 0 ) {
      mpz_set_ui(c.e, 0);
      mpz_clear(v); mpz_clear(h); mpz_clear(g);
      goto end;
    }
    mpz_set(g, d.minExp);
    mpz_add(g, g, d.expWidth);
    if ( (d.eMode != c3__i) && (mpz_cmp(g, c.e) < 0) ) {
      mpz_clear(v); mpz_clear(h); mpz_clear(g);
      mpz_clear(d.minExp); mpz_clear(d.expWidth);
      mpz_clear(c.a); mpz_clear(c.e);
      return u3nc(c3__i, c3y);
    }
    mpz_clear(v); mpz_clear(h); mpz_clear(g);

    //  all mpz except in c, d structures cleared; c contains result
    end:
    if ( d.eMode == c3__f ) {
      if ( mpz_sizeinbase(c.a, 2) != d.precision ) {
        mpz_set_ui(c.a, 0);
        mpz_set_ui(c.e, 0);
      }
    }
    u3_noun ret = u3nq(c3__f, c3y, u3k(_mp_to_satom(c.e)), u3k(u3i_mp(c.a)));
    mpz_clear(d.minExp); mpz_clear(d.expWidth);
    return ret;
  }

  u3_noun
  u3wef_lug(u3_noun cor)
  {
    u3_noun a, b, c, d, e;
    a = u3x_at(u3x_sam, cor);
    b = u3x_at(30, cor);
    u3x_trel(a, &c, &d, &e);

    return u3qef_lug(d, b, c, e);
  }

  u3_noun
  u3qef_drg(u3_noun a,
            u3_noun b)
  {
    ea c;
    flOptions d;
    _noun_to_ea(&c, a);
    _noun_to_flOptions(&d, b);
    if ( mpz_sgn(c.a) == 0 ) {
      mpz_clear(d.minExp); mpz_clear(d.expWidth);
      mpz_clear(c.a); mpz_clear(c.e);
      u3m_bail(c3__exit);
    }
    _xpd(&c, &d);
    if ( !mpz_fits_sint_p(c.e) ) {
      mpz_clear(d.minExp); mpz_clear(d.expWidth);
      mpz_clear(c.a); mpz_clear(c.e);
      u3m_bail(c3__exit);
    }
    mpz_t r, s, mn, mp, i, j, u, o;
    mpz_init_set(r, c.a);
    mpz_init_set_ui(s, 1);
    mpz_init_set_ui(mn, 1);
    mpz_init(i);
    mpz_init(j);
    c3_w se = mpz_sgn(c.e);
    if ( se == 1 ) {
      mpz_mul_2exp(r, r, mpz_get_ui(c.e));
      mpz_mul_2exp(mn, mn, mpz_get_ui(c.e));
    }
    else if ( se == -1 ) {
      mpz_mul_2exp(s, s, mpz_get_ui(c.e));
    }
    mpz_init_set(mp, mn);
    mpz_set_ui(i, 1);
    mpz_mul_2exp(i, i, d.precision - 1);
    if ( (mpz_cmp(c.a, i) == 0) &&
         ((mpz_cmp(c.e, d.minExp) != 0 ) ||
         (d.eMode == c3__i)) ) {
      mpz_mul_2exp(mp, mp, 1);
      mpz_mul_2exp(r, r, 1);
      mpz_mul_2exp(s, s, 1);
    }
    mpz_cdiv_q_ui(i, s, 10);
    mpz_set_ui(c.e, 0);
    while ( mpz_cmp(r, i) < 0 ) {
      mpz_sub_ui(c.e, c.e, 1);
      mpz_mul_ui(r, r, 10);
      mpz_mul_ui(mn, mn, 10);
      mpz_mul_ui(mp, mp, 10);
    }
    while ( 1 ) {
      mpz_mul_2exp(i, r, 1);
      mpz_add(i, i, mp);
      mpz_mul_2exp(j, s, 1);
      if ( mpz_cmp(i, j) < 0 ) {
        break;
      }
      mpz_mul_ui(s, s, 10);
      mpz_add_ui(c.e, c.e, 1);
    }
    mpz_init(u);
    mpz_init_set_ui(o, 0);
    while ( 1 ) {
      mpz_sub_ui(c.e, c.e, 1);
      mpz_mul_ui(r, r, 10);
      mpz_mul_ui(mn, mn, 10);
      mpz_mul_ui(mp, mp, 10);
      mpz_tdiv_qr(u, r, r, s);
      mpz_mul_2exp(i, r, 1);
      mpz_mul_2exp(j, s, 1);
      c3_t l = mpz_cmp(i, mn) < 0;
      c3_t h = mpz_cmp(j, mp) < 0;
      if ( !h ) {
        mpz_sub(j, j, mp);
        h = mpz_cmp(i, j) > 0;
      }
      if ( l || h ) {
        mpz_mul_ui(o, o, 10);
        mpz_add(o, o, u);
        if ( h && (!l || (mpz_cmp(i, s) > 0)) ) {
          mpz_add_ui(o, o, 1);
        }
        break;
      }
      mpz_mul_ui(o, o, 10);
      mpz_add(o, o, u);
    }
    mpz_set(c.a, o);
    mpz_clear(r); mpz_clear(s);
    mpz_clear(mn); mpz_clear(mp);
    mpz_clear(i); mpz_clear(j); mpz_clear(u);
    mpz_clear(o); mpz_clear(d.minExp); mpz_clear(d.expWidth);

    return _ea_to_noun(&c);
  }

  u3_noun
  u3wef_drg(u3_noun cor)
  {
    u3_noun a, b;
    a = u3x_at(u3x_sam, cor);
    b = u3x_at(30, cor);

    return u3qef_drg(a, b);
  }
