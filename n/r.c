/* n/r.c
**
** This file is in the public domain.
*/
#include "all.h"

/* _frag_word(): fast fragment/branch prediction for top word.
*/
static u2_weak
_frag_word(c3_w a_w, u2_noun b)
{
  c3_assert(0 != a_w);

  {
    c3_w dep_w = u2_ax_dep(a_w);

    while ( dep_w ) {
      if ( u2_no == u2_co_is_cell(b) ) {
        return u2_none;
      }
      else {
        u2_cs_cell* b_u = u2_co_to_ptr(b);

        b = *(((u2_noun*)&(b_u->hed)) + (1 & (a_w >> (dep_w - 1))));
        dep_w--;
      }
    }
    return b;
  }
}

/* _frag_deep(): fast fragment/branch for deep words.
*/
static u2_weak
_frag_deep(c3_w a_w, u2_noun b)
{
  c3_w dep_w = 32;

  while ( dep_w ) {
    if ( u2_no == u2_co_is_cell(b) ) {
      return u2_none;
    }
    else {
      u2_cs_cell* b_u = u2_co_to_ptr(b);

      b = *(((u2_noun*)&(b_u->hed)) + (1 & (a_w >> (dep_w - 1))));
      dep_w--;
    }
  }
  return b;
}

/* u2_cr_at():
**
**   Return fragment (a) of (b), or u2_none if not applicable.
*/
u2_weak
u2_cr_at(u2_atom a,
         u2_noun b)
{
  c3_assert(u2_none != a);
  c3_assert(u2_none != b);

  if ( 0 == a ) {
    return u2_none;
  }

  if ( u2_so(u2_co_is_cat(a)) ) {
    return _frag_word(a, b);
  }
  else {
    if ( u2_ne(u2_co_is_pug(a)) ) {
      return u2_none;
    }
    else {
      u2_cs_atom* a_u = u2_co_to_ptr(a);
      c3_w len_w      = a_u->len_w;

      b = _frag_word(a_u->buf_w[len_w - 1], b);
      len_w -= 1;

      while ( len_w ) {
        b = _frag_deep(a_u->buf_w[len_w], b);

        if ( u2_none == b ) {
          return b;
        } else {
          len_w--;
        }
      }
      return b;
    }
  }
}

/* u2_cr_mean():
**
**   Attempt to deconstruct `a` by axis, noun pairs; 0 terminates.
**   Axes must be sorted in tree order.
*/
  struct _mean_pair {
    c3_w    axe_w;
    u2_noun* som;
  };

  static c3_w
  _mean_cut(c3_w               len_w,
            struct _mean_pair* prs_m)
  {
    c3_w i_w, cut_t, cut_w;

    cut_t = c3_false;
    cut_w = 0;
    for ( i_w = 0; i_w < len_w; i_w++ ) {
      c3_w axe_w = prs_m[i_w].axe_w;

      if ( (cut_t == c3_false) && (3 == u2_ax_cap(axe_w)) ) {
        cut_t = c3_true;
        cut_w = i_w;
      }
      prs_m[i_w].axe_w = u2_ax_mas(axe_w);
    }
    return cut_t ? cut_w : i_w;
  }

  static u2_bean
  _mean_extract(u2_noun            som,
                c3_w               len_w,
                struct _mean_pair* prs_m)
  {
    if ( len_w == 0 ) {
      return u2_yes;
    }
    else if ( (len_w == 1) && (1 == prs_m[0].axe_w) ) {
      *prs_m->som = som;
      return u2_yes;
    }
    else {
      if ( u2_no == u2_co_is_cell(som) ) {
        return u2_no;
      } else {
        c3_w cut_w = _mean_cut(len_w, prs_m);

        return u2_and
          (_mean_extract(u2_co_h(som), cut_w, prs_m),
           _mean_extract(u2_co_t(som), (len_w - cut_w), (prs_m + cut_w)));
      }
    }
  }

u2_bean
u2_cr_mean(u2_noun som,
        ...)
{
  va_list            ap;
  c3_w               len_w;
  struct _mean_pair* prs_m;

  c3_assert(u2_none != som);

  /* Count.
  */
  len_w = 0;
  {
    va_start(ap, som);
    while ( 1 ) {
      if ( 0 == va_arg(ap, c3_w) ) {
        break;
      }
      va_arg(ap, u2_noun*);
      len_w++;
    }
    va_end(ap);
  }
  prs_m = alloca(len_w * sizeof(struct _mean_pair));

  /* Install.
  */
  {
    c3_w i_w;

    va_start(ap, som);
    for ( i_w = 0; i_w < len_w; i_w++ ) {
      prs_m[i_w].axe_w = va_arg(ap, c3_w);
      prs_m[i_w].som = va_arg(ap, u2_noun*);
    }
    va_end(ap);
  }

  /* Extract.
  */
  return _mean_extract(som, len_w, prs_m);
}

static __inline__ c3_w
_mug_fnv(c3_w has_w)
{
  return (has_w * ((c3_w)16777619));
}

static __inline__ c3_w
_mug_out(c3_w has_w)
{
  return (has_w >> 31) ^ (has_w & 0x7fffffff);
}

static __inline__ c3_w
_mug_both(c3_w lef_w, c3_w rit_w)
{
  c3_w bot_w = _mug_fnv(lef_w ^ _mug_fnv(rit_w));
  c3_w out_w = _mug_out(bot_w);

  if ( 0 != out_w ) {
    return out_w;
  }
  else {
    return _mug_both(lef_w, ++rit_w);
  }
}

/* u2_cr_mug_both():
**
**   Join two mugs.
*/
c3_w
u2_cr_mug_both(c3_w lef_w, c3_w rit_w)
{
  return _mug_both(lef_w, rit_w);
}

static __inline__ c3_w
_mug_bytes_in(c3_w off_w, c3_w nby_w, c3_y* byt_y)
{
  c3_w i_w;

  for ( i_w = 0; i_w < nby_w; i_w++ ) {
    off_w = _mug_fnv(off_w ^ byt_y[i_w]);
  }
  return off_w;
}

static c3_w
_mug_bytes(c3_w off_w, c3_w nby_w, c3_y* byt_y)
{
  c3_w has_w = _mug_bytes_in(off_w, nby_w, byt_y);
  c3_w out_w = _mug_out(has_w);

  if ( 0 != out_w ) {
    return out_w;
  }
  else {
    return _mug_bytes(++off_w, nby_w, byt_y);
  }
}

static __inline__ c3_w
_mug_words_in_buf(c3_w off_w, c3_w nwd_w, u2_noun veb)
{
  u2_cs_atom* veb_u = u2_co_to_ptr(veb);

  if ( 0 == nwd_w ) {
    return off_w;
  } else {
    c3_w i_w, x_w;

    for ( i_w = 0; i_w < (nwd_w - 1); i_w++ ) {
      x_w = veb_u->buf_w[i_w];
      {
        c3_y a_y = (x_w & 0xff);
        c3_y b_y = ((x_w >> 8) & 0xff);
        c3_y c_y = ((x_w >> 16) & 0xff);
        c3_y d_y = ((x_w >> 24) & 0xff);

        off_w = _mug_fnv(off_w ^ a_y);
        off_w = _mug_fnv(off_w ^ b_y);
        off_w = _mug_fnv(off_w ^ c_y);
        off_w = _mug_fnv(off_w ^ d_y);
      }
    }
    x_w = veb_u->buf_w[nwd_w - 1];

    if ( x_w ) {
      off_w = _mug_fnv(off_w ^ (x_w & 0xff));
      x_w >>= 8;

      if ( x_w ) {
        off_w = _mug_fnv(off_w ^ (x_w & 0xff));
        x_w >>= 8;

        if ( x_w ) {
          off_w = _mug_fnv(off_w ^ (x_w & 0xff));
          x_w >>= 8;

          if ( x_w ) {
            off_w = _mug_fnv(off_w ^ (x_w & 0xff));
          }
        }
      }
    }
  }
  return off_w;
}

static __inline__ c3_w
_mug_words_in(c3_w off_w, c3_w nwd_w, const c3_w* wod_w)
{
  if ( 0 == nwd_w ) {
    return off_w;
  } else {
    c3_w i_w, x_w;

    for ( i_w = 0; i_w < (nwd_w - 1); i_w++ ) {
      x_w = wod_w[i_w];
      {
        c3_y a_y = (x_w & 0xff);
        c3_y b_y = ((x_w >> 8) & 0xff);
        c3_y c_y = ((x_w >> 16) & 0xff);
        c3_y d_y = ((x_w >> 24) & 0xff);

        off_w = _mug_fnv(off_w ^ a_y);
        off_w = _mug_fnv(off_w ^ b_y);
        off_w = _mug_fnv(off_w ^ c_y);
        off_w = _mug_fnv(off_w ^ d_y);
      }
    }
    x_w = wod_w[nwd_w - 1];

    if ( x_w ) {
      off_w = _mug_fnv(off_w ^ (x_w & 0xff));
      x_w >>= 8;

      if ( x_w ) {
        off_w = _mug_fnv(off_w ^ (x_w & 0xff));
        x_w >>= 8;

        if ( x_w ) {
          off_w = _mug_fnv(off_w ^ (x_w & 0xff));
          x_w >>= 8;

          if ( x_w ) {
            off_w = _mug_fnv(off_w ^ (x_w & 0xff));
          }
        }
      }
    }
  }
  return off_w;
}

static c3_w
_mug_words(c3_w off_w, c3_w nwd_w, const c3_w* wod_w)
{
  c3_w has_w = _mug_words_in(off_w, nwd_w, wod_w);
  c3_w out_w = _mug_out(has_w);

  if ( 0 != out_w ) {
    return out_w;
  }
  else {
    return _mug_words(++off_w, nwd_w, wod_w);
  }
}

static c3_w
_mug_words_buf(c3_w off_w, c3_w nwd_w, u2_noun veb)
{
  c3_w has_w = _mug_words_in_buf(off_w, nwd_w, veb);
  c3_w out_w = _mug_out(has_w);

  if ( 0 != out_w ) {
    return out_w;
  }
  else {
    return _mug_words_buf(++off_w, nwd_w, veb);
  }
}

/* u2_cr_mug():
**
**   Compute and/or recall the mug (31-bit FNV1a hash) of (a).
*/
c3_w
u2_cr_mug(u2_noun veb)
{
  c3_assert(u2_none != veb);

  if ( u2_so(u2_co_is_cat(veb)) ) {
    c3_w x_w = veb;

    return _mug_words(2166136261, (veb ? 1 : 0), &x_w);
  } else {
    u2_cs_noun* veb_u = u2_co_to_ptr(veb);

    if ( veb_u->mug_w ) {
      return veb_u->mug_w;
    }
    else {
      if ( u2_so(u2_co_is_cell(veb)) ) {
        u2_cs_cell* veb_u = u2_co_to_ptr(veb);
        u2_noun     hed   = veb_u->hed;
        u2_noun     tel   = veb_u->tel;

        veb_u->mug_w = u2_cr_mug_cell(hed, tel);
        return veb_u->mug_w;
      }
      else {
        u2_cs_atom* veb_u = u2_co_to_ptr(veb);
        c3_w        len_w = veb_u->len_w;

        veb_u->mug_w = _mug_words_buf(2166136261, len_w, veb);
        return veb_u->mug_w;
      }
    }
  }
}

/* u2_cr_mug_words():
**
**   Compute the mug of `buf`, `len`, LSW first.
*/
c3_w
u2_cr_mug_words(const c3_w *buf_w,
                c3_w        len_w)
{
  return _mug_words(2166136261, len_w, buf_w);
}

/* u2_cr_mug_string():
**
**   Compute the mug of `a`, LSB first.
*/
c3_w
u2_cr_mug_string(const c3_c *a_c)
{
  return _mug_bytes(2166136261, strlen(a_c), (c3_y *)a_c);
}

/* u2_cr_mug_cell():
**
**   Compute the mug of the cell `[hed tel]`.
*/
c3_w
u2_cr_mug_cell(u2_noun hed,
               u2_noun tel)
{
  c3_w   lus_w = u2_cr_mug(hed);
  c3_w   biq_w = u2_cr_mug(tel);

  return u2_cr_mug_both(lus_w, biq_w);
}

/* u2_cr_mug_trel():
**
**   Compute the mug of `[a b c]`.
*/
c3_w
u2_cr_mug_trel(u2_noun a,
               u2_noun b,
               u2_noun c)
{
  return u2_cr_mug_both(u2_cr_mug(a), u2_cr_mug_both(u2_cr_mug(b), u2_cr_mug(c)));
}

/* u2_cr_mug_qual():
**
**   Compute the mug of `[a b c d]`.
*/
c3_w
u2_cr_mug_qual(u2_noun a,
               u2_noun b,
               u2_noun c,
               u2_noun d)
{
  return u2_cr_mug_both
          (u2_cr_mug(a),
           u2_cr_mug_both(u2_cr_mug(b),
                          u2_cr_mug_both(u2_cr_mug(c), u2_cr_mug(d))));
}

/* _sing_x():
**
**   Yes iff (a) and (b) are the same noun.
*/
static u2_bean
_sing_x(u2_noun a,
        u2_noun b)
{
  c3_assert(u2_none != a);
  c3_assert(u2_none != b);

  if ( a == b ) {
    return u2_yes;
  }
  else {
    if ( u2_so(u2_co_is_atom(a)) ) {
      u2_cs_atom* a_u = u2_co_to_ptr(a);

      if ( !u2_so(u2_co_is_atom(b)) ||
           u2_so(u2_co_is_cat(a)) ||
           u2_so(u2_co_is_cat(b)) )
      {
        return u2_no;
      }
      else {
        u2_cs_atom* b_u = u2_co_to_ptr(b);

        if ( a_u->mug_w &&
             b_u->mug_w &&
             (a_u->mug_w != b_u->mug_w) )
        {
          return u2_no;
        }
        else {
          c3_w w_rez = a_u->len_w;
          c3_w w_mox = b_u->len_w;

          if ( w_rez != w_mox ) {
            return u2_no;
          }
          else {
            c3_w i_w;

            for ( i_w = 0; i_w < w_rez; i_w++ ) {
              if ( a_u->buf_w[i_w] != b_u->buf_w[i_w] ) {
                return u2_no;
              }
            }
            return u2_yes;
          }
        }
      }
    }
    else {
      if ( u2_so(u2_co_is_atom(b)) ) {
        return u2_no;
      }
      else {
        u2_cs_cell* a_u = u2_co_to_ptr(a);
        u2_cs_cell* b_u = u2_co_to_ptr(b);

        if ( a_u->mug_w &&
             b_u->mug_w &&
             (a_u->mug_w != b_u->mug_w) )
        {
          return u2_no;
        }
        else {
          if ( u2_no == _sing_x(u2_co_h(a), u2_co_h(b)) ) {
            return u2_no;
          }
          else if ( u2_no == _sing_x(u2_co_t(a), u2_co_t(b)) ) {
            return u2_no;
          }
          return u2_yes;
        }
      }
    }
  }
}

/* u2_cr_sing():
**
**   Yes iff (a) and (b) are the same noun.
*/
u2_bean
u2_cr_sing(u2_noun a,
           u2_noun b)
{
  return _sing_x(a, b);
}

u2_bean
u2_cr_fing(u2_noun a,
           u2_noun b)
{
  return (a == b) ? u2_yes : u2_no;
}

/* u2_cr_sing_cell():
**
**   Yes iff `[p q]` and `b` are the same noun.
*/
u2_bean
u2_cr_sing_cell(u2_noun p,
                u2_noun q,
                u2_noun b)
{
  return u2_and(u2_so(u2_co_is_cell(b)),
                u2_and(u2_cr_sing(p, u2_co_h(b)),
                       u2_cr_sing(q, u2_co_t(b))));
}
u2_bean
u2_cr_fing_cell(u2_noun p,
                u2_noun q,
                u2_noun b)
{
  return u2_and(u2_so(u2_co_is_cell(b)),
                u2_and(u2_cr_fing(p, u2_co_h(b)),
                       u2_cr_fing(q, u2_co_t(b))));
}

/* u2_cr_sing_mixt():
**
**   Yes iff `[p q]` and `b` are the same noun.
*/
u2_bean
u2_cr_sing_mixt(const c3_c* p_c,
                u2_noun     q,
                u2_noun     b)
{
  return u2_and(u2_so(u2_co_is_cell(b)),
                u2_and(u2_cr_sing_c(p_c, u2_co_h(b)),
                       u2_cr_sing(q, u2_co_t(b))));
}
u2_bean
u2_cr_fing_mixt(const c3_c* p_c,
                u2_noun     q,
                u2_noun     b)
{
  return u2_and(u2_so(u2_co_is_cell(b)),
                u2_and(u2_cr_sing_c(p_c, u2_co_h(b)),
                       u2_cr_fing(q, u2_co_t(b))));
}

/* u2_cr_sing_trel():
**
**   Yes iff `[p q r]` and `b` are the same noun.
*/
u2_bean
u2_cr_sing_trel(u2_noun p,
                u2_noun q,
                u2_noun r,
                u2_noun b)
{
  return u2_and(u2_so(u2_co_is_cell(b)),
                u2_and(u2_cr_sing(p, u2_co_h(b)),
                       u2_cr_sing_cell(q, r, u2_co_t(b))));
}
u2_bean
u2_cr_fing_trel(u2_noun p,
                u2_noun q,
                u2_noun r,
                u2_noun b)
{
  return u2_and(u2_so(u2_co_is_cell(b)),
                u2_and(u2_cr_fing(p, u2_co_h(b)),
                       u2_cr_fing_cell(q, r, u2_co_t(b))));
}

/* u2_cr_sing_qual():
**
**   Yes iff `[p q r]` and `b` are the same noun.
*/
u2_bean
u2_cr_sing_qual(u2_noun p,
                u2_noun q,
                u2_noun r,
                u2_noun s,
                u2_noun b)
{
  return u2_and(u2_so(u2_co_is_cell(b)),
                u2_and(u2_cr_sing(p, u2_co_h(b)),
                       u2_cr_sing_trel(q, r, s, u2_co_t(b))));
}
u2_bean
u2_cr_fing_qual(u2_noun p,
                u2_noun q,
                u2_noun r,
                u2_noun s,
                u2_noun b)
{
  return u2_and(u2_so(u2_co_is_cell(b)),
                u2_and(u2_cr_fing(p, u2_co_h(b)),
                       u2_cr_fing_trel(q, r, s, u2_co_t(b))));
}

/* u2_cr_nord():
**
**   Return 0, 1 or 2 if `a` is below, equal to, or above `b`.
*/
u2_atom
u2_cr_nord(u2_noun a,
        u2_noun b)
{
  c3_assert(u2_none != a);
  c3_assert(u2_none != b);

  if ( a == b ) {
    return 1;
  }
  else {
    if ( u2_so(u2_co_is_atom(a)) ) {
      if ( !u2_so(u2_co_is_atom(b)) ) {
        return 0;
      } else {
        if ( u2_so(u2_co_is_cat(a)) ) {
          if ( u2_so(u2_co_is_cat(b)) ) {
            return (a < b) ? 0 : 2;
          }
          else return 0;
        }
        else if ( u2_so(u2_co_is_cat(b)) ) {
          return 2;
        }
        else {
          u2_cs_atom* a_u = u2_co_to_ptr(a);
          u2_cs_atom* b_u = u2_co_to_ptr(b);

          c3_w w_rez = a_u->len_w;
          c3_w w_mox = b_u->len_w;

          if ( w_rez != w_mox ) {
            return (w_rez < w_mox) ? 0 : 2;
          }
          else {
            c3_w i_w;

            for ( i_w = 0; i_w < w_rez; i_w++ ) {
              c3_w ai_w = a_u->buf_w[i_w];
              c3_w bi_w = b_u->buf_w[i_w];

              if ( ai_w != bi_w ) {
                return (ai_w < bi_w) ? 0 : 2;
              }
            }
            return 1;
          }
        }
      }
    } else {
      if ( u2_so(u2_co_is_atom(b)) ) {
        return 2;
      } else {
        u2_atom c = u2_cr_nord(u2_co_h(a), u2_co_h(b));

        if ( 1 == c ) {
          return u2_cr_nord(u2_co_t(a), u2_co_t(b));
        } else {
          return c;
        }
      }
    }
  }
}

/* u2_cr_sing_c():
**
**   Yes iff (b) is the same noun as the C string a_c.
*/
u2_bean
u2_cr_sing_c(const c3_c* a_c,
          u2_noun     b)
{
  c3_assert(u2_none != b);

  if ( !u2_so(u2_co_is_atom(b)) ) {
    return u2_no;
  }
  else {
    c3_w w_sof = strlen(a_c);
    c3_w i_w;

    for ( i_w = 0; i_w < w_sof; i_w++ ) {
      if ( u2_cr_byte(i_w, b) != a_c[i_w] ) {
        return u2_no;
      }
    }
    return u2_yes;
  }
}

/* u2_cr_bush():
**
**   Factor [a] as a bush [b.[p q] c].
*/
u2_bean
u2_cr_bush(u2_noun  a,
           u2_noun* b,
           u2_noun* c)
{
  c3_assert(u2_none != a);

  if ( u2_so(u2_co_is_atom(a)) ) {
    return u2_no;
  }
  else {
    *b = u2_co_h(a);

    if ( u2_so(u2_co_is_atom(*b)) ) {
      return u2_no;
    } else {
      *c = u2_co_t(a);
      return u2_yes;
    }
  }
}

/* u2_cr_cell():
**
**   Factor (a) as a cell (b c).
*/
u2_bean
u2_cr_cell(u2_noun  a,
           u2_noun* b,
           u2_noun* c)
{
  c3_assert(u2_none != a);

  if ( u2_so(u2_co_is_atom(a)) ) {
    return u2_no;
  }
  else {
    if ( b ) *b = u2_co_h(a);
    if ( c ) *c = u2_co_t(a);
    return u2_yes;
  }
}

/* u2_cr_p():
**
**   & [0] if [a] is of the form [b *c].
*/
u2_bean
u2_cr_p(u2_noun  a,
        u2_noun  b,
        u2_noun* c)
{
  u2_noun feg, nux;

  if ( (u2_yes == u2_cr_cell(a, &feg, &nux)) &&
       (u2_yes == u2_cr_sing(feg, b)) )
  {
    *c = nux;
    return u2_yes;
  }
  else return u2_no;
}

/* u2_cr_pq():
**
**   & [0] if [a] is of the form [b *c d].
*/
u2_bean
u2_cr_pq(u2_noun  a,
         u2_noun  b,
         u2_noun* c,
         u2_noun* d)
{
  u2_noun nux;

  if ( (u2_yes == u2_cr_p(a, b, &nux)) &&
       (u2_yes == u2_cr_cell(nux, c, d)) )
  {
    return u2_yes;
  }
  else return u2_no;
}

/* u2_cr_pqr():
**
**   & [0] if [a] is of the form [b *c *d *e].
*/
u2_bean
u2_cr_pqr(u2_noun  a,
          u2_noun  b,
          u2_noun* c,
          u2_noun* d,
          u2_noun* e)
{
  u2_noun nux;

  if ( (u2_yes == u2_cr_p(a, b, &nux)) &&
       (u2_yes == u2_cr_trel(nux, c, d, e)) )
  {
    return u2_yes;
  }
  else return u2_no;
}

/* u2_cr_pqrs():
**
**   & [0] if [a] is of the form [b *c *d *e *f].
*/
u2_bean
u2_cr_pqrs(u2_noun  a,
           u2_noun  b,
           u2_noun* c,
           u2_noun* d,
           u2_noun* e,
           u2_noun* f)
{
  u2_noun nux;

  if ( (u2_yes == u2_cr_p(a, b, &nux)) &&
       (u2_yes == u2_cr_qual(nux, c, d, e, f)) )
  {
    return u2_yes;
  }
  else return u2_no;
}

/* u2_cr_trel():
**
**   Factor (a) as a trel (b c d).
*/
u2_bean
u2_cr_trel(u2_noun a,
           u2_noun *b,
           u2_noun *c,
           u2_noun *d)
{
  u2_noun guf;

  if ( (u2_yes == u2_cr_cell(a, b, &guf)) &&
       (u2_yes == u2_cr_cell(guf, c, d)) ) {
    return u2_yes;
  }
  else {
    return u2_no;
  }
}

/* u2_cr_qual():
**
**   Factor (a) as a qual (b c d e).
*/
u2_bean
u2_cr_qual(u2_noun  a,
           u2_noun* b,
           u2_noun* c,
           u2_noun* d,
           u2_noun* e)
{
  u2_noun guf;

  if ( (u2_yes == u2_cr_cell(a, b, &guf)) &&
       (u2_yes == u2_cr_trel(guf, c, d, e)) ) {
    return u2_yes;
  }
  else return u2_no;
}

/* u2_cr_quil():
**
**   Factor (a) as a quil (b c d e f).
*/
u2_bean
u2_cr_quil(u2_noun  a,
           u2_noun* b,
           u2_noun* c,
           u2_noun* d,
           u2_noun* e,
           u2_noun* f)
{
  u2_noun guf;

  if ( (u2_yes == u2_cr_cell(a, b, &guf)) &&
       (u2_yes == u2_cr_qual(guf, c, d, e, f)) ) {
    return u2_yes;
  }
  else return u2_no;
}

/* u2_cr_hext():
**
**   Factor (a) as a hext (b c d e f g)
*/
u2_bean
u2_cr_hext(u2_noun  a,
           u2_noun* b,
           u2_noun* c,
           u2_noun* d,
           u2_noun* e,
           u2_noun* f,
           u2_noun* g)
{
  u2_noun guf;

  if ( (u2_yes == u2_cr_cell(a, b, &guf)) &&
       (u2_yes == u2_cr_quil(guf, c, d, e, f, g)) ) {
    return u2_yes;
  }
  else return u2_no;
}

/* u2_cr_met():
**
**   Return the size of (b) in bits, rounded up to
**   (1 << a_y).
**
**   For example, (a_y == 3) returns the size in bytes.
*/
c3_w
u2_cr_met(c3_y    a_y,
          u2_atom b)
{
  c3_assert(u2_none != b);
  c3_assert(u2_so(u2_co_is_atom(b)));

  if ( b == 0 ) {
    return 0;
  }
  else {
    /* gal_w: number of words besides (daz_w) in (b).
    ** daz_w: top word in (b).
    */
    c3_w gal_w;
    c3_w daz_w;

    if ( u2_so(u2_co_is_cat(b)) ) {
      gal_w = 0;
      daz_w = b;
    }
    else {
      u2_cs_atom* b_u = u2_co_to_ptr(b);

      gal_w = (b_u->len_w) - 1;
      daz_w = b_u->buf_w[gal_w];
    }

    switch ( a_y ) {
      case 0:
      case 1:
      case 2: {
        /* col_w: number of bits in (daz_w)
        ** bif_w: number of bits in (b)
        */
        c3_w bif_w, col_w;

        col_w = c3_bits_word(daz_w);
        bif_w = col_w + (gal_w << 5);

        return (bif_w + ((1 << a_y) - 1)) >> a_y;
      }
      case 3: {
        return  (gal_w << 2)
              + ((daz_w >> 24) ? 4 : (daz_w >> 16) ? 3 : (daz_w >> 8) ? 2 : 1);
      }
      case 4: {
        return  (gal_w << 1)
              + ((daz_w >> 16) ? 2 : 1);
      }
      default: {
        c3_y gow_y = (a_y - 5);

        return ((gal_w + 1) + ((1 << gow_y) - 1)) >> gow_y;
      }
    }
  }
}

/* u2_cr_bit():
**
**   Return bit (a_w) of (b).
*/
c3_b
u2_cr_bit(c3_w    a_w,
          u2_atom b)
{
  c3_assert(u2_none != b);
  c3_assert(u2_so(u2_co_is_atom(b)));

  if ( u2_so(u2_co_is_cat(b)) ) {
    if ( a_w >= 31 ) {
      return 0;
    }
    else return (1 & (b >> a_w));
  }
  else {
    u2_cs_atom* b_u   = u2_co_to_ptr(b);
    c3_y        vut_y = (a_w & 31);
    c3_w        pix_w = (a_w >> 5);

    if ( pix_w >= b_u->len_w ) {
      return 0;
    }
    else {
      c3_w nys_w = b_u->buf_w[pix_w];

      return (1 & (nys_w >> vut_y));
    }
  }
}

/* u2_cr_byte():
**
**   Return byte (a_w) of (b).
*/
c3_y
u2_cr_byte(c3_w    a_w,
           u2_atom b)
{
  c3_assert(u2_none != b);
  c3_assert(u2_so(u2_co_is_atom(b)));

  if ( u2_so(u2_co_is_cat(b)) ) {
    if ( a_w > 3 ) {
      return 0;
    }
    else return (255 & (b >> (a_w << 3)));
  }
  else {
    u2_cs_atom* b_u   = u2_co_to_ptr(b);
    c3_y        vut_y = (a_w & 3);
    c3_w        pix_w = (a_w >> 2);

    if ( pix_w >= b_u->len_w ) {
      return 0;
    }
    else {
      c3_w nys_w = b_u->buf_w[pix_w];

      return (255 & (nys_w >> (vut_y << 3)));
    }
  }
}

/* u2_cr_bytes():
**
**  Copy bytes (a_w) through (a_w + b_w - 1) from (d) to (c).
*/
void
u2_cr_bytes(c3_w    a_w,
            c3_w    b_w,
            c3_y*   c_y,
            u2_atom d)
{
  c3_w i_w;

  c3_assert(u2_none != d);

  /* Efficiency: don't call u2_cr_byte().
  */
  for ( i_w = 0; i_w < b_w; i_w++ ) {
    c_y[i_w] = u2_cr_byte((a_w + i_w), d);
  }
}

/* u2_cr_mp():
**
**   Copy (b) into (a_mp).
*/
void
u2_cr_mp(mpz_t   a_mp,
         u2_atom b)
{
  c3_assert(u2_none != b);
  c3_assert(u2_so(u2_co_is_atom(b)));

  if ( u2_so(u2_co_is_cat(b)) ) {
    mpz_init_set_ui(a_mp, b);
  }
  else {
    u2_cs_atom* b_u   = u2_co_to_ptr(b);
    c3_w        len_w = b_u->len_w;

    /* Slight deficiency in the GMP API.
    */
    c3_assert(!(len_w >> 27));
    mpz_init2(a_mp, len_w << 5);

    /* Efficiency: horrible.
    */
    {
      c3_w *buf_w = alloca(len_w << 2);
      c3_w i_w;

      for ( i_w=0; i_w < len_w; i_w++ ) {
        buf_w[i_w] = b_u->buf_w[i_w];
      }
      mpz_import(a_mp, len_w, -1, 4, 0, 0, buf_w);
    }
  }
}

/* u2_cr_word():
**
**   Return word (a_w) of (b).
*/
c3_w
u2_cr_word(c3_w    a_w,
           u2_atom b)
{
  c3_assert(u2_none != b);
  c3_assert(u2_so(u2_co_is_atom(b)));

  if ( u2_so(u2_co_is_cat(b)) ) {
    if ( a_w > 0 ) {
      return 0;
    }
    else return b;
  }
  else {
    u2_cs_atom* b_u = u2_co_to_ptr(b);

    if ( a_w >= b_u->len_w ) {
      return 0;
    }
    else return b_u->buf_w[a_w];
  }
}

/* u2_cr_chub():
**
**   Return double-word (a_w) of (b).
*/
c3_d
u2_cr_chub(c3_w  a_w,
           u2_atom b)
{
  c3_w wlo_w = u2_cr_word(a_w * 2, b);
  c3_w whi_w = u2_cr_word(1 + (a_w * 2), b);

  return (((uint64_t)whi_w) << 32ULL) | ((uint64_t)wlo_w);
}

/* u2_cr_words():
**
**  Copy words (a_w) through (a_w + b_w - 1) from (d) to (c).
*/
void
u2_cr_words(c3_w    a_w,
            c3_w    b_w,
            c3_w*   c_w,
            u2_atom d)
{
  c3_w i_w;

  c3_assert(u2_none != d);

  /* Efficiency: don't call u2_cr_word().
  */
  for ( i_w = 0; i_w < b_w; i_w++ ) {
    c_w[i_w] = u2_cr_word((a_w + i_w), d);
  }
}

/* u2_cr_chop():
**
**   Into the bloq space of `met`, from position `fum` for a
**   span of `wid`, to position `tou`, XOR from atom `src`
**   into `dst_w`.
*/
void
u2_cr_chop(c3_g    met_g,
           c3_w    fum_w,
           c3_w    wid_w,
           c3_w    tou_w,
           c3_w*   dst_w,
           u2_atom src)
{
  c3_w  i_w;
  c3_w  len_w;
  c3_w* buf_w;
  
  c3_assert(u2_none != src);
  c3_assert(u2_so(u2_co_is_atom(src)));

  if ( u2_so(u2_co_is_cat(src)) ) {
    len_w = src ? 1 : 0;
    buf_w = &src;
  }
  else {
    u2_cs_atom* src_u = u2_co_to_ptr(src);
   
    len_w = src_u->len_w;
    buf_w = src_u->buf_w;
  }

  if ( met_g < 5 ) {
    c3_w san_w = (1 << met_g);
    c3_w mek_w = ((1 << san_w) - 1);
    c3_w baf_w = (fum_w << met_g);
    c3_w bat_w = (tou_w << met_g);

    // XX: efficiency: poor.  Iterate by words.
    //
    for ( i_w = 0; i_w < wid_w; i_w++ ) {
      c3_w waf_w = (baf_w >> 5);
      c3_g raf_g = (baf_w & 31);
      c3_w wat_w = (bat_w >> 5);
      c3_g rat_g = (bat_w & 31);
      c3_w hop_w;

      hop_w = (waf_w >= len_w) ? 0 : buf_w[waf_w];
      hop_w = (hop_w >> raf_g) & mek_w;

      dst_w[wat_w] ^= (hop_w << rat_g);

      baf_w += san_w;
      bat_w += san_w;
    }
  }
  else {
    c3_g hut_g = (met_g - 5);
    c3_w san_w = (1 << hut_g);
    c3_w j_w;

    for ( i_w = 0; i_w < wid_w; i_w++ ) {
      c3_w wuf_w = (fum_w + i_w) << hut_g;
      c3_w wut_w = (tou_w + i_w) << hut_g;

      for ( j_w = 0; j_w < san_w; j_w++ ) {
        dst_w[wut_w + j_w] ^=
            ((wuf_w + j_w) >= len_w)
              ? 0
              : buf_w[wuf_w + j_w];
      }
    }
  }
}

/* u2_cr_string(): `a` as malloced C string.
*/
c3_c*
u2_cr_string(u2_atom a)
{
  c3_w  met_w = u2_cr_met(3, a);
  c3_c* str_c = c3_malloc(met_w + 1);

  u2_cr_bytes(0, met_w, (c3_y*)str_c, a);
  str_c[met_w] = 0;
  return str_c;
}

/* u2_cr_tape(): `a`, a list of bytes, as malloced C string.
*/
c3_y*
u2_cr_tape(u2_noun a)
{
  u2_noun b;
  c3_w    i_w;
  c3_y    *a_y;

  for ( i_w = 0, b=a; u2_yes == u2_co_is_cell(b); i_w++, b=u2_co_t(b) )
    ;
  a_y = c3_malloc(i_w + 1);

  for ( i_w = 0, b=a; u2_yes == u2_co_is_cell(b); i_w++, b=u2_co_t(b) ) {
    a_y[i_w] = u2_co_h(b);
  }
  a_y[i_w] = 0;

  return a_y;
}

