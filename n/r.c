/* n/r.c
**
** This file is in the public domain.
*/
#include "all.h"

/* _frag_word(): fast fragment/branch prediction for top word.
*/
static u3_weak
_frag_word(c3_w a_w, u3_noun b)
{
  c3_assert(0 != a_w);

  {
    c3_w dep_w = u3_ax_dep(a_w);

    while ( dep_w ) {
      if ( u3_no == u3_co_is_cell(b) ) {
        return u3_none;
      }
      else {
        u3_cs_cell* b_u = u3_co_to_ptr(b);

        b = *(((u3_noun*)&(b_u->hed)) + (1 & (a_w >> (dep_w - 1))));
        dep_w--;
      }
    }
    return b;
  }
}

/* _frag_deep(): fast fragment/branch for deep words.
*/
static u3_weak
_frag_deep(c3_w a_w, u3_noun b)
{
  c3_w dep_w = 32;

  while ( dep_w ) {
    if ( u3_no == u3_co_is_cell(b) ) {
      return u3_none;
    }
    else {
      u3_cs_cell* b_u = u3_co_to_ptr(b);

      b = *(((u3_noun*)&(b_u->hed)) + (1 & (a_w >> (dep_w - 1))));
      dep_w--;
    }
  }
  return b;
}

/* u3_cr_at():
**
**   Return fragment (a) of (b), or u3_none if not applicable.
*/
u3_weak
u3_cr_at(u3_atom a,
         u3_noun b)
{
  c3_assert(u3_none != a);
  c3_assert(u3_none != b);

  if ( 0 == a ) {
    return u3_none;
  }

  if ( u3_so(u3_co_is_cat(a)) ) {
    return _frag_word(a, b);
  }
  else {
    if ( u3_ne(u3_co_is_pug(a)) ) {
      return u3_none;
    }
    else {
      u3_cs_atom* a_u = u3_co_to_ptr(a);
      c3_w len_w      = a_u->len_w;

      b = _frag_word(a_u->buf_w[len_w - 1], b);
      len_w -= 1;

      while ( len_w ) {
        b = _frag_deep(a_u->buf_w[len_w], b);

        if ( u3_none == b ) {
          return b;
        } else {
          len_w--;
        }
      }
      return b;
    }
  }
}

/* u3_cr_mean():
**
**   Attempt to deconstruct `a` by axis, noun pairs; 0 terminates.
**   Axes must be sorted in tree order.
*/
  struct _mean_pair {
    c3_w    axe_w;
    u3_noun* som;
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

      if ( (cut_t == c3_false) && (3 == u3_ax_cap(axe_w)) ) {
        cut_t = c3_true;
        cut_w = i_w;
      }
      prs_m[i_w].axe_w = u3_ax_mas(axe_w);
    }
    return cut_t ? cut_w : i_w;
  }

  static u3_bean
  _mean_extract(u3_noun            som,
                c3_w               len_w,
                struct _mean_pair* prs_m)
  {
    if ( len_w == 0 ) {
      return u3_yes;
    }
    else if ( (len_w == 1) && (1 == prs_m[0].axe_w) ) {
      *prs_m->som = som;
      return u3_yes;
    }
    else {
      if ( u3_no == u3_co_is_cell(som) ) {
        return u3_no;
      } else {
        c3_w cut_w = _mean_cut(len_w, prs_m);

        return u3_and
          (_mean_extract(u3_co_h(som), cut_w, prs_m),
           _mean_extract(u3_co_t(som), (len_w - cut_w), (prs_m + cut_w)));
      }
    }
  }

u3_bean
u3_cr_mean(u3_noun som,
        ...)
{
  va_list            ap;
  c3_w               len_w;
  struct _mean_pair* prs_m;

  c3_assert(u3_none != som);

  /* Count.
  */
  len_w = 0;
  {
    va_start(ap, som);
    while ( 1 ) {
      if ( 0 == va_arg(ap, c3_w) ) {
        break;
      }
      va_arg(ap, u3_noun*);
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
      prs_m[i_w].som = va_arg(ap, u3_noun*);
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

/* u3_cr_mug_both():
**
**   Join two mugs.
*/
c3_w
u3_cr_mug_both(c3_w lef_w, c3_w rit_w)
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
_mug_words_in_buf(c3_w off_w, c3_w nwd_w, u3_noun veb)
{
  u3_cs_atom* veb_u = u3_co_to_ptr(veb);

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
_mug_words_buf(c3_w off_w, c3_w nwd_w, u3_noun veb)
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

/* u3_cr_mug():
**
**   Compute and/or recall the mug (31-bit FNV1a hash) of (a).
*/
c3_w
u3_cr_mug(u3_noun veb)
{
  c3_assert(u3_none != veb);

  if ( u3_so(u3_co_is_cat(veb)) ) {
    c3_w x_w = veb;

    return _mug_words(2166136261, (veb ? 1 : 0), &x_w);
  } else {
    u3_cs_noun* veb_u = u3_co_to_ptr(veb);

    if ( veb_u->mug_w ) {
      return veb_u->mug_w;
    }
    else {
      if ( u3_so(u3_co_is_cell(veb)) ) {
        u3_cs_cell* veb_u = u3_co_to_ptr(veb);
        u3_noun     hed   = veb_u->hed;
        u3_noun     tel   = veb_u->tel;

        veb_u->mug_w = u3_cr_mug_cell(hed, tel);
        return veb_u->mug_w;
      }
      else {
        u3_cs_atom* veb_u = u3_co_to_ptr(veb);
        c3_w        len_w = veb_u->len_w;

        veb_u->mug_w = _mug_words_buf(2166136261, len_w, veb);
        return veb_u->mug_w;
      }
    }
  }
}

/* u3_cr_mug_words():
**
**   Compute the mug of `buf`, `len`, LSW first.
*/
c3_w
u3_cr_mug_words(const c3_w *buf_w,
                c3_w        len_w)
{
  return _mug_words(2166136261, len_w, buf_w);
}

/* u3_cr_mug_string():
**
**   Compute the mug of `a`, LSB first.
*/
c3_w
u3_cr_mug_string(const c3_c *a_c)
{
  return _mug_bytes(2166136261, strlen(a_c), (c3_y *)a_c);
}

/* u3_cr_mug_cell():
**
**   Compute the mug of the cell `[hed tel]`.
*/
c3_w
u3_cr_mug_cell(u3_noun hed,
               u3_noun tel)
{
  c3_w   lus_w = u3_cr_mug(hed);
  c3_w   biq_w = u3_cr_mug(tel);

  return u3_cr_mug_both(lus_w, biq_w);
}

/* u3_cr_mug_trel():
**
**   Compute the mug of `[a b c]`.
*/
c3_w
u3_cr_mug_trel(u3_noun a,
               u3_noun b,
               u3_noun c)
{
  return u3_cr_mug_both(u3_cr_mug(a), u3_cr_mug_both(u3_cr_mug(b), u3_cr_mug(c)));
}

/* u3_cr_mug_qual():
**
**   Compute the mug of `[a b c d]`.
*/
c3_w
u3_cr_mug_qual(u3_noun a,
               u3_noun b,
               u3_noun c,
               u3_noun d)
{
  return u3_cr_mug_both
          (u3_cr_mug(a),
           u3_cr_mug_both(u3_cr_mug(b),
                          u3_cr_mug_both(u3_cr_mug(c), u3_cr_mug(d))));
}

/* _sing_x():
**
**   Yes iff (a) and (b) are the same noun.
*/
static u3_bean
_sing_x(u3_noun a,
        u3_noun b)
{
  c3_assert(u3_none != a);
  c3_assert(u3_none != b);

  if ( a == b ) {
    return u3_yes;
  }
  else {
    if ( u3_so(u3_co_is_atom(a)) ) {
      u3_cs_atom* a_u = u3_co_to_ptr(a);

      if ( !u3_so(u3_co_is_atom(b)) ||
           u3_so(u3_co_is_cat(a)) ||
           u3_so(u3_co_is_cat(b)) )
      {
        return u3_no;
      }
      else {
        u3_cs_atom* b_u = u3_co_to_ptr(b);

        if ( a_u->mug_w &&
             b_u->mug_w &&
             (a_u->mug_w != b_u->mug_w) )
        {
          return u3_no;
        }
        else {
          c3_w w_rez = a_u->len_w;
          c3_w w_mox = b_u->len_w;

          if ( w_rez != w_mox ) {
            return u3_no;
          }
          else {
            c3_w i_w;

            for ( i_w = 0; i_w < w_rez; i_w++ ) {
              if ( a_u->buf_w[i_w] != b_u->buf_w[i_w] ) {
                return u3_no;
              }
            }
            return u3_yes;
          }
        }
      }
    }
    else {
      if ( u3_so(u3_co_is_atom(b)) ) {
        return u3_no;
      }
      else {
        u3_cs_cell* a_u = u3_co_to_ptr(a);
        u3_cs_cell* b_u = u3_co_to_ptr(b);

        if ( a_u->mug_w &&
             b_u->mug_w &&
             (a_u->mug_w != b_u->mug_w) )
        {
          return u3_no;
        }
        else {
          if ( u3_no == _sing_x(u3_co_h(a), u3_co_h(b)) ) {
            return u3_no;
          }
          else if ( u3_no == _sing_x(u3_co_t(a), u3_co_t(b)) ) {
            return u3_no;
          }
          return u3_yes;
        }
      }
    }
  }
}

/* u3_cr_sing():
**
**   Yes iff (a) and (b) are the same noun.
*/
u3_bean
u3_cr_sing(u3_noun a,
           u3_noun b)
{
  return _sing_x(a, b);
}

u3_bean
u3_cr_fing(u3_noun a,
           u3_noun b)
{
  return (a == b) ? u3_yes : u3_no;
}

/* u3_cr_sing_cell():
**
**   Yes iff `[p q]` and `b` are the same noun.
*/
u3_bean
u3_cr_sing_cell(u3_noun p,
                u3_noun q,
                u3_noun b)
{
  return u3_and(u3_so(u3_co_is_cell(b)),
                u3_and(u3_cr_sing(p, u3_co_h(b)),
                       u3_cr_sing(q, u3_co_t(b))));
}
u3_bean
u3_cr_fing_cell(u3_noun p,
                u3_noun q,
                u3_noun b)
{
  return u3_and(u3_so(u3_co_is_cell(b)),
                u3_and(u3_cr_fing(p, u3_co_h(b)),
                       u3_cr_fing(q, u3_co_t(b))));
}

/* u3_cr_sing_mixt():
**
**   Yes iff `[p q]` and `b` are the same noun.
*/
u3_bean
u3_cr_sing_mixt(const c3_c* p_c,
                u3_noun     q,
                u3_noun     b)
{
  return u3_and(u3_so(u3_co_is_cell(b)),
                u3_and(u3_cr_sing_c(p_c, u3_co_h(b)),
                       u3_cr_sing(q, u3_co_t(b))));
}
u3_bean
u3_cr_fing_mixt(const c3_c* p_c,
                u3_noun     q,
                u3_noun     b)
{
  return u3_and(u3_so(u3_co_is_cell(b)),
                u3_and(u3_cr_sing_c(p_c, u3_co_h(b)),
                       u3_cr_fing(q, u3_co_t(b))));
}

/* u3_cr_sing_trel():
**
**   Yes iff `[p q r]` and `b` are the same noun.
*/
u3_bean
u3_cr_sing_trel(u3_noun p,
                u3_noun q,
                u3_noun r,
                u3_noun b)
{
  return u3_and(u3_so(u3_co_is_cell(b)),
                u3_and(u3_cr_sing(p, u3_co_h(b)),
                       u3_cr_sing_cell(q, r, u3_co_t(b))));
}
u3_bean
u3_cr_fing_trel(u3_noun p,
                u3_noun q,
                u3_noun r,
                u3_noun b)
{
  return u3_and(u3_so(u3_co_is_cell(b)),
                u3_and(u3_cr_fing(p, u3_co_h(b)),
                       u3_cr_fing_cell(q, r, u3_co_t(b))));
}

/* u3_cr_sing_qual():
**
**   Yes iff `[p q r]` and `b` are the same noun.
*/
u3_bean
u3_cr_sing_qual(u3_noun p,
                u3_noun q,
                u3_noun r,
                u3_noun s,
                u3_noun b)
{
  return u3_and(u3_so(u3_co_is_cell(b)),
                u3_and(u3_cr_sing(p, u3_co_h(b)),
                       u3_cr_sing_trel(q, r, s, u3_co_t(b))));
}
u3_bean
u3_cr_fing_qual(u3_noun p,
                u3_noun q,
                u3_noun r,
                u3_noun s,
                u3_noun b)
{
  return u3_and(u3_so(u3_co_is_cell(b)),
                u3_and(u3_cr_fing(p, u3_co_h(b)),
                       u3_cr_fing_trel(q, r, s, u3_co_t(b))));
}

/* u3_cr_nord():
**
**   Return 0, 1 or 2 if `a` is below, equal to, or above `b`.
*/
u3_atom
u3_cr_nord(u3_noun a,
        u3_noun b)
{
  c3_assert(u3_none != a);
  c3_assert(u3_none != b);

  if ( a == b ) {
    return 1;
  }
  else {
    if ( u3_so(u3_co_is_atom(a)) ) {
      if ( !u3_so(u3_co_is_atom(b)) ) {
        return 0;
      } else {
        if ( u3_so(u3_co_is_cat(a)) ) {
          if ( u3_so(u3_co_is_cat(b)) ) {
            return (a < b) ? 0 : 2;
          }
          else return 0;
        }
        else if ( u3_so(u3_co_is_cat(b)) ) {
          return 2;
        }
        else {
          u3_cs_atom* a_u = u3_co_to_ptr(a);
          u3_cs_atom* b_u = u3_co_to_ptr(b);

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
      if ( u3_so(u3_co_is_atom(b)) ) {
        return 2;
      } else {
        u3_atom c = u3_cr_nord(u3_co_h(a), u3_co_h(b));

        if ( 1 == c ) {
          return u3_cr_nord(u3_co_t(a), u3_co_t(b));
        } else {
          return c;
        }
      }
    }
  }
}

/* u3_cr_sing_c():
**
**   Yes iff (b) is the same noun as the C string a_c.
*/
u3_bean
u3_cr_sing_c(const c3_c* a_c,
          u3_noun     b)
{
  c3_assert(u3_none != b);

  if ( !u3_so(u3_co_is_atom(b)) ) {
    return u3_no;
  }
  else {
    c3_w w_sof = strlen(a_c);
    c3_w i_w;

    for ( i_w = 0; i_w < w_sof; i_w++ ) {
      if ( u3_cr_byte(i_w, b) != a_c[i_w] ) {
        return u3_no;
      }
    }
    return u3_yes;
  }
}

/* u3_cr_bush():
**
**   Factor [a] as a bush [b.[p q] c].
*/
u3_bean
u3_cr_bush(u3_noun  a,
           u3_noun* b,
           u3_noun* c)
{
  c3_assert(u3_none != a);

  if ( u3_so(u3_co_is_atom(a)) ) {
    return u3_no;
  }
  else {
    *b = u3_co_h(a);

    if ( u3_so(u3_co_is_atom(*b)) ) {
      return u3_no;
    } else {
      *c = u3_co_t(a);
      return u3_yes;
    }
  }
}

/* u3_cr_cell():
**
**   Factor (a) as a cell (b c).
*/
u3_bean
u3_cr_cell(u3_noun  a,
           u3_noun* b,
           u3_noun* c)
{
  c3_assert(u3_none != a);

  if ( u3_so(u3_co_is_atom(a)) ) {
    return u3_no;
  }
  else {
    if ( b ) *b = u3_co_h(a);
    if ( c ) *c = u3_co_t(a);
    return u3_yes;
  }
}

/* u3_cr_p():
**
**   & [0] if [a] is of the form [b *c].
*/
u3_bean
u3_cr_p(u3_noun  a,
        u3_noun  b,
        u3_noun* c)
{
  u3_noun feg, nux;

  if ( (u3_yes == u3_cr_cell(a, &feg, &nux)) &&
       (u3_yes == u3_cr_sing(feg, b)) )
  {
    *c = nux;
    return u3_yes;
  }
  else return u3_no;
}

/* u3_cr_pq():
**
**   & [0] if [a] is of the form [b *c d].
*/
u3_bean
u3_cr_pq(u3_noun  a,
         u3_noun  b,
         u3_noun* c,
         u3_noun* d)
{
  u3_noun nux;

  if ( (u3_yes == u3_cr_p(a, b, &nux)) &&
       (u3_yes == u3_cr_cell(nux, c, d)) )
  {
    return u3_yes;
  }
  else return u3_no;
}

/* u3_cr_pqr():
**
**   & [0] if [a] is of the form [b *c *d *e].
*/
u3_bean
u3_cr_pqr(u3_noun  a,
          u3_noun  b,
          u3_noun* c,
          u3_noun* d,
          u3_noun* e)
{
  u3_noun nux;

  if ( (u3_yes == u3_cr_p(a, b, &nux)) &&
       (u3_yes == u3_cr_trel(nux, c, d, e)) )
  {
    return u3_yes;
  }
  else return u3_no;
}

/* u3_cr_pqrs():
**
**   & [0] if [a] is of the form [b *c *d *e *f].
*/
u3_bean
u3_cr_pqrs(u3_noun  a,
           u3_noun  b,
           u3_noun* c,
           u3_noun* d,
           u3_noun* e,
           u3_noun* f)
{
  u3_noun nux;

  if ( (u3_yes == u3_cr_p(a, b, &nux)) &&
       (u3_yes == u3_cr_qual(nux, c, d, e, f)) )
  {
    return u3_yes;
  }
  else return u3_no;
}

/* u3_cr_trel():
**
**   Factor (a) as a trel (b c d).
*/
u3_bean
u3_cr_trel(u3_noun a,
           u3_noun *b,
           u3_noun *c,
           u3_noun *d)
{
  u3_noun guf;

  if ( (u3_yes == u3_cr_cell(a, b, &guf)) &&
       (u3_yes == u3_cr_cell(guf, c, d)) ) {
    return u3_yes;
  }
  else {
    return u3_no;
  }
}

/* u3_cr_qual():
**
**   Factor (a) as a qual (b c d e).
*/
u3_bean
u3_cr_qual(u3_noun  a,
           u3_noun* b,
           u3_noun* c,
           u3_noun* d,
           u3_noun* e)
{
  u3_noun guf;

  if ( (u3_yes == u3_cr_cell(a, b, &guf)) &&
       (u3_yes == u3_cr_trel(guf, c, d, e)) ) {
    return u3_yes;
  }
  else return u3_no;
}

/* u3_cr_quil():
**
**   Factor (a) as a quil (b c d e f).
*/
u3_bean
u3_cr_quil(u3_noun  a,
           u3_noun* b,
           u3_noun* c,
           u3_noun* d,
           u3_noun* e,
           u3_noun* f)
{
  u3_noun guf;

  if ( (u3_yes == u3_cr_cell(a, b, &guf)) &&
       (u3_yes == u3_cr_qual(guf, c, d, e, f)) ) {
    return u3_yes;
  }
  else return u3_no;
}

/* u3_cr_hext():
**
**   Factor (a) as a hext (b c d e f g)
*/
u3_bean
u3_cr_hext(u3_noun  a,
           u3_noun* b,
           u3_noun* c,
           u3_noun* d,
           u3_noun* e,
           u3_noun* f,
           u3_noun* g)
{
  u3_noun guf;

  if ( (u3_yes == u3_cr_cell(a, b, &guf)) &&
       (u3_yes == u3_cr_quil(guf, c, d, e, f, g)) ) {
    return u3_yes;
  }
  else return u3_no;
}

/* u3_cr_met():
**
**   Return the size of (b) in bits, rounded up to
**   (1 << a_y).
**
**   For example, (a_y == 3) returns the size in bytes.
*/
c3_w
u3_cr_met(c3_y    a_y,
          u3_atom b)
{
  c3_assert(u3_none != b);
  c3_assert(u3_so(u3_co_is_atom(b)));

  if ( b == 0 ) {
    return 0;
  }
  else {
    /* gal_w: number of words besides (daz_w) in (b).
    ** daz_w: top word in (b).
    */
    c3_w gal_w;
    c3_w daz_w;

    if ( u3_so(u3_co_is_cat(b)) ) {
      gal_w = 0;
      daz_w = b;
    }
    else {
      u3_cs_atom* b_u = u3_co_to_ptr(b);

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

/* u3_cr_bit():
**
**   Return bit (a_w) of (b).
*/
c3_b
u3_cr_bit(c3_w    a_w,
          u3_atom b)
{
  c3_assert(u3_none != b);
  c3_assert(u3_so(u3_co_is_atom(b)));

  if ( u3_so(u3_co_is_cat(b)) ) {
    if ( a_w >= 31 ) {
      return 0;
    }
    else return (1 & (b >> a_w));
  }
  else {
    u3_cs_atom* b_u   = u3_co_to_ptr(b);
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

/* u3_cr_byte():
**
**   Return byte (a_w) of (b).
*/
c3_y
u3_cr_byte(c3_w    a_w,
           u3_atom b)
{
  c3_assert(u3_none != b);
  c3_assert(u3_so(u3_co_is_atom(b)));

  if ( u3_so(u3_co_is_cat(b)) ) {
    if ( a_w > 3 ) {
      return 0;
    }
    else return (255 & (b >> (a_w << 3)));
  }
  else {
    u3_cs_atom* b_u   = u3_co_to_ptr(b);
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

/* u3_cr_bytes():
**
**  Copy bytes (a_w) through (a_w + b_w - 1) from (d) to (c).
*/
void
u3_cr_bytes(c3_w    a_w,
            c3_w    b_w,
            c3_y*   c_y,
            u3_atom d)
{
  c3_w i_w;

  c3_assert(u3_none != d);

  /* Efficiency: don't call u3_cr_byte().
  */
  for ( i_w = 0; i_w < b_w; i_w++ ) {
    c_y[i_w] = u3_cr_byte((a_w + i_w), d);
  }
}

/* u3_cr_mp():
**
**   Copy (b) into (a_mp).
*/
void
u3_cr_mp(mpz_t   a_mp,
         u3_atom b)
{
  c3_assert(u3_none != b);
  c3_assert(u3_so(u3_co_is_atom(b)));

  if ( u3_so(u3_co_is_cat(b)) ) {
    mpz_init_set_ui(a_mp, b);
  }
  else {
    u3_cs_atom* b_u   = u3_co_to_ptr(b);
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

/* u3_cr_word():
**
**   Return word (a_w) of (b).
*/
c3_w
u3_cr_word(c3_w    a_w,
           u3_atom b)
{
  c3_assert(u3_none != b);
  c3_assert(u3_so(u3_co_is_atom(b)));

  if ( u3_so(u3_co_is_cat(b)) ) {
    if ( a_w > 0 ) {
      return 0;
    }
    else return b;
  }
  else {
    u3_cs_atom* b_u = u3_co_to_ptr(b);

    if ( a_w >= b_u->len_w ) {
      return 0;
    }
    else return b_u->buf_w[a_w];
  }
}

/* u3_cr_chub():
**
**   Return double-word (a_w) of (b).
*/
c3_d
u3_cr_chub(c3_w  a_w,
           u3_atom b)
{
  c3_w wlo_w = u3_cr_word(a_w * 2, b);
  c3_w whi_w = u3_cr_word(1 + (a_w * 2), b);

  return (((uint64_t)whi_w) << 32ULL) | ((uint64_t)wlo_w);
}

/* u3_cr_words():
**
**  Copy words (a_w) through (a_w + b_w - 1) from (d) to (c).
*/
void
u3_cr_words(c3_w    a_w,
            c3_w    b_w,
            c3_w*   c_w,
            u3_atom d)
{
  c3_w i_w;

  c3_assert(u3_none != d);

  /* Efficiency: don't call u3_cr_word().
  */
  for ( i_w = 0; i_w < b_w; i_w++ ) {
    c_w[i_w] = u3_cr_word((a_w + i_w), d);
  }
}

/* u3_cr_chop():
**
**   Into the bloq space of `met`, from position `fum` for a
**   span of `wid`, to position `tou`, XOR from atom `src`
**   into `dst_w`.
*/
void
u3_cr_chop(c3_g    met_g,
           c3_w    fum_w,
           c3_w    wid_w,
           c3_w    tou_w,
           c3_w*   dst_w,
           u3_atom src)
{
  c3_w  i_w;
  c3_w  len_w;
  c3_w* buf_w;
  
  c3_assert(u3_none != src);
  c3_assert(u3_so(u3_co_is_atom(src)));

  if ( u3_so(u3_co_is_cat(src)) ) {
    len_w = src ? 1 : 0;
    buf_w = &src;
  }
  else {
    u3_cs_atom* src_u = u3_co_to_ptr(src);
   
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

/* u3_cr_string(): `a` as malloced C string.
*/
c3_c*
u3_cr_string(u3_atom a)
{
  c3_w  met_w = u3_cr_met(3, a);
  c3_c* str_c = c3_malloc(met_w + 1);

  u3_cr_bytes(0, met_w, (c3_y*)str_c, a);
  str_c[met_w] = 0;
  return str_c;
}

/* u3_cr_tape(): `a`, a list of bytes, as malloced C string.
*/
c3_y*
u3_cr_tape(u3_noun a)
{
  u3_noun b;
  c3_w    i_w;
  c3_y    *a_y;

  for ( i_w = 0, b=a; u3_yes == u3_co_is_cell(b); i_w++, b=u3_co_t(b) )
    ;
  a_y = c3_malloc(i_w + 1);

  for ( i_w = 0, b=a; u3_yes == u3_co_is_cell(b); i_w++, b=u3_co_t(b) ) {
    a_y[i_w] = u3_co_h(b);
  }
  a_y[i_w] = 0;

  return a_y;
}


#if 0

/* Finalization mix for better avalanching.
*/
static c3_w 
_mur_fmix(c3_w h_w)
{
  h_w ^= h_w >> 16;
  h_w *= 0x85ebca6b;
  h_w ^= h_w >> 13;
  h_w *= 0xc2b2ae35;
  h_w ^= h_w >> 16;

  return h_w;
}

/* _mur_words(): raw MurmurHash3 on raw words.
*/
static c3_w
_mur_words(c3_w syd_w, const c3_w* key_w, c3_w len_w)
{
  c3_w goc_w = syd_w;
  c3_w lig_w = 0xcc9e2d51;
  c3_w duf_w = 0x1b873593;
  c3_w i_w;

  for ( i_w = 0; i_w < len_w; i_w++ ) {
    c3_w kop_w = key_w[i_w];

    kop_w *= lig_w;
    kop_w = c3_rotw(15, kop_w);
    kop_w *= duf_w;

    goc_w ^= kop_w;
    goc_w = c3_rotw(13, goc_w); 
    goc_w = (goc_w * 5) + 0xe6546b64;
  }
  goc_w ^= len_w;
  goc_w = _mur_fmix(goc_w);

  return goc_w;
}

/* u3_mur_words(): 31-bit nonzero MurmurHash3 on raw words.
*/
c3_w
u3_mur_words(const c3_w* key_w, c3_w len_w)
{
  c3_w syd_w = 0xcafebabe;

  while ( 1 ) {
    c3_w haz_w = _mur_words(syd_w, key_w, len_w);
    c3_w ham_w = (haz_w >> 31) ^ (haz_w & 0x7fffffff);

    if ( 0 != ham_w ) return ham_w;
    else syd_w++;
  }
}

/* u3_mur_both():
**
**   Join two murs.
*/
c3_w
u3_mur_both(c3_w lef_w, c3_w rit_w)
{
  c3_w ham_w = lef_w ^ (0x7fffffff ^ rit_w);

  return u3_mur_words(&ham_w, (0 == ham_w) ? 0 : 1);
}

/* u3_mur(): MurmurHash3 on a noun.
*/
c3_w
u3_mur(u3_noun veb)
{
  if ( u3_fly_is_cat(veb) ) {
    return u3_mur_words(&veb, (0 == veb) ? 0 : 1);
  }
  else {
    c3_w mur_w;

    if ( (mur_w=*u3_at_dog_mur(veb)) ) {
      return mur_w;
    }

    if ( u3_dog_is_pom(veb) ) {
      mur_w = u3_mur_both(u3_mur(u3h(veb)), u3_mur(u3t(veb)));
    }
    else {
      c3_w  len_w = u3_met(5, veb);
      c3_w* buf_w = malloc(4 * len_w);

      u3_words(0, len_w, buf_w, veb);
      mur_w = u3_mur_words(buf_w, len_w);

      free(buf_w);
    }

    *u3_at_dog_mur(veb) = mur_w;
    return mur_w;
  }
}

/* u3_mur_string():
**
**   Compute the mur of `a`, LSB first.
*/
c3_w
u3_mur_string(const c3_c *a_c)
{
  c3_w  len_w = strlen(a_c);
  c3_w  wor_w = ((len_w + 3) >> 2);
  c3_w* buf_w = alloca(4 * wor_w);
  c3_w  i_w;

  for ( i_w = 0; i_w < wor_w; i_w++ ) { buf_w[i_w] = 0; }

  for ( i_w = 0; i_w < len_w; i_w++ ) {
    c3_w inx_w = (i_w >> 2);
    c3_w byt_w = (i_w & 3);

    buf_w[inx_w] |= (a_c[i_w] << (8 * byt_w));
  }
  return u3_mur_words(buf_w, wor_w);
}

/* u3_mur_cell():
**
**   Compute the mur of the cell `[hed tel]`.
*/
c3_w
u3_mur_cell(u3_noun hed,
            u3_noun tel)
{
  c3_w   lus_w = u3_mur(hed);
  c3_w   biq_w = u3_mur(tel);

  return u3_mur_both(lus_w, biq_w);
}

/* u3_mur_trel():
**
**   Compute the mur of `[a b c]`.
*/
c3_w
u3_mur_trel(u3_noun a,
            u3_noun b,
            u3_noun c)
{
  return u3_mur_both(u3_mur(a), u3_mur_both(u3_mur(b), u3_mur(c)));
}

/* u3_mur_qual():
**
**   Compute the mur of `[a b c d]`.
*/
c3_w
u3_mur_qual(u3_noun a,
            u3_noun b,
            u3_noun c,
            u3_noun d)
{
  return u3_mur_both(u3_mur(a),
                     u3_mur_both(u3_mur(b),
                                 u3_mur_both(u3_mur(c), u3_mur(d))));
}
#endif

