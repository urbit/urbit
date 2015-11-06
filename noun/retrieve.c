/* g/r.c
**
*/
#include "all.h"

/* _frag_word(): fast fragment/branch prediction for top word.
*/
static u3_weak
_frag_word(c3_w a_w, u3_noun b)
{
  c3_assert(0 != a_w);

  {
    c3_w dep_w = u3x_dep(a_w);

    while ( dep_w ) {
      if ( c3n == u3a_is_cell(b) ) {
        return u3_none;
      }
      else {
        u3a_cell* b_u = u3a_to_ptr(b);

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
    if ( c3n == u3a_is_cell(b) ) {
      return u3_none;
    }
    else {
      u3a_cell* b_u = u3a_to_ptr(b);

      b = *(((u3_noun*)&(b_u->hed)) + (1 & (a_w >> (dep_w - 1))));
      dep_w--;
    }
  }
  return b;
}

/* u3r_at():
**
**   Return fragment (a) of (b), or u3_none if not applicable.
*/
u3_weak
u3r_at(u3_atom a, u3_noun b)
{
  c3_assert(u3_none != a);
  c3_assert(u3_none != b);

  u3t_on(far_o);

  if ( 0 == a ) {
    u3t_off(far_o);
    return u3_none;
  }

  if ( _(u3a_is_cat(a)) ) {
    u3t_off(far_o);
    return _frag_word(a, b);
  }
  else {
    if ( !_(u3a_is_pug(a)) ) {
      u3t_off(far_o);
      return u3_none;
    }
    else {
      u3a_atom* a_u = u3a_to_ptr(a);
      c3_w len_w      = a_u->len_w;

      b = _frag_word(a_u->buf_w[len_w - 1], b);
      len_w -= 1;

      while ( len_w ) {
        b = _frag_deep(a_u->buf_w[len_w - 1], b);

        if ( u3_none == b ) {
          u3t_off(far_o);

          return b;
        } else {
          len_w--;
        }
      }
      u3t_off(far_o);

      return b;
    }
  }
}

/* u3r_mean():
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

    cut_t = 0;
    cut_w = 0;
    for ( i_w = 0; i_w < len_w; i_w++ ) {
      c3_w axe_w = prs_m[i_w].axe_w;

      if ( (cut_t == 0) && (3 == u3x_cap(axe_w)) ) {
        cut_t = 1;
        cut_w = i_w;
      }
      prs_m[i_w].axe_w = u3x_mas(axe_w);
    }
    return cut_t ? cut_w : i_w;
  }

  static c3_o
  _mean_extract(u3_noun            som,
                c3_w               len_w,
                struct _mean_pair* prs_m)
  {
    if ( len_w == 0 ) {
      return c3y;
    }
    else if ( (len_w == 1) && (1 == prs_m[0].axe_w) ) {
      *prs_m->som = som;
      return c3y;
    }
    else {
      if ( c3n == u3a_is_cell(som) ) {
        return c3n;
      } else {
        c3_w cut_w = _mean_cut(len_w, prs_m);

        return c3a
          (_mean_extract(u3a_h(som), cut_w, prs_m),
           _mean_extract(u3a_t(som), (len_w - cut_w), (prs_m + cut_w)));
      }
    }
  }

c3_o
u3r_mean(u3_noun som,
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

/* u3r_mug_both():
**
**   Join two mugs.
*/
c3_w
u3r_mug_both(c3_w lef_w, c3_w rit_w)
{
  return _mug_both(lef_w, rit_w);
}

static __inline__ c3_w
_mug_bytes_in(c3_w off_w, c3_w nby_w, const c3_y* byt_y)
{
  c3_w i_w;

  for ( i_w = 0; i_w < nby_w; i_w++ ) {
    off_w = _mug_fnv(off_w ^ byt_y[i_w]);
  }
  return off_w;
}

static c3_w
_mug_bytes(c3_w off_w, c3_w nby_w, const c3_y* byt_y)
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
  u3a_atom* veb_u = u3a_to_ptr(veb);

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

/* u3r_mug():
**
**   Compute and/or recall the mug (31-bit FNV1a hash) of (a).
*/
c3_w
u3r_mug(u3_noun veb)
{
  c3_assert(u3_none != veb);

  if ( _(u3a_is_cat(veb)) ) {
    c3_w x_w = veb;

    return _mug_words(2166136261U, (veb ? 1 : 0), &x_w);
  } else {
    u3a_noun* veb_u = u3a_to_ptr(veb);

    if ( veb_u->mug_w ) {
      return veb_u->mug_w;
    }
    else {
      if ( _(u3a_is_cell(veb)) ) {
        u3a_cell* veb_u = u3a_to_ptr(veb);
        u3_noun     hed   = veb_u->hed;
        u3_noun     tel   = veb_u->tel;

        veb_u->mug_w = u3r_mug_cell(hed, tel);
        return veb_u->mug_w;
      }
      else {
        u3a_atom* veb_u = u3a_to_ptr(veb);
        c3_w        len_w = veb_u->len_w;

        veb_u->mug_w = _mug_words_buf(2166136261U, len_w, veb);
        return veb_u->mug_w;
      }
    }
  }
}

/* u3r_mug_words():
**
**   Compute the mug of `buf`, `len`, LSW first.
*/
c3_w
u3r_mug_words(const c3_w *buf_w,
                c3_w        len_w)
{
  return _mug_words(2166136261U, len_w, buf_w);
}

/* u3r_mug_d():
**
**   Compute the mug of `num`, LSW first.
*/
c3_w
u3r_mug_d(c3_d num_d)
{
  c3_w buf_w[2];

  buf_w[0] = (c3_w)(num_d & 0xffffffffULL);
  buf_w[1] = (c3_w)(num_d >> 32ULL);

  return u3r_mug_words(buf_w, 2);
}

/* u3r_mug_bytes():
**
**   Compute the mug of `buf`, `len`, LSW first.
*/
c3_w
u3r_mug_bytes(const c3_y *buf_w,
                c3_w        len_w)
{
  return _mug_bytes(2166136261U, len_w, buf_w);
}

/* u3r_mug_string():
**
**   Compute the mug of `a`, LSB first.
*/
c3_w
u3r_mug_string(const c3_c *a_c)
{
  return _mug_bytes(2166136261U, strlen(a_c), (c3_y *)a_c);
}

/* u3r_mug_cell():
**
**   Compute the mug of the cell `[hed tel]`.
*/
c3_w
u3r_mug_cell(u3_noun hed,
               u3_noun tel)
{
  c3_w   lus_w = u3r_mug(hed);
  c3_w   biq_w = u3r_mug(tel);

  return u3r_mug_both(lus_w, biq_w);
}

/* u3r_mug_trel():
**
**   Compute the mug of `[a b c]`.
*/
c3_w
u3r_mug_trel(u3_noun a,
               u3_noun b,
               u3_noun c)
{
  return u3r_mug_both
    (u3r_mug(a), u3r_mug_both(u3r_mug(b), u3r_mug(c)));
}

/* u3r_mug_qual():
**
**   Compute the mug of `[a b c d]`.
*/
c3_w
u3r_mug_qual(u3_noun a,
               u3_noun b,
               u3_noun c,
               u3_noun d)
{
  return u3r_mug_both
          (u3r_mug(a),
           u3r_mug_both(u3r_mug(b),
                          u3r_mug_both(u3r_mug(c), u3r_mug(d))));
}

/* _sang_one(): unify but leak old.
*/
static void
_sang_one(u3_noun* a, u3_noun* b)
{
  if ( *a == *b ) {
    return;
  } 
  else {
    c3_o asr_o = u3a_is_senior(u3R, *a);
    c3_o bsr_o = u3a_is_senior(u3R, *b);

    if ( _(asr_o) && _(bsr_o) ) {
      // You shouldn't have let this happen.  We don't want to
      // descend down to a lower road and free there, because
      // synchronization - though this could be revisited under
      // certain circumstances.
      //
      return;
    }
    if ( _(asr_o) && !_(bsr_o) ){
      // u3z(*b); 
      *b = *a;
    }
    if ( _(bsr_o) && !_(asr_o) ) {
      //  u3z(*a); 
      *a = *b;
    }
    if ( u3a_is_north(u3R) ) {
      if ( *a <= *b ) {
        u3k(*a);
        //  u3z(*b); 
        *b = *a;
      } else {
        u3k(*b);
        //  u3z(*a); 
        *a = *b;
      }
    }
    else {
      if ( *a >= *b ) {
        u3k(*a);
        // u3z(*b); 
        *b = *a;
      } else {
        u3k(*b);
        // u3z(*a); 
        *a = *b;
      }
    }
  }
}

/* _sang_x(): yes if a and b are the same noun, unifying but leaking.
*/
static c3_o
_sang_x(u3_noun a, u3_noun b)
{
  if ( a == b ) {
    return c3y;
  }
  else {
    if ( _(u3a_is_atom(a)) ) {
      u3a_atom* a_u = u3a_to_ptr(a);

      if ( !_(u3a_is_atom(b)) ||
           _(u3a_is_cat(a)) ||
           _(u3a_is_cat(b)) )
      {
        return c3n;
      }
      else {
        u3a_atom* b_u = u3a_to_ptr(b);

        if ( a_u->mug_w &&
             b_u->mug_w &&
             (a_u->mug_w != b_u->mug_w) )
        {
          return c3n;
        }
        else {
          c3_w w_rez = a_u->len_w;
          c3_w w_mox = b_u->len_w;

          if ( w_rez != w_mox ) {
            return c3n;
          }
          else {
            c3_w i_w;

            for ( i_w = 0; i_w < w_rez; i_w++ ) {
              if ( a_u->buf_w[i_w] != b_u->buf_w[i_w] ) {
                return c3n;
              }
            }
            return c3y;
          }
        }
      }
    }
    else {
      if ( _(u3a_is_atom(b)) ) {
        return c3n;
      }
      else {
        u3a_cell* a_u = u3a_to_ptr(a);
        u3a_cell* b_u = u3a_to_ptr(b);

        if ( a_u->mug_w &&
             b_u->mug_w &&
             (a_u->mug_w != b_u->mug_w) )
        {
          return c3n;
        }
        else {
          if ( c3n == _sang_x(a_u->hed, b_u->hed) ) {
            return c3n;
          }
          else {
            _sang_one(&a_u->hed, &b_u->hed);

            if ( c3n == _sang_x(a_u->tel, b_u->tel) ) {
              return c3n;
            }
            else {
              _sang_one(&a_u->tel, &b_u->tel);

              return c3y;
            }
          }
        }
      }
    }
  }
}

/* _sung_one(): pick a unified pointer for identical (a) and (b).
*/
static void
_sung_one(u3_noun* a, u3_noun* b)
{
  if ( *a == *b ) {
    return;
  } 
  else {
    c3_o asr_o = u3a_is_senior(u3R, *a);
    c3_o bsr_o = u3a_is_senior(u3R, *b);

    if ( _(asr_o) && _(bsr_o) ) {
      // You shouldn't have let this happen.  We don't want to
      // descend down to a lower road and free there, because
      // synchronization - though this could be revisited under
      // certain circumstances.
      //
      return;
    }
    if ( _(asr_o) && !_(bsr_o) ){
      u3z(*b); 
      *b = *a;
    }
    if ( _(bsr_o) && !_(asr_o) ) {
      u3z(*a); 
      *a = *b;
    }
    if ( u3a_is_north(u3R) ) {
      if ( *a <= *b ) {
        u3k(*a);
        u3z(*b); 
        *b = *a;
      } else {
        u3k(*b);
        u3z(*a); 
        *a = *b;
      }
    }
    else {
      if ( *a >= *b ) {
        u3k(*a);
        u3z(*b); 
        *b = *a;
      } else {
        u3k(*b);
        u3z(*a); 
        *a = *b;
      }
    }
  }
}

/* _sung_x(): yes if a and b are the same noun, unifying.
*/
static c3_o
_sung_x(u3_noun a, u3_noun b)
{
  if ( a == b ) {
    return c3y;
  }
  else {
    if ( _(u3a_is_atom(a)) ) {
      u3a_atom* a_u = u3a_to_ptr(a);

      if ( !_(u3a_is_atom(b)) ||
           _(u3a_is_cat(a)) ||
           _(u3a_is_cat(b)) )
      {
        return c3n;
      }
      else {
        u3a_atom* b_u = u3a_to_ptr(b);

        if ( a_u->mug_w &&
             b_u->mug_w &&
             (a_u->mug_w != b_u->mug_w) )
        {
          return c3n;
        }
        else {
          c3_w w_rez = a_u->len_w;
          c3_w w_mox = b_u->len_w;

          if ( w_rez != w_mox ) {
            return c3n;
          }
          else {
            c3_w i_w;

            for ( i_w = 0; i_w < w_rez; i_w++ ) {
              if ( a_u->buf_w[i_w] != b_u->buf_w[i_w] ) {
                return c3n;
              }
            }
            return c3y;
          }
        }
      }
    }
    else {
      if ( _(u3a_is_atom(b)) ) {
        return c3n;
      }
      else {
        u3a_cell* a_u = u3a_to_ptr(a);
        u3a_cell* b_u = u3a_to_ptr(b);

        if ( a_u->mug_w &&
             b_u->mug_w &&
             (a_u->mug_w != b_u->mug_w) )
        {
          return c3n;
        }
        else {
          if ( c3n == _sung_x(a_u->hed, b_u->hed) ) {
            return c3n;
          }
          else {
            _sung_one(&a_u->hed, &b_u->hed);

            if ( c3n == _sung_x(a_u->tel, b_u->tel) ) {
              return c3n;
            }
            else {
              _sung_one(&a_u->tel, &b_u->tel);

              return c3y;
            }
          }
        }
      }
    }
  }
}

/* _sing_x():
**
**   Yes iff (a) and (b) are the same noun.
*/
static c3_o
_sing_x(u3_noun a,
        u3_noun b)
{
  c3_assert(u3_none != a);
  c3_assert(u3_none != b);

  if ( a == b ) {
    return c3y;
  }
  else {
    if ( _(u3a_is_atom(a)) ) {
      u3a_atom* a_u = u3a_to_ptr(a);

      if ( !_(u3a_is_atom(b)) ||
           _(u3a_is_cat(a)) ||
           _(u3a_is_cat(b)) )
      {
        return c3n;
      }
      else {
        u3a_atom* b_u = u3a_to_ptr(b);

        if ( a_u->mug_w &&
             b_u->mug_w &&
             (a_u->mug_w != b_u->mug_w) )
        {
          return c3n;
        }
        else {
          c3_w w_rez = a_u->len_w;
          c3_w w_mox = b_u->len_w;

          if ( w_rez != w_mox ) {
            return c3n;
          }
          else {
            c3_w i_w;

            for ( i_w = 0; i_w < w_rez; i_w++ ) {
              if ( a_u->buf_w[i_w] != b_u->buf_w[i_w] ) {
                return c3n;
              }
            }
            return c3y;
          }
        }
      }
    }
    else {
      if ( _(u3a_is_atom(b)) ) {
        return c3n;
      }
      else {
        u3a_cell* a_u = u3a_to_ptr(a);
        u3a_cell* b_u = u3a_to_ptr(b);

        if ( a_u->mug_w &&
             b_u->mug_w &&
             (a_u->mug_w != b_u->mug_w) )
        {
          return c3n;
        }
        else {
          if ( c3n == _sing_x(u3a_h(a), u3a_h(b)) ) {
            return c3n;
          }
          else if ( c3n == _sing_x(u3a_t(a), u3a_t(b)) ) {
            return c3n;
          }
          return c3y;
        }
      }
    }
  }
}

/* u3r_sang(): yes iff (a) and (b) are the same noun, unifying equals.
*/
c3_o
u3r_sang(u3_noun a, u3_noun b)
{
  return _sang_x(a, b);
}

/* u3r_sing():
**
**   Yes iff (a) and (b) are the same noun.
*/
c3_o
u3r_sing(u3_noun a, u3_noun b)
{
#ifndef U3_MEMORY_DEBUG
  if ( u3R->par_p ) {
    return u3r_sang(a, b);
  } 
#endif
  {
    c3_o ret_o;

    u3t_on(euq_o);
    ret_o = _sing_x(a, b);
    u3t_off(euq_o);

    return ret_o;
  }
}

/* u3r_sung(): yes iff (a) and (b) are the same noun, unifying equals.
*/
c3_o
u3r_sung(u3_noun a, u3_noun b)
{
  return _sung_x(a, b);
}

c3_o
u3r_fing(u3_noun a,
           u3_noun b)
{
  return (a == b) ? c3y : c3n;
}

/* u3r_sing_cell():
**
**   Yes iff `[p q]` and `b` are the same noun.
*/
c3_o
u3r_sing_cell(u3_noun p,
                u3_noun q,
                u3_noun b)
{
  return c3a(_(u3a_is_cell(b)),
                c3a(u3r_sing(p, u3a_h(b)),
                       u3r_sing(q, u3a_t(b))));
}
c3_o
u3r_fing_cell(u3_noun p,
                u3_noun q,
                u3_noun b)
{
  return c3a(_(u3a_is_cell(b)),
                c3a(u3r_fing(p, u3a_h(b)),
                       u3r_fing(q, u3a_t(b))));
}

/* u3r_sing_mixt():
**
**   Yes iff `[p q]` and `b` are the same noun.
*/
c3_o
u3r_sing_mixt(const c3_c* p_c,
                u3_noun     q,
                u3_noun     b)
{
  return c3a(_(u3a_is_cell(b)),
                c3a(u3r_sing_c(p_c, u3a_h(b)),
                       u3r_sing(q, u3a_t(b))));
}
c3_o
u3r_fing_mixt(const c3_c* p_c,
                u3_noun     q,
                u3_noun     b)
{
  return c3a(_(u3a_is_cell(b)),
                c3a(u3r_sing_c(p_c, u3a_h(b)),
                       u3r_fing(q, u3a_t(b))));
}

/* u3r_sing_trel():
**
**   Yes iff `[p q r]` and `b` are the same noun.
*/
c3_o
u3r_sing_trel(u3_noun p,
                u3_noun q,
                u3_noun r,
                u3_noun b)
{
  return c3a(_(u3a_is_cell(b)),
                c3a(u3r_sing(p, u3a_h(b)),
                       u3r_sing_cell(q, r, u3a_t(b))));
}
c3_o
u3r_fing_trel(u3_noun p,
                u3_noun q,
                u3_noun r,
                u3_noun b)
{
  return c3a(_(u3a_is_cell(b)),
                c3a(u3r_fing(p, u3a_h(b)),
                       u3r_fing_cell(q, r, u3a_t(b))));
}

/* u3r_sing_qual():
**
**   Yes iff `[p q r]` and `b` are the same noun.
*/
c3_o
u3r_sing_qual(u3_noun p,
                u3_noun q,
                u3_noun r,
                u3_noun s,
                u3_noun b)
{
  return c3a(_(u3a_is_cell(b)),
                c3a(u3r_sing(p, u3a_h(b)),
                       u3r_sing_trel(q, r, s, u3a_t(b))));
}
c3_o
u3r_fing_qual(u3_noun p,
                u3_noun q,
                u3_noun r,
                u3_noun s,
                u3_noun b)
{
  return c3a(_(u3a_is_cell(b)),
                c3a(u3r_fing(p, u3a_h(b)),
                       u3r_fing_trel(q, r, s, u3a_t(b))));
}

/* u3r_nord():
**
**   Return 0, 1 or 2 if `a` is below, equal to, or above `b`.
*/
u3_atom
u3r_nord(u3_noun a,
        u3_noun b)
{
  c3_assert(u3_none != a);
  c3_assert(u3_none != b);

  if ( a == b ) {
    return 1;
  }
  else {
    if ( _(u3a_is_atom(a)) ) {
      if ( !_(u3a_is_atom(b)) ) {
        return 0;
      } else {
        if ( _(u3a_is_cat(a)) ) {
          if ( _(u3a_is_cat(b)) ) {
            return (a < b) ? 0 : 2;
          }
          else return 0;
        }
        else if ( _(u3a_is_cat(b)) ) {
          return 2;
        }
        else {
          u3a_atom* a_u = u3a_to_ptr(a);
          u3a_atom* b_u = u3a_to_ptr(b);

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
      if ( _(u3a_is_atom(b)) ) {
        return 2;
      } else {
        u3_atom c = u3r_nord(u3a_h(a), u3a_h(b));

        if ( 1 == c ) {
          return u3r_nord(u3a_t(a), u3a_t(b));
        } else {
          return c;
        }
      }
    }
  }
}

/* u3r_sing_c():
**
**   Yes iff (b) is the same noun as the C string a_c.
*/
c3_o
u3r_sing_c(const c3_c* a_c,
             u3_noun     b)
{
  c3_assert(u3_none != b);

  if ( !_(u3a_is_atom(b)) ) {
    return c3n;
  }
  else {
    c3_w w_sof = strlen(a_c);
    c3_w i_w;

    if ( w_sof != u3r_met(3, b) ) {
      return c3n;
    }
    for ( i_w = 0; i_w < w_sof; i_w++ ) {
      if ( u3r_byte(i_w, b) != a_c[i_w] ) {
        return c3n;
      }
    }
    return c3y;
  }
}

/* u3r_bush():
**
**   Factor [a] as a bush [b.[p q] c].
*/
c3_o
u3r_bush(u3_noun  a,
           u3_noun* b,
           u3_noun* c)
{
  c3_assert(u3_none != a);

  if ( _(u3a_is_atom(a)) ) {
    return c3n;
  }
  else {
    *b = u3a_h(a);

    if ( _(u3a_is_atom(*b)) ) {
      return c3n;
    } else {
      *c = u3a_t(a);
      return c3y;
    }
  }
}

/* u3r_cell():
**
**   Factor (a) as a cell (b c).
*/
c3_o
u3r_cell(u3_noun  a,
           u3_noun* b,
           u3_noun* c)
{
  c3_assert(u3_none != a);

  if ( _(u3a_is_atom(a)) ) {
    return c3n;
  }
  else {
    if ( b ) *b = u3a_h(a);
    if ( c ) *c = u3a_t(a);
    return c3y;
  }
}

/* u3r_p():
**
**   & [0] if [a] is of the form [b *c].
*/
c3_o
u3r_p(u3_noun  a,
        u3_noun  b,
        u3_noun* c)
{
  u3_noun feg, nux;

  if ( (c3y == u3r_cell(a, &feg, &nux)) &&
       (c3y == u3r_sing(feg, b)) )
  {
    *c = nux;
    return c3y;
  }
  else return c3n;
}

/* u3r_pq():
**
**   & [0] if [a] is of the form [b *c d].
*/
c3_o
u3r_pq(u3_noun  a,
         u3_noun  b,
         u3_noun* c,
         u3_noun* d)
{
  u3_noun nux;

  if ( (c3y == u3r_p(a, b, &nux)) &&
       (c3y == u3r_cell(nux, c, d)) )
  {
    return c3y;
  }
  else return c3n;
}

/* u3r_pqr():
**
**   & [0] if [a] is of the form [b *c *d *e].
*/
c3_o
u3r_pqr(u3_noun  a,
          u3_noun  b,
          u3_noun* c,
          u3_noun* d,
          u3_noun* e)
{
  u3_noun nux;

  if ( (c3y == u3r_p(a, b, &nux)) &&
       (c3y == u3r_trel(nux, c, d, e)) )
  {
    return c3y;
  }
  else return c3n;
}

/* u3r_pqrs():
**
**   & [0] if [a] is of the form [b *c *d *e *f].
*/
c3_o
u3r_pqrs(u3_noun  a,
           u3_noun  b,
           u3_noun* c,
           u3_noun* d,
           u3_noun* e,
           u3_noun* f)
{
  u3_noun nux;

  if ( (c3y == u3r_p(a, b, &nux)) &&
       (c3y == u3r_qual(nux, c, d, e, f)) )
  {
    return c3y;
  }
  else return c3n;
}

/* u3r_trel():
**
**   Factor (a) as a trel (b c d).
*/
c3_o
u3r_trel(u3_noun a,
           u3_noun *b,
           u3_noun *c,
           u3_noun *d)
{
  u3_noun guf;

  if ( (c3y == u3r_cell(a, b, &guf)) &&
       (c3y == u3r_cell(guf, c, d)) ) {
    return c3y;
  }
  else {
    return c3n;
  }
}

/* u3r_qual():
**
**   Factor (a) as a qual (b c d e).
*/
c3_o
u3r_qual(u3_noun  a,
           u3_noun* b,
           u3_noun* c,
           u3_noun* d,
           u3_noun* e)
{
  u3_noun guf;

  if ( (c3y == u3r_cell(a, b, &guf)) &&
       (c3y == u3r_trel(guf, c, d, e)) ) {
    return c3y;
  }
  else return c3n;
}

/* u3r_quil():
**
**   Factor (a) as a quil (b c d e f).
*/
c3_o
u3r_quil(u3_noun  a,
           u3_noun* b,
           u3_noun* c,
           u3_noun* d,
           u3_noun* e,
           u3_noun* f)
{
  u3_noun guf;

  if ( (c3y == u3r_cell(a, b, &guf)) &&
       (c3y == u3r_qual(guf, c, d, e, f)) ) {
    return c3y;
  }
  else return c3n;
}

/* u3r_hext():
**
**   Factor (a) as a hext (b c d e f g)
*/
c3_o
u3r_hext(u3_noun  a,
           u3_noun* b,
           u3_noun* c,
           u3_noun* d,
           u3_noun* e,
           u3_noun* f,
           u3_noun* g)
{
  u3_noun guf;

  if ( (c3y == u3r_cell(a, b, &guf)) &&
       (c3y == u3r_quil(guf, c, d, e, f, g)) ) {
    return c3y;
  }
  else return c3n;
}

/* u3r_met():
**
**   Return the size of (b) in bits, rounded up to
**   (1 << a_y).
**
**   For example, (a_y == 3) returns the size in bytes.
*/
c3_w
u3r_met(c3_y    a_y,
          u3_atom b)
{
  c3_assert(u3_none != b);
  c3_assert(_(u3a_is_atom(b)));

  if ( b == 0 ) {
    return 0;
  }
  else {
    /* gal_w: number of words besides (daz_w) in (b).
    ** daz_w: top word in (b).
    */
    c3_w gal_w;
    c3_w daz_w;

    if ( _(u3a_is_cat(b)) ) {
      gal_w = 0;
      daz_w = b;
    }
    else {
      u3a_atom* b_u = u3a_to_ptr(b);

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

/* u3r_bit():
**
**   Return bit (a_w) of (b).
*/
c3_b
u3r_bit(c3_w    a_w,
          u3_atom b)
{
  c3_assert(u3_none != b);
  c3_assert(_(u3a_is_atom(b)));

  if ( _(u3a_is_cat(b)) ) {
    if ( a_w >= 31 ) {
      return 0;
    }
    else return (1 & (b >> a_w));
  }
  else {
    u3a_atom* b_u   = u3a_to_ptr(b);
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

/* u3r_byte():
**
**   Return byte (a_w) of (b).
*/
c3_y
u3r_byte(c3_w    a_w,
           u3_atom b)
{
  c3_assert(u3_none != b);
  c3_assert(_(u3a_is_atom(b)));

  if ( _(u3a_is_cat(b)) ) {
    if ( a_w > 3 ) {
      return 0;
    }
    else return (255 & (b >> (a_w << 3)));
  }
  else {
    u3a_atom* b_u   = u3a_to_ptr(b);
    c3_y      vut_y = (a_w & 3);
    c3_w      pix_w = (a_w >> 2);

    if ( pix_w >= b_u->len_w ) {
      return 0;
    }
    else {
      c3_w nys_w = b_u->buf_w[pix_w];

      return (255 & (nys_w >> (vut_y << 3)));
    }
  }
}

/* u3r_bytes():
**
**  Copy bytes (a_w) through (a_w + b_w - 1) from (d) to (c).
*/
void
u3r_bytes(c3_w    a_w,
            c3_w    b_w,
            c3_y*   c_y,
            u3_atom d)
{
  c3_assert(u3_none != d);
  c3_assert(_(u3a_is_atom(d)));

  if ( _(u3a_is_cat(d)) ) {
    c3_w e_w = d >> (c3_min(a_w, 4) << 3);
    c3_w m_w = c3_min(b_w, 4);
    memcpy(c_y, (c3_y*)&e_w, m_w);
    if ( b_w > 4 ) {
      memset(c_y + 4, 0, b_w - 4);
    }
  }
  else {
    u3a_atom* d_u   = u3a_to_ptr(d);
    c3_w n_w = d_u->len_w << 2;
    c3_y* x_y = (c3_y*)d_u->buf_w + a_w;

    if ( a_w >= n_w ) {
      memset(c_y, 0, b_w);
    }
    else {
      c3_w z_w = c3_min(b_w, n_w - a_w);
      memcpy(c_y, x_y, z_w);
      if ( b_w > n_w - a_w ) {
        memset(c_y + z_w, 0, b_w + a_w - n_w);
      } 
    }
  }
}

/* u3r_mp():
**
**   Copy (b) into (a_mp).
*/
void
u3r_mp(mpz_t   a_mp,
         u3_atom b)
{
  c3_assert(u3_none != b);
  c3_assert(_(u3a_is_atom(b)));

  if ( _(u3a_is_cat(b)) ) {
    mpz_init_set_ui(a_mp, b);
  }
  else {
    u3a_atom* b_u   = u3a_to_ptr(b);
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

/* u3r_word():
**
**   Return word (a_w) of (b).
*/
c3_w
u3r_word(c3_w    a_w,
           u3_atom b)
{
  c3_assert(u3_none != b);
  c3_assert(_(u3a_is_atom(b)));

  if ( _(u3a_is_cat(b)) ) {
    if ( a_w > 0 ) {
      return 0;
    }
    else return b;
  }
  else {
    u3a_atom* b_u = u3a_to_ptr(b);

    if ( a_w >= b_u->len_w ) {
      return 0;
    }
    else return b_u->buf_w[a_w];
  }
}

/* u3r_chub():
**
**   Return double-word (a_w) of (b).
*/
c3_d
u3r_chub(c3_w  a_w,
           u3_atom b)
{
  c3_w wlo_w = u3r_word(a_w * 2, b);
  c3_w whi_w = u3r_word(1 + (a_w * 2), b);

  return (((uint64_t)whi_w) << 32ULL) | ((uint64_t)wlo_w);
}

/* u3r_words():
**
**  Copy words (a_w) through (a_w + b_w - 1) from (d) to (c).
*/
void
u3r_words(c3_w    a_w,
            c3_w    b_w,
            c3_w*   c_w,
            u3_atom d)
{
  c3_assert(u3_none != d);
  c3_assert(_(u3a_is_atom(d)));

  if ( b_w == 0 ) {
    return;
  }
  if ( _(u3a_is_cat(d)) ) {
    if ( a_w == 0 ) {
      *c_w = d;
      memset((c3_y*)(c_w + 1), 0, (b_w - 1) << 2);
    }
    else {
      memset((c3_y*)c_w, 0, b_w << 2);
    }
  }
  else {
    u3a_atom* d_u = u3a_to_ptr(d);
    if ( a_w >= d_u->len_w ) {
      memset((c3_y*)c_w, 0, b_w << 2);
    }
    else {      
      c3_w z_w = c3_min(b_w, d_u->len_w - a_w);
      c3_w* x_w = d_u->buf_w + a_w;
      memcpy((c3_y*)c_w, (c3_y*)x_w, z_w << 2);
      if ( b_w > d_u->len_w - a_w ) {
        memset((c3_y*)(c_w + z_w), 0, (b_w + a_w - d_u->len_w) << 2);
      } 
    }
  }
}

/* u3r_chop():
**
**   Into the bloq space of `met`, from position `fum` for a
**   span of `wid`, to position `tou`, XOR from atom `src`
**   into `dst_w`.
*/
void
u3r_chop(c3_g    met_g,
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
  c3_assert(_(u3a_is_atom(src)));

  if ( _(u3a_is_cat(src)) ) {
    len_w = src ? 1 : 0;
    buf_w = &src;
  }
  else {
    u3a_atom* src_u = u3a_to_ptr(src);
   
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

/* u3r_string(): `a` as malloced C string.
*/
c3_c*
u3r_string(u3_atom a)
{
  c3_w  met_w = u3r_met(3, a);
  c3_c* str_c = c3_malloc(met_w + 1);

  u3r_bytes(0, met_w, (c3_y*)str_c, a);
  str_c[met_w] = 0;
  return str_c;
}

/* u3r_tape(): `a`, a list of bytes, as malloced C string.
*/
c3_y*
u3r_tape(u3_noun a)
{
  u3_noun b;
  c3_w    i_w;
  c3_y    *a_y;

  for ( i_w = 0, b=a; c3y == u3a_is_cell(b); i_w++, b=u3a_t(b) )
    ;
  a_y = c3_malloc(i_w + 1);

  for ( i_w = 0, b=a; c3y == u3a_is_cell(b); i_w++, b=u3a_t(b) ) {
    a_y[i_w] = u3a_h(b);
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

    if ( u3dog_is_pom(veb) ) {
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

