/* j/5/cue.c
**
*/
#include "all.h"

typedef struct cueframe
{
  c3_y    tag_y;
  u3_atom cur;
  u3_atom wid;
  u3_noun hed;
} cueframe;

static inline void
_cue_push(c3_ys   mov,
          c3_ys   off,
          c3_y    tag_y,
          u3_atom cur,
          u3_atom wid,
          u3_noun hed)
{
  u3R->cap_p += mov;

  //  ensure we haven't overflowed the stack
  //  (off==0 means we're on a north road)
  //
  if ( 0 == off ) {
    c3_assert(u3R->cap_p > u3R->hat_p);
  }
  else {
    c3_assert(u3R->cap_p < u3R->hat_p);
  }

  cueframe* fam_u = u3to(cueframe, u3R->cap_p + off);
  fam_u->tag_y = tag_y;
  fam_u->cur   = cur;
  fam_u->wid   = wid;
  fam_u->hed   = hed;
}

static inline cueframe*
_cue_pop(c3_ys mov, c3_ys off)
{
  cueframe* fam_u = u3to(cueframe, u3R->cap_p + off);
  u3R->cap_p -= mov;

  return fam_u;
}

/* _cue_rub(): rub, retaining target (b)
*/
static inline u3_atom
_cue_rub(u3_atom a, u3_atom b)
{
  u3_atom pro = u3qe_rub(a, b);
  u3z(a);
  return pro;
}

u3_noun
u3qe_cue(u3_atom a)
{
  c3_y  wis_y = c3_wiseof(cueframe);
  c3_o  nor_o = u3a_is_north(u3R);
  c3_ys mov   = ( c3y == nor_o ? -wis_y : wis_y );
  c3_ys off   = ( c3y == nor_o ? 0 : -wis_y );

# define CUE_ROOT 0
# define CUE_HEAD 1
# define CUE_TAIL 2

  //  stash the current stack post
  //
  u3p(cueframe) cap_p = u3R->cap_p;
  _cue_push(mov, off, CUE_ROOT, 0, 0, 0);

  u3p(u3h_root) har_p = u3h_new();

  u3_atom cur = 0;
  u3_atom wid = 0;
  u3_noun pro = u3_none;

  //  read from .a at .cur
  //  push on to the stack and continue (cell-head recursion)
  //  or set .wid and .pro and goto give
  //
  pass: {
    while ( 1 ) {
      //  read tag bit at .cur
      //
      c3_y tag_y = u3qc_cut(0, cur, 1, a);

      //  low bit unset, cursor points to an atom
      //
      if ( 0 == tag_y ) {
        u3_noun bur = _cue_rub(u3qa_inc(cur), a);
        pro = u3k(u3t(bur));
        u3h_put(har_p, u3k(cur), u3k(pro));
        wid = u3qa_inc(u3h(bur));

        u3z(bur);
        goto give;
      }
      else {
        //  read tag bit at (1 + cur)
        //
        {
          u3_noun l = u3qa_inc(cur);
          tag_y = u3qc_cut(0, l, 1, a);
          u3z(l);
        }

        //  next bit unset, cursor points to a cell
        //
        if ( 0 == tag_y ) {
          _cue_push(mov, off, CUE_HEAD, cur, 0, 0);
          cur = u3ka_add(2, cur);
          continue;
        }
        //  next bit set, cursor points to a backreference
        //
        else {
          u3_noun bur;
          bur = _cue_rub(u3qa_add(2, cur), a);
          pro = u3h_get(har_p, u3k(u3t(bur)));

          if ( u3_none == pro ) {
            return u3m_bail(c3__exit);
          }

          wid = u3qa_add(2, u3h(bur));

          u3z(bur);
          goto give;
        }
      }
    }
  }

  //  read from .wid, .pro, and top of stack
  //  push on to the stack and goto pass (cell-tail recursion)
  //  or pop the stack and continue
  //
  give: {
    cueframe fam_u = *(_cue_pop(mov, off));

    switch ( fam_u.tag_y ) {
      default: {
        c3_assert(0);
      }

      case CUE_ROOT: {
        break;
      }

      case CUE_HEAD: {
        cur = u3ka_add(2, u3qa_add(wid, fam_u.cur));
        _cue_push(mov, off, CUE_TAIL, fam_u.cur, wid, pro);
        goto pass;
      }

      case CUE_TAIL: {
        pro = u3nc(fam_u.hed, pro);
        u3h_put(har_p, fam_u.cur, u3k(pro));
        wid = u3ka_add(2, u3ka_add(wid, fam_u.wid));
        goto give;
      }
    }
  }

  u3z(cur);
  u3z(wid);
  u3h_free(har_p);

  u3R->cap_p = cap_p;

  return pro;
}

u3_noun
u3we_cue(u3_noun cor)
{
  u3_noun a;

  if ( (u3_none == (a = u3r_at(u3x_sam, cor))) ) {
    return u3m_bail(c3__fail);
  } else {
    return u3qe_cue(a);
  }
}

u3_noun
u3ke_cue(u3_atom a)
{
  u3_noun b = u3qe_cue(a);

  u3z(a);
  return b;
}
