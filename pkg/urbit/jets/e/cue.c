/* j/5/cue.c
**
*/
#include "all.h"

#define CUE_ROOT 0
#define CUE_HEAD 1
#define CUE_TAIL 2

//  stack frame for recording head vs tail iteration
//
//    In Hoon, this structure would be as follows:
//
//    $%  [%root ~]
//        [%head cell-cursor=@]
//        [%tail cell-cursor=@ hed-width=@ hed-value=@]
//    ==
//
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

static inline cueframe
_cue_pop(c3_ys mov, c3_ys off)
{
  cueframe* fam_u = u3to(cueframe, u3R->cap_p + off);
  u3R->cap_p -= mov;

  return *fam_u;
}

u3_noun
u3qe_cue(u3_atom a)
{
  //  initialize signed stack offsets (relative to north/south road)
  //
  c3_ys mov, off;
  {
    c3_y wis_y = c3_wiseof(cueframe);
    c3_o nor_o = u3a_is_north(u3R);
    mov = ( c3y == nor_o ? -wis_y : wis_y );
    off = ( c3y == nor_o ? 0 : -wis_y );
  }

  //  initialize a hash table for dereference backrefs
  //
  u3p(u3h_root) har_p = u3h_new();

  //  stash the current stack post
  //
  u3p(cueframe) cap_p = u3R->cap_p;

  //  push the (only) ROOT stack frame (our termination condition)
  //
  _cue_push(mov, off, CUE_ROOT, 0, 0, 0);

  // initialize cursor to bit-position 0
  //
  u3_atom cur = 0;

  //  the bitwidth and product from reading at cursor
  //
  u3_atom wid, pro;

  //  read from atom at cursor
  //
  //    TRANSFER .cur
  //
  read: {
    //  read tag bit at cur
    //
    c3_y tag_y = u3qc_cut(0, cur, 1, a);

    //  low bit unset, (1 + cur) points to an atom
    //
    //    produce atom and the width we read
    //
    if ( 0 == tag_y ) {
      u3_noun bur;
      {
        u3_noun x = u3qa_inc(cur);
        bur = u3qe_rub(x, a);
        u3z(x);
      }

      pro = u3k(u3t(bur));
      u3h_put(har_p, cur, u3k(pro));
      wid = u3qa_inc(u3h(bur));

      u3z(bur);
      goto take;
    }

    //  read tag bit at (1 + cur)
    //
    {
      u3_noun x = u3qa_inc(cur);
      tag_y = u3qc_cut(0, x, 1, a);
      u3z(x);
    }

    //  next bit set, (2 + cur) points to a backref
    //
    //    produce referenced value and the width we read
    //
    if ( 1 == tag_y ) {
      u3_noun bur;
      {
        u3_noun x = u3ka_add(2, cur);
        bur = u3qe_rub(x, a);
        u3z(x);
      }

      pro = u3h_get(har_p, u3k(u3t(bur)));

      if ( u3_none == pro ) {
        return u3m_bail(c3__exit);
      }

      wid = u3qa_add(2, u3h(bur));

      u3z(bur);
      goto take;
    }

    //  next bit unset, (2 + cur) points to the head of a cell
    //
    //    push a frame to mark HEAD recursion and read the head
    //
    {
      _cue_push(mov, off, CUE_HEAD, cur, 0, 0);

      cur = u3qa_add(2, cur);
      goto read;
    }
  }

  //  consume: popped stack frame, .wid and .pro from above.
  //
  //    TRANSFER .wid, .pro, and contents of .fam_u
  //    (.cur is in scope, but we have already lost our reference to it)
  //
  take: {
    cueframe fam_u = _cue_pop(mov, off);

    switch ( fam_u.tag_y ) {
      default: {
        c3_assert(0);
      }

      //  fam_u is our stack root, we're done.
      //
      case CUE_ROOT: {
        break;
      }

      //  .wid and .pro are the head of the cell at fam_u.cur.
      //  save them (and the cell cursor) in a TAIL frame,
      //  set the cursor to the tail and read there.
      //
      case CUE_HEAD: {
        _cue_push(mov, off, CUE_TAIL, fam_u.cur, wid, pro);

        cur = u3ka_add(2, u3qa_add(wid, fam_u.cur));
        goto read;
      }

      //  .wid and .pro are the tail of the cell at fam_u.cur,
      //  construct the cell, memoize it, and produce it along with
      //  its total width (as if it were a read from above).
      //
      case CUE_TAIL: {
        pro = u3nc(fam_u.hed, pro);
        u3h_put(har_p, fam_u.cur, u3k(pro));
        wid = u3ka_add(2, u3ka_add(wid, fam_u.wid));
        goto take;
      }
    }
  }

  u3z(wid);
  u3h_free(har_p);

  //  sanity check
  //
  c3_assert( u3R->cap_p == cap_p );

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
