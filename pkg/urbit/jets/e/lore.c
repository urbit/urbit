/* j/5/lore.c
**
*/
#include "all.h"

  static u3_noun
  _lore_cut(c3_w pos_w, c3_w len_w, u3_noun src)
  {
    if ( 0 == len_w ) {
      return 0;
    }
    else {
      // as an optimization, we alloc small lines on the stack
      // (only small to prevent stack overflow)
      c3_t  big_t = len_w > 255;
      c3_y* buf_y = big_t ? u3a_malloc(len_w) : alloca(len_w);
      u3_noun red;

      u3r_bytes(pos_w, len_w, buf_y, src);
      red = u3i_bytes(len_w, buf_y);
      if ( big_t ) {
        u3a_free(buf_y);
      }
      return red;
    }
  }

  // Leaving the lore jet in place for backwards compatibility.
  // TODO: remove u3[qw]e_lore (also from jet tree)

  u3_noun
  u3qe_lore(u3_atom lub)
  {
    c3_w    len_w = u3r_met(3, lub);
    c3_w    pos_w = 0;
    u3_noun tez = u3_nul;

    while ( 1 ) {
      c3_w meg_w = 0;
      c3_y end_y;

      c3_y byt_y;
      while ( 1 ) {
        if ( pos_w >= len_w ) {
          byt_y = 0;
          end_y = c3y;
          break;
        }
        byt_y = u3r_byte(pos_w + meg_w, lub);

        if ( (10 == byt_y) || (0 == byt_y) ) {
          end_y = __(byt_y == 0);
          break;
        } else meg_w++;
      }

      if ((byt_y == 0) && ((pos_w + meg_w + 1) < len_w)) {
        return u3m_bail(c3__exit);
      }

      if ( !_(end_y) && pos_w >= len_w ) {
        return u3kb_flop(tez);
      }
      else {
        tez = u3nc(_lore_cut(pos_w, meg_w, lub), tez);
        if ( _(end_y) ) {
          return u3kb_flop(tez);
        }
        pos_w += (meg_w + 1);
      }
    }
  }

  u3_noun
  u3we_lore(u3_noun cor)
  {
    u3_noun lub;

    if ( (u3_none == (lub = u3r_at(u3x_sam, cor))) ||
         (c3n == u3ud(lub)) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qe_lore(lub);
    }
  }

  u3_noun
  u3qe_leer(u3_atom txt)
  {
    c3_w pos_w, i_w = 0, len_w = u3r_met(3, txt);
    u3_noun out = u3_nul;

    while ( 1 ) {
      // scan till end or newline
      for ( pos_w = i_w; i_w < len_w; ++i_w ) {
        if ( 10 == u3r_byte(i_w, txt) ) {
          break;
        }
      }
      // append to list
      out = u3nc(_lore_cut(pos_w, i_w - pos_w, txt), out);
      // finish or advance
      if ( i_w++ == len_w ) {
        break;
      }
    }

    return u3kb_flop(out);
  }

  u3_noun
  u3we_leer(u3_noun cor)
  {
    u3_noun txt;

    if ( (u3_none == (txt = u3r_at(u3x_sam, cor))) ||
         (c3n == u3ud(txt)) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qe_leer(txt);
    }
  }
