/* j/5/base.c
**
*/
#include "all.h"

const c3_y hex_y[16] = { '0', '1', '2', '3', '4', '5', '6', '7',
                         '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'  };

u3_noun
u3qe_en_base16(u3_atom len, u3_atom dat)
{
  if ( c3n == u3a_is_cat(len) ) {
    return u3m_bail(c3__fail);
  }
  else {
    c3_w     len_w = (c3_w)len;
    u3i_slab sab_u;

    u3i_slab_bare(&sab_u, 4, len_w);
    sab_u.buf_w[sab_u.len_w - 1] = 0;

    {
      c3_y* buf_y = sab_u.buf_y;
      c3_y  inp_y;

      while ( len_w-- ) {
        inp_y = u3r_byte(len_w, dat);

        *buf_y++ = hex_y[inp_y >> 4];
        *buf_y++ = hex_y[inp_y & 0xf];
      }
    }

    return u3i_slab_moot_bytes(&sab_u);
  }
}

static inline c3_o
_of_hex_digit(c3_y inp_y, c3_y* out_y)
{
  if ( inp_y >= '0' && inp_y <= '9' ) {
    *out_y = inp_y - '0';
    return c3y;
  }
  else if ( inp_y >= 'a' && inp_y <= 'f' ) {
    *out_y = inp_y - 87;
    return c3y;
  }
  else if ( inp_y >= 'A' && inp_y <= 'F' ) {
    *out_y = inp_y - 55;
    return c3y;
  }

  return c3n;
}

static inline c3_o
_of_hex_odd(u3_atom inp, c3_w len_w, c3_w byt_w, c3_y* buf_y)
{
  c3_y low_y, hig_y, lit_y, hit_y;

  hig_y = u3r_byte(--byt_w, inp);

  while ( --len_w ) {
    low_y = u3r_byte(--byt_w, inp);

    if (  (c3n == _of_hex_digit(low_y, &lit_y))
       || (c3n == _of_hex_digit(hig_y, &hit_y)) )
    {
      return c3n;
    }
    else {
      *buf_y++ = (hit_y & 0xf) ^ (lit_y << 4);
    }

    hig_y = u3r_byte(--byt_w, inp);
  }

  if ( c3n == _of_hex_digit(hig_y, &hit_y) ) {
    return c3n;
  }
  else {
    *buf_y = hit_y & 0xf;
  }

  return c3y;
}

static inline c3_o
_of_hex_even(u3_atom inp, c3_w len_w, c3_y* buf_y)
{
  c3_y lit_y, hit_y;
  c3_s inp_s;

  while ( len_w-- ) {
    inp_s = u3r_short(len_w, inp);

    if (  (c3n == _of_hex_digit(inp_s & 0xff, &lit_y))
       || (c3n == _of_hex_digit(inp_s >> 8,   &hit_y)) )
    {
      return c3n;
    }
    else {
      *buf_y++ = (hit_y & 0xf) ^ (lit_y << 4);
    }
  }

  return c3y;
}

u3_noun
u3qe_de_base16(u3_atom inp)
{
  c3_w     len_w = u3r_met(4, inp);
  u3i_slab sab_u;

  u3i_slab_bare(&sab_u, 3, len_w);
  sab_u.buf_w[sab_u.len_w - 1] = 0;

  //  even byte-length input can be parsed in aligned, 16-bit increments,
  //  but odd byte-length input cannot, and is expressed more simply in bytes.
  //
  {
    c3_w byt_w = u3r_met(3, inp);
    c3_o ret_o = ( byt_w & 1 )
               ? _of_hex_odd(inp, len_w, byt_w, sab_u.buf_y)
               : _of_hex_even(inp, len_w, sab_u.buf_y);

    if ( c3n == ret_o ) {
      u3i_slab_free(&sab_u);
      return u3_nul;
    }
    else {
      u3_noun dat = u3i_slab_mint_bytes(&sab_u);
      return u3nt(u3_nul, u3i_word(len_w), dat);
    }
  }
}

u3_noun
u3we_en_base16(u3_noun cor)
{
  u3_noun a, b;
  u3x_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0);
  return u3qe_en_base16(u3x_atom(a), u3x_atom(b));
}

u3_noun
u3we_de_base16(u3_noun cor)
{
  u3_noun sam = u3x_at(u3x_sam, cor);
  return u3qe_de_base16(u3x_atom(sam));
}
