#include "all.h"

/*
  Get the lowest `n` bits of a word `w` using a bitmask.
*/
#define TAKEBITS(n,w) \
  ((n)==32) ? (w) :   \
  ((n)==0)  ? 0   :   \
  ((w) & ((1 << (n)) - 1))

/*
  Divide, rounding up.
*/
#define DIVCEIL(x,y) \
  (x==0) ? 0 :       \
  1 + ((x - 1) / y);

u3_noun
u3qc_repn(u3_atom bits, u3_noun blox)
{
  if ( (c3n == u3a_is_cat(bits) || bits==0 || bits>31) ) {
    return u3m_bail(c3__fail);
  }

  //
  //  Calculate input and output size.
  //
  c3_w num_blox_w = u3qb_lent(blox);
  c3_w bit_widt_w = num_blox_w * bits;
  c3_w wor_widt_w = DIVCEIL(bit_widt_w, 32);
  u3i_slab  sab_u;
  u3i_slab_bare(&sab_u, 5, wor_widt_w);

  //
  //  Fill the atom buffer with bits from each block.
  //
  //  Bits are pushed into the `acc_w` register and flushed to the buffer
  //  once full.
  //
  //  acc_w  register
  //  use_w  number of register bits filled (used)
  //  cur_w  next buffer word to flush into.
  //
  {
    c3_w acc_w=0, use_w=0, *cur_w=sab_u.buf_w;

#   define FLUSH() *cur_w++=acc_w; acc_w=use_w=0
#   define SLICE(sz,off,val) TAKEBITS(sz, val) << off

    for (c3_w i=0; i<num_blox_w; i++) {
      u3_noun blok_n = u3h(blox);
      blox = u3t(blox);

      if ( c3n == u3a_is_cat(blok_n) ) {
        return u3m_bail(c3__fail);
      }

      c3_w blok_w = blok_n;

      for (c3_w rem_in_blok_w=bits; rem_in_blok_w;) {
        c3_w rem_in_acc_w = 32 - use_w;
        if (rem_in_blok_w == rem_in_acc_w) {              //  EQ
          acc_w |= SLICE(rem_in_blok_w, use_w, blok_w);
          FLUSH();
          rem_in_blok_w = 0;
        }
        else if (rem_in_blok_w < rem_in_acc_w) {          //  LT
          acc_w |= SLICE(rem_in_blok_w, use_w, blok_w);
          use_w += rem_in_blok_w;
          rem_in_blok_w = 0;
        }
        else {                                            //  GT
          acc_w |= SLICE(rem_in_acc_w, use_w, blok_w);
          rem_in_blok_w -= rem_in_acc_w;
          blok_w = blok_w >> rem_in_acc_w;
          FLUSH();
        }
      }
    }

    //
    //  If the last word isn't fully used, it will still need to be
    //  flushed.
    //
    if (use_w) {
      FLUSH();
    }
  }

  return u3i_slab_mint(&sab_u);
}

u3_noun
u3wc_repn(u3_noun cor)
{
  u3_noun bits, blox;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &bits, u3x_sam_3, &blox, 0)) ||
       (c3n == u3ud(bits)) )
  {
    return u3m_bail(c3__exit);
  }

  return u3qc_repn(bits, blox);
}

u3_noun
u3kc_repn(u3_atom bits, u3_atom blox)
{
  u3_noun res = u3qc_repn(bits, blox);
  u3z(bits); u3z(blox);
  return res;
}
