/* j/3/rep.c
**
*/
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

static u3_noun
_bit_rep(u3_atom bits, u3_noun blox)
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

static u3_noun
_block_rep(u3_atom a,
           u3_noun b)
{
  if ( !_(u3a_is_cat(a)) || (a >= 32) ) {
    return u3m_bail(c3__fail);
  }
  else {
    c3_g       a_g = a;
    c3_w     tot_w = 0;
    u3i_slab sab_u;

    /* Measure and validate the slab required.
    */
    {
      u3_noun cab = b;

      while ( 1 ) {
        u3_noun h_cab;
        c3_w    len_w;

        if ( 0 == cab ) {
          break;
        }
        else if ( c3n == u3du(cab) ) {
          return u3m_bail(c3__exit);
        }
        else if ( c3n == u3ud(h_cab = u3h(cab)) ) {
          return u3m_bail(c3__exit);
        }
        else if ( (tot_w + (len_w = u3r_met(a_g, h_cab))) < tot_w ) {
          return u3m_bail(c3__fail);
        }
        tot_w++;
        cab = u3t(cab);
      }

      if ( 0 == tot_w ) {
        return 0;
      }

      u3i_slab_init(&sab_u, a_g, tot_w);
    }

    /* Chop the list atoms in.
    */
    {
      u3_noun cab = b;
      c3_w  pos_w = 0;

      while ( 0 != cab ) {
        u3_noun h_cab = u3h(cab);

        u3r_chop(a_g, 0, 1, pos_w, sab_u.buf_w, h_cab);
        pos_w++;
        cab = u3t(cab);
      }
    }

    return u3i_slab_mint(&sab_u);
  }
}

u3_noun
u3qc_rep(u3_atom a,
         u3_atom b,
         u3_noun c)
{
  if ( 1 == b ) {
    return _block_rep(a, c);
  }

  if ( 0 == a ) {
    return _bit_rep(b, c);
  }

  u3l_log("rep: stub");
  return u3_none;
}

u3_noun
u3wc_rep(u3_noun cor)
{
  u3_atom bloq, step;
  u3_noun a, b;
  u3x_mean(cor, u3x_sam_2, &a,
                u3x_sam_3, &b, 0);
  u3x_bite(a, &bloq, &step);

  return u3qc_rep(bloq, step, b);
}

u3_noun
u3kc_rep(u3_atom a,
         u3_atom b,
         u3_noun c)
{
  u3_noun res = u3qc_rep(a, b, c);
  u3z(a); u3z(b); u3z(c);
  return res;
}
