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

/*
  TODO Don't do the double flop.
*/
u3_noun u3qc_repn(u3_atom bits, u3_noun blox) {
    if ( !_(u3a_is_cat(bits) || bits==0 || bits>31) ) {
        return u3m_bail(c3__fail);
    }

    blox = u3kb_flop(blox);

    //
    //  We need to enforce the invariant that the result does not contain
    //  leading (? TODO). Since each input block must be smaller than
    //  a word, simply stripping off leading zero blocks is enough to
    //  ensure this.
    //
    //  We also verify that each input block is a direct atom.
    //
    while (1) {
        if (blox == 0) {
            return 0;
        }

        u3_noun blok_n = u3h(blox);

        if (!_(u3a_is_cat(blok_n)))
            return u3m_bail(c3__fail);

        if (blok_n != 0) break;

        blox = u3t(blox);
    }

    blox = u3kb_flop(blox);

    //
    //  Calculate input and output size.
    //
    c3_w num_blox = u3qb_lent(blox);
    c3_w bit_widt = num_blox * bits;
    c3_w wor_widt = DIVCEIL(bit_widt, 32);

    //
    //  Allocate a proto-atom. This is u3a_slab without initialization.
    //
    c3_w* buf;
    {
        c3_w*     nov_w = u3a_walloc(wor_widt + c3_wiseof(u3a_atom));
        u3a_atom* pug_u = (void *)nov_w;

        pug_u->mug_w = 0;
        pug_u->len_w = wor_widt;
        buf          = pug_u->buf_w;
    }

    //
    //  Fill the atom buffer with bits from each block.
    //
    //  Bits are pushed into the `acc` register and flushed to the buffer
    //  once full.
    //
    //  acc  register
    //  idx  where the register will be flushed to
    //  use  number of register bits filled (used)
    //

    c3_w acc=0, idx=0, use=0;

    void flush() {
        buf[idx++] = acc;
        acc = 0;
        use = 0;
    }

    c3_w slice(c3_w sz, c3_w off, c3_w val) {
       return TAKEBITS(sz, val) << off;
    }

    for (c3_w i=0; i<num_blox; i++) {
        c3_w blok = u3a_h(blox);
        blox = u3t(blox);

        for (c3_w rem_in_blok=bits; rem_in_blok;) {
            c3_w rem_in_acc = 32 - use;
            if (rem_in_blok == rem_in_acc) {            //  EQ
                acc |= slice(rem_in_blok, use, blok);
                flush();
                rem_in_blok = 0;
            } else if (rem_in_blok < rem_in_acc) {      //  LT
                acc |= slice(rem_in_blok, use, blok);
                use += rem_in_blok;
                rem_in_blok = 0;
            } else {                                    //  GT
                acc |= slice(rem_in_acc, use, blok);
                rem_in_blok -= rem_in_acc;
                blok = blok >> rem_in_acc;
                flush();
            }
        }

    }

    //
    //  If the last word isn't fully used, it will still need to be
    //  flushed.
    //
    if (use) flush();

    return u3a_malt(buf);
}

u3_noun u3wc_repn(u3_noun cor) {
  u3_noun bits, blox=0;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &bits, u3x_sam_3, &blox, 0)) ||
       (c3n == u3ud(bits)) )
  {
    return u3m_bail(c3__exit);
  }

  return u3qc_repn(bits, blox);
}

u3_noun u3kc_repn(u3_atom bits, u3_atom blox) {
  u3_noun res = u3qc_repn(bits, blox);
  u3z(bits), u3z(blox);
  return res;
}
