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

u3_noun u3qc_repn(u3_atom bits, u3_noun blox) {
    if ( !_(u3a_is_cat(bits) || bits==0 || bits>31) ) {
        return u3m_bail(c3__fail);
    }

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
    //  use  number of register bits filled (used)
    //  cur  next buffer word to flush into.
    //

    c3_w acc=0, use=0, *cur=buf;

#   define FLUSH() *cur++=acc; acc=use=0
#   define SLICE(sz,off,val) TAKEBITS(sz, val) << off

    for (c3_w i=0; i<num_blox; i++) {
        u3_noun blok_n = u3h(blox);
        blox = u3t(blox);

        if (!_(u3a_is_cat(blok_n)))
            return u3m_bail(c3__fail);

        c3_w blok = blok_n;

        for (c3_w rem_in_blok=bits; rem_in_blok;) {
            c3_w rem_in_acc = 32 - use;
            if (rem_in_blok == rem_in_acc) {            //  EQ
                acc |= SLICE(rem_in_blok, use, blok);
                FLUSH();
                rem_in_blok = 0;
            } else if (rem_in_blok < rem_in_acc) {      //  LT
                acc |= SLICE(rem_in_blok, use, blok);
                use += rem_in_blok;
                rem_in_blok = 0;
            } else {                                    //  GT
                acc |= SLICE(rem_in_acc, use, blok);
                rem_in_blok -= rem_in_acc;
                blok = blok >> rem_in_acc;
                FLUSH();
            }
        }
    }

    //
    //  If the last word isn't fully used, it will still need to be
    //  flushed.
    //
    if (use) FLUSH();

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
