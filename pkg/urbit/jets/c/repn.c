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

//  repn :: Word -> [Word] -> Atom
//  repn bits blox =
//      (bits > 64) & \case
//          True  → error "repn only works with block sizes <= 64"
//          False → view (from atomWords)
//                $ VP.fromList
//                $ finish
//                $ foldl' f ([], 0, 0)
//                $ zip (repeat bits) blox
//    where
//      finish (acc, wor, n) = reverse
//                           $ dropWhile (==0)
//                           $ case n of { 0 -> acc; _ -> wor:acc }
//
//      slice size off wor = shiftL (takeBits size wor)
//                         $ fromIntegral off
//
//      f (acc, wor, off) (remBlok, blok) =
//          let rem = 64 - off in
//          compare remBlok rem & \case
//              LT -> (acc, res, off+bits)
//                where res = wor .|. slice bits off blok
//              EQ -> (res:acc, 0, 0)
//                where res = (wor .|. slice bits off blok)
//              GT -> f (res:acc, 0, 0) (remBlok', blok')
//                where res      = wor .|. slice rem off blok
//                      remBlok' = remBlok-rem
//                      blok'    = shiftR blok (fromIntegral bits)

u3_noun u3qc_repn(u3_atom bits, u3_noun blox) {
    if ( !_(u3a_is_cat(bits) || bits==0 || bits>31) ) {
        return u3m_bail(c3__fail);
    }

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

        u3l_log("strip\n");

        blox = u3t(blox);
    }

    //
    //  Calculate input and output size.
    //
    c3_w num_blox = u3qb_lent(blox);
    c3_w bit_widt = num_blox * bits;
    c3_w wor_widt = DIVCEIL(bit_widt, 32);

    u3l_log("%d*%d=%d [%d]\n", bits, num_blox, bit_widt, wor_widt);

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

    c3_w acc = 0;      // Word to write into
    c3_w acc_idx = 0;  // Where the word will be written to
    c3_w acc_used = 0; // Number of bits used in acc.

    void flush() {
        u3l_log("%d @ %d\n", acc, acc_idx);
        buf[acc_idx++] = acc;
        acc = 0;
        acc_used = 0;
    }

    c3_w slice(c3_w sz, c3_w off, c3_w val) {
       u3l_log("slice(%d,%d,%d) = %d\n", sz, off, val, TAKEBITS(sz, val) << off);
       return TAKEBITS(sz, val) << off;
    }

    for (c3_w i=0; i<num_blox; i++) {
        c3_w block = u3a_h(blox);
        blox = u3t(blox);

        u3l_log("#%d = %d\n", i, block);

        for (c3_w rem_in_block=bits; rem_in_block;) {
            c3_w rem_in_acc = 32 - acc_used;
            if (rem_in_block == rem_in_acc) {                       //  EQ
                acc |= slice(rem_in_block, acc_used, block);
                u3l_log("acc:%d\n", acc);
                flush();
                rem_in_block = 0;
            } else if (rem_in_block < rem_in_acc) {                 //  LT
                acc |= slice(rem_in_block, acc_used, block);
                u3l_log("acc:%d\n", acc);
                acc_used += rem_in_block;
                rem_in_block = 0;
            } else {                                                //  GT
                acc |= slice(rem_in_acc, acc_used, block);
                u3l_log("acc:%d\n", acc);
                rem_in_block -= rem_in_acc;
                block = block >> rem_in_acc;
                flush();
            }
        }

        u3l_log("acc:%d acc_idx:%d acc_used:%d\n", acc, acc_idx, acc_used);
    }

    //
    //  If the last word isn't fully used, it will still need to be
    //  flushed.
    //
    if (acc_used) flush();

    return u3a_mint(buf, wor_widt);
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
