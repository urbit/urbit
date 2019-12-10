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

#define SLICE(x,y,z) u3m_bail(c3__fail)
#define HEAD(x) u3m_bail(c3__fail)
#define TAIL(x) u3m_bail(c3__fail)

u3_noun u3qc_repn(u3_atom bits, u3_cell blox) {
    if ( !_(u3a_is_cat(bits) || bits==0 || bits>31) ) {
        return u3m_bail(c3__fail);
    }

    c3_w num_blox = u3qb_lent(blox);
    c3_w bit_widt = num_blox * bits;
    c3_w wor_widt = DIVCEIL(bit_widt, 32);

    c3_w *buf = malloc(4*wor_widt); // TODO Allocate an atom on the loom.

    c3_w acc = 0;      // Word to write into
    c3_w acc_idx = 0;  // Where the word will be written to
    c3_w acc_used = 0; // Number of bits used in acc.

// #define FLUSH { buf[acc_idx++] = acc; acc = acc_used = 0; }
// #define DIVCEIL(x,y) \

    for (c3_w i=0; i<num_blox; i++) {
        //  TODO Make sure `blox` is cell.
        //  TODO Make sure head of `blox` is cell.

        c3_w block = HEAD(blox);
        blox = TAIL(blox);

        for (c3_w rem_in_block=bits; rem_in_block;) {
            c3_w rem_in_acc   = 32 - acc_used;
            if (rem_in_block == rem_in_acc) {                       //  EQ
                acc |= SLICE(rem_in_block, acc_used, block);
                buf[acc_idx++] = acc; acc = acc_used = 0;
                rem_in_block = 0;
            } else if (rem_in_block < rem_in_acc) {                 //  LT
                acc |= SLICE(rem_in_block, acc_used, block);
                acc_used += rem_in_block;
                rem_in_block = 0;
            } else {                                                //  GT
                acc |= SLICE(rem_in_acc, acc_used, block);
                rem_in_block -= rem_in_acc;
                block = block >> rem_in_acc;
                buf[acc_idx++] = acc; acc = acc_used = 0;
            }
        }
    }

    //  TODO Remove leading zeros and reallocate.
    if (buf[0]==0) {
        return u3m_bail(c3__fail);
    }

  return 0;
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
