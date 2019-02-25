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
#define DIVCEIL(x,y)  1 + ((x - 1) / y);

/*
  `ripn` breaks `atom` into a list of blocks, of bit-width `bits`. The
  resulting list will be least-significant block first.

  XX TODO This only handles cases where the bit-width is <= 32.

  For each block we produce, we need to grab the relevant words inside
  `atom`, so we first compute their indicies.

  `ins_idx` is the word-index of the least-significant word we
  care about, and `sig_idx` is the word after that.

  Next we grab those words (`ins_word` and `sig_word`) from the atom
  using `u3r_word`. Note that `sig_idx` might be out-of-bounds for the
  underlying array of `atom`, but `u3r_word` returns 0 in that case,
  which is exatly what we want.

  Now, we need to grab the relevant bits out of both words, and combine
  them. `bits_rem_in_ins_word` is the number of remaining (insignificant)
  bits in `ins_word`, `nbits_ins` is the number of bits we want from the
  less-significant word, and `nbits_sig` from the more-significant one.

  Take the least significant `nbits_sig` bits from `sig_word`, and take
  the slice we care about from `ins_word`. In order to take that slice,
  we drop `bits_rem_in_ins_word` insignificant bits, and then take the
  `nbits_sig` most-significant bits.

  Last, we slice out those bits from the two words, combine them into
  one word, and cons them onto the front of the result.
*/
u3_noun u3qc_ripn(u3_atom bits, u3_atom atom) {
  if ( !_(u3a_is_cat(bits) || bits==0 || bits>31) ) {
    return u3m_bail(c3__fail);
  }

  c3_w bit_width  = u3r_met(0, atom);
  c3_w num_blocks = DIVCEIL(bit_width, bits);

  u3_noun res = u3_nul;

  for ( c3_w blk = 0; blk < num_blocks; blk++ ) {
    c3_w next_blk = blk + 1;
    c3_w blks_rem = num_blocks - next_blk;
    c3_w bits_rem = blks_rem * bits;
    c3_w ins_idx  = bits_rem / 32;
    c3_w sig_idx  = ins_idx + 1;

    c3_w bits_rem_in_ins_word = bits_rem % 32;

    c3_w ins_word  = u3r_word(ins_idx, atom);
    c3_w sig_word  = u3r_word(sig_idx, atom);
    c3_w nbits_ins = c3_min(bits, 32 - bits_rem_in_ins_word);
    c3_w nbits_sig = bits - nbits_ins;

    c3_w ins_word_bits = TAKEBITS(nbits_ins, ins_word >> bits_rem_in_ins_word);
    c3_w sig_word_bits = TAKEBITS(nbits_sig, sig_word);

    c3_w item = ins_word_bits | (sig_word_bits << nbits_ins);

    res = u3nc(item, res);
  }

  return res;
}

u3_noun u3wc_ripn(u3_noun cor) {
  u3_noun bits, atom;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &bits, u3x_sam_3, &atom, 0)) ||
       (c3n == u3ud(bits)) ||
       (c3n == u3ud(atom)) )
  {
    return u3m_bail(c3__exit);
  }

  return u3qc_ripn(bits, atom);
}

u3_noun u3kc_ripn(u3_atom bits, u3_atom atom) {
  u3_noun res = u3qc_ripn(bits, atom);
  u3z(bits), u3z(atom);
  return res;
}
