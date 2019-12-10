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

u3_noun u3qc_repn(u3_atom bits, u3_atom atom) {
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

u3_noun u3wc_repn(u3_noun cor) {
  u3_noun bits, atom=0;

  u3l_log("WE'RE ALL GOING TO DIE\n");

  return 0;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &bits, u3x_sam_3, &atom, 0)) ||
       (c3n == u3ud(bits)) ||
       (c3n == u3ud(atom)) )
  {
    return u3m_bail(c3__exit);
  }

  return u3qc_repn(bits, atom);
}

u3_noun u3kc_repn(u3_atom bits, u3_atom atom) {
  u3_noun res = u3qc_repn(bits, atom);
  u3z(bits), u3z(atom);
  return res;
}
