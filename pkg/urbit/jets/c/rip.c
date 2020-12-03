#include "all.h"

u3_noun u3qc_rip(u3_atom bloq, u3_atom b) {
  if ( !_(u3a_is_cat(bloq)) || (bloq >= 32) ) {
    return u3m_bail(c3__fail);
  }

  c3_g bloq_g = bloq;

  /*
    This is a fast-path for the case where all the resulting blocks will
    fit in 31-bit direct atoms.
  */
  if ( bloq_g < 5 ) {                                   //  produce direct atoms
    u3_noun acc     = u3_nul;

    c3_w met_w   = u3r_met(bloq_g, b);                  //  num blocks in atom
    c3_w nbits_w = 1 << bloq_g;                         //  block size in bits
    c3_w bmask_w = (1 << nbits_w) - 1;                  //  result mask

    for ( c3_w i_w = 0; i_w < met_w; i_w++ ) {          //  `i_w` is block index
      c3_w nex_w = i_w + 1;                             //  next block
      c3_w pat_w = met_w - nex_w;                       //  blks left after this
      c3_w bit_w = pat_w << bloq_g;                     //  bits left after this
      c3_w wor_w = bit_w >> 5;                          //  wrds left after this
      c3_w sif_w = bit_w & 31;                          //  bits left in word
      c3_w src_w = u3r_word(wor_w, b);                  //  find word by index
      c3_w rip_w = (src_w >> sif_w) & bmask_w;          //  get item from word

      acc = u3nc(rip_w, acc);
    }

    return acc;
  }

  u3_noun acc   = u3_nul;
  c3_w    met_w = u3r_met(bloq_g, b);
  c3_w    len_w = u3r_met(5, b);
  c3_g    san_g = (bloq_g - 5);
  c3_w    san_w = 1 << san_g;
  c3_w    dif_w = (met_w << san_g) - len_w;
  c3_w    tub_w = ((dif_w == 0) ? san_w : (san_w - dif_w));

  for ( c3_w i_w = 0; i_w < met_w; i_w++ ) {
    c3_w     pat_w = (met_w - (i_w + 1));
    c3_w     wut_w = (pat_w << san_g);
    c3_w     sap_w = ((0 == i_w) ? tub_w : san_w);
    c3_w       j_w;
    u3_atom    rip;
    u3i_slab sab_u;
    u3i_slab_bare(&sab_u, 5, sap_w);

    for ( j_w = 0; j_w < sap_w; j_w++ ) {
      sab_u.buf_w[j_w] = u3r_word(wut_w + j_w, b);
    }

    rip = u3i_slab_mint(&sab_u);
    acc = u3nc(rip, acc);
    len_w -= san_w;
  }

  return acc;
}

u3_noun u3wc_rip(u3_noun cor) {
  u3_noun a, b;

  if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
       (c3n == u3ud(a)) ||
       (c3n == u3ud(b))
     ) {
    return u3m_bail(c3__exit);
  }

  return u3qc_rip(a, b);
}

u3_noun u3kc_rip(u3_atom a, u3_atom b) {
  u3_noun res = u3qc_rip(a, b);
  u3z(a); u3z(b);
  return res;
}
