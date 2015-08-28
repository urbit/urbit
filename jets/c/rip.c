/* j/3/rip.c
**
*/
#include "all.h"


/* functions
*/
  u3_noun
  u3qc_rip(u3_atom a,
           u3_atom b)
  {
    if ( !_(u3a_is_cat(a)) || (a >= 32) ) {
      return u3m_bail(c3__fail);
    }
    else {
      u3_noun pir = u3_nul;
      c3_g a_g = a;
      c3_w i_w;

      if ( a_g < 5 ) {
        c3_w met_w = u3r_met(a_g, b);
        c3_w mek_w = ((1 << (1 << a_g)) - 1);

        for ( i_w = 0; i_w < met_w; i_w++ ) {
          c3_w pat_w = (met_w - (i_w + 1));
          c3_w bit_w = (pat_w << a_g);
          c3_w wor_w = (bit_w >> 5);
          c3_w sif_w = (bit_w & 31);
          c3_w src_w = u3r_word(wor_w, b);
          c3_w rip_w = ((src_w >> sif_w) & mek_w);

          pir = u3nc(rip_w, pir);
        }
        return pir;
      }
      else {
        c3_w met_w = u3r_met(a_g, b);
        c3_w len_w = u3r_met(5, b);
        c3_g san_g = (a_g - 5);
        c3_w san_w = 1 << san_g;
        c3_w dif_w = (met_w << san_g) - len_w;
        c3_w tub_w = ((dif_w == 0) ? san_w : (san_w - dif_w));

        for ( i_w = 0; i_w < met_w; i_w++ ) {
          c3_w  pat_w = (met_w - (i_w + 1));
          c3_w  wut_w = (pat_w << san_g);
          c3_w  sap_w = ((0 == i_w) ? tub_w : san_w);
          c3_w* sal_w = u3a_slab(sap_w);

          if ( 0 == sal_w ) {
            return u3m_bail(c3__fail);
          } else {
            c3_w    j_w;
            u3_atom rip;

            for ( j_w = 0; j_w < sap_w; j_w++ ) {
              sal_w[j_w] = u3r_word(wut_w + j_w, b);
            }

            rip = u3a_malt(sal_w);
            pir = u3nc(rip, pir);
          }
          len_w -= san_w;
        }
      }
      return pir;
    }
  }
  u3_noun
  u3wc_rip(u3_noun cor)
  {
    u3_noun a, b;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &a, u3x_sam_3, &b, 0)) ||
         (c3n == u3ud(a)) ||
         (c3n == u3ud(b)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qc_rip(a, b);
    }
  }
