/* j/5/hmac.c
**
*/
#include "all.h"

/* functions
*/

  u3_noun
  u3qe_hmac(u3_noun haj,
            u3_atom boq,
            u3_atom out,
            u3_atom wik,
            u3_atom key,
            u3_atom wid,
            u3_atom dat)
  {
    c3_assert(_(u3a_is_cat(boq)) && _(u3a_is_cat(wik)) && _(u3a_is_cat(wid)));

    // ensure key and message fit signaled lengths
    key = u3qc_end(3, wik, key);
    dat = u3qc_end(3, wid, dat);

    // keys longer than block size are shortened by hashing
    if (wik > boq) {
      key = u3n_slam_on(u3k(haj), u3nc(wik, key));
      wik = out;
    }

    // keys shorter than block size are right-padded
    if (wik < boq) {
      key = u3kc_lsh(3, (boq - wik), key);
    }

    // pad key, inner and outer
    c3_y trail = (boq % 4);
    c3_y padwords = (boq / 4) + (trail == 0 ? 0 : 1);
    c3_w innpad[padwords], outpad[padwords];
    memset(innpad, 0x36, padwords * 4);
    memset(outpad, 0x5c, padwords * 4);
    if ( trail > 0 ) {
      innpad[padwords-1] = 0x36363636 >> (8 * (4 - trail));
      outpad[padwords-1] = 0x5c5c5c5c >> (8 * (4 - trail));
    }
    u3_atom innkey = u3kc_mix(u3k(key), u3i_words(padwords, innpad));
    u3_atom outkey = u3kc_mix(    key , u3i_words(padwords, outpad));

    // append inner padding to message, then hash
    u3_atom innmsg = u3ka_add(u3kc_lsh(3, wid, innkey), u3k(dat));
    u3_atom innhaj = u3n_slam_on(u3k(haj), u3nc((wid + boq), innmsg));

    // prepend outer padding to result, hash again
    u3_atom outmsg = u3ka_add(u3kc_lsh(3, out, outkey), innhaj);
    u3_atom outhaj = u3n_slam_on(u3k(haj), u3nc((out + boq), outmsg));

    return outhaj;
  }

  u3_noun
  u3we_hmac(u3_noun cor)
  {
    u3_noun haj, boq, out, wik, key, wid, dat;

    // sample is [[haj boq out] [wik key] [wid dat]]
    if ( (c3n == u3r_mean(cor, u3x_sam_4,  &haj,
                               50,         &boq, // +<->-
                               51,         &out, // +<->+
                               u3x_sam_12, &wik,
                               u3x_sam_13, &key,
                               u3x_sam_14, &wid,
                               u3x_sam_15, &dat, 0)) ||
         (c3n == u3ud(boq)) ||
         (c3n == u3a_is_cat(boq)) ||
         (c3n == u3ud(out)) ||
         (c3n == u3a_is_cat(out)) ||
         (c3n == u3ud(wik)) ||
         (c3n == u3a_is_cat(wik)) ||
         (c3n == u3ud(key)) ||
         (c3n == u3ud(wid)) ||
         (c3n == u3a_is_cat(wid)) ||
         (c3n == u3ud(dat)) )
    {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qe_hmac(haj, boq, out, wik, key, wid, dat);
    }
  }
