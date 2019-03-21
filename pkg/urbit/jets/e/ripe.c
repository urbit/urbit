/* j/5/ripe.c
**
*/
#include "all.h"
#include <openssl/evp.h>

/* functions
*/

  u3_noun
  u3qe_ripe(u3_atom wid, u3_atom dat)
  {
    c3_assert(_(u3a_is_cat(wid)));
    dat = u3qc_rev(3, wid, dat);

    c3_y* dat_y = (c3_y*)u3a_malloc(wid);  // msg body
    u3r_bytes(0, wid, (void*)dat_y, dat);

    const EVP_MD* rip_u = EVP_ripemd160(); // ripem algorithm
    static EVP_MD_CTX* con_u = NULL;       // context

    /* build library context object
       we do this once (and only once)
    */

    if (NULL == con_u) {
      con_u = EVP_MD_CTX_create();
    }

    /* perform signature
    */

    c3_y sib_y[20]; // signature body
    c3_w sil_w;     // signature length
    c3_w ret_w;     // return code

    ret_w = EVP_DigestInit_ex(con_u, rip_u, NULL);
    if ( 1 != ret_w ) {
      u3a_free(dat_y);
      fprintf(stderr, "\rripe jet: crypto library fail 1\n");
      return u3m_bail(c3__exit);
    }

    ret_w = EVP_DigestUpdate(con_u, (void*)dat_y, wid);
    u3a_free(dat_y);
    if (1 != ret_w) {
      fprintf(stderr, "\rripe jet: crypto library fail 2\n");
      return u3m_bail(c3__exit);
    }

    ret_w = EVP_DigestFinal_ex(con_u, sib_y, &sil_w);
    if ( 1 != ret_w ) {
      fprintf(stderr, "\rripe jet: crypto library fail 3\n");
      return u3m_bail(c3__exit);
    }

    /* endian conversion;
       turn into noun for return
    */
    return u3kc_rev(3, sil_w, u3i_bytes(sil_w, sib_y));
  }


  u3_noun
  u3we_ripe(u3_noun cor)
  {
    u3_noun wid, dat;

    if ( (c3n == u3r_mean(cor, u3x_sam_2, &wid,
                               u3x_sam_3, &dat, 0) ||
                 u3ud(wid) || u3ud(dat))
       )
    {
      fprintf(stderr, "\rripe jet: argument error\n");
      return u3m_bail(c3__exit);
    }
    else {
      return u3qe_ripe(wid, dat);
    }
  }
