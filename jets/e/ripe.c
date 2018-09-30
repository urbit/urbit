/* j/5/ripe.c
**
*/
#include "all.h"
#include <openssl/evp.h>

static void byte_reverse_in_place(c3_y *c_y,  /* in */
                                    c3_w  n_w)   /* size */
{
  c3_w i_w;

  for (i_w = 0; i_w < (n_w / 2) ; i_w++) {
    c3_y lo = c_y[i_w];
    c3_y hi = c_y[n_w - i_w - 1];
    c_y[i_w] = hi;
    c_y[n_w - i_w - 1] = lo;
  }

}



u3_noun
u3qe_ripe(u3_atom msg)  
{

  /* decompose 
     input
  */
  
  u3_noun men;  /* msg length */
  u3_noun mod;  /* msg body */

  u3r_mean(msg,
           2,   & men,
           3,   & mod,
           0);

  c3_w met_w = u3r_met(3, men);  /* meta length: length of the length */

  if (met_w > 4){
    fprintf(stderr, "\rripe jet: msg meta-size too big\n");
    return u3m_bail(c3__exit);
  }

  c3_w men_w;   /* msg length in bits */
  men_w =  u3r_word(0, men);

  if (0 != men_w % 8) {
    fprintf(stderr, "\rripe jet: input size was not an even number of bytesn");
    return u3m_bail(c3__exit);
  }
  men_w = men_w / 8;

  
  c3_y * mod_y =  (c3_y * ) u3a_malloc(men_w);  /* msg body */
  u3r_bytes(0, men, (void *) mod_y, mod);

  /* endian 
     convert
     input
  */

  byte_reverse_in_place(mod_y, men_w);

  const EVP_MD *rip_u = EVP_ripemd160();      /* ripem algorithm */
  EVP_MD_CTX *  con_u = NULL;                 /* context */

  /* build library context object
     once (and only once) 
  */
  if (NULL == con_u) {
    con_u = EVP_MD_CTX_create();
  } 

  /* perform 
     signature
  */
  c3_y sib_y[20];             /* signature body */
  c3_w sil_w;                 /* signature length */
  c3_w ret_w;                 /* return code */
  
  ret_w = EVP_DigestInit_ex  (con_u, rip_u, NULL);
  if (ret_w != 1) {
    u3a_free(mod_y );
    fprintf(stderr, "\rripe jet: crypto library fail 1\n");
    return u3m_bail(c3__exit);
  }
    
  ret_w = EVP_DigestUpdate   (con_u, (void *) mod_y, men_w); 

  u3a_free(mod_y );
  
  if (ret_w != 1) {
    fprintf(stderr, "\rripe jet: crypto library fail 2\n");
    return u3m_bail(c3__exit);
  }
  ret_w = EVP_DigestFinal_ex (con_u, sib_y, &sil_w);
  if (ret_w != 1) {
    fprintf(stderr, "\rripe jet: crypto library fail 3\n");
    return u3m_bail(c3__exit);
  }

  /* endian conversion ;
     turn into noun for return
  */

  byte_reverse_in_place(sib_y, 20);
  u3_noun sig = u3i_bytes(sil_w, sib_y);   
  
  return(sig);
}


u3_noun
u3we_ripe(u3_noun cor)
{
  u3_noun msg;      /* hash */

  fprintf(stderr, "HERE\n\r");
  
  if ( (c3n == u3r_mean(cor,
                        u3x_sam_1,   &msg,
                        0)) ||
       (c3n == u3du(msg)) )
    {
      fprintf(stderr, "\rripe jet: argument error\n");
      return u3m_bail(c3__exit);
    } else {
    return u3qe_ripe(msg);
  }
}
