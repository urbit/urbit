/* vere/frag.c
**
**  This file is in the public domain.
**
**  Fragment large data into multiple fragments for the persistence drivers.
*/

#include "all.h"

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <dirent.h>
#include <stdint.h>
#include <uv.h>
#include <termios.h>
#include <term.h>
#include <errno.h>
#include <libgen.h>
#include <ftw.h>
#include <pthread.h>
#include <time.h>

#include "vere/vere.h"

/*

   Fragment header format:
   <fragment index>:<total fragments>:

   Q: How are <fragment index> and <total fragments> coded?

   A: ASN.1 DER encoding has a nice approach to length-prefixed
   serialization (I know, shocking). If the high bit of the first byte
   is unset, the lower 7 bits encode the length of the value. If it is
   set, they encode the number of bytes that encode the length of the
   value.

   Return: total number of bytes that make up the header.

 */


c3_w u3_frag_head_size(c3_w len_w, /* size of total write */
                       c3_w frg_w, /* this fragment index */
                       c3_w max_w) /* max fragment size */
{
  if (0 == max_w){
    return(0);
  }
  c3_w hed_w = 2; /* two ':' */

  c3_w cnt_w = len_w / max_w + (0 == (len_w % max_w) ? 0 : 1);

  /* we know that it takes 4 bytes, bc that's the size of a c3_w; change this if we ever do 64 bits */

  hed_w +=  (frg_w < (0x1 << 7)) ? 1 : 5;  /* how many bytes to store the index ? */
  hed_w +=  (cnt_w < (0x1 << 7)) ? 1 : 5;  /* how many bytes to store the total ? */

  return(hed_w);
}

c3_w _frag_head_writ_help(c3_y * buf_y, c3_w arg_w)
{
  c3_w cnt_w = 0;
  if (arg_w < (0x1 << 7)){
    buf_y[cnt_w] = (c3_y) arg_w;
    cnt_w ++;
  } else {
    /* we know that it takes 4 bytes, bc that's the size of a c3_w; change this if we ever do 64 bits */
    buf_y[cnt_w] = 4 | (1 << 7); /* 4 bytes masked with 1 in the high bit to indicate multi byte size */
    cnt_w ++;
    * ((c3_w *) & buf_y[cnt_w]) = arg_w;
    cnt_w += 4;
  }
  return(cnt_w);
}

c3_w _frag_head_writ(c3_y * buf_y, c3_w dex_w, c3_w tot_w)
{
  c3_w cnt_w = 0;
  cnt_w += _frag_head_writ_help(buf_y, dex_w);
  buf_y[cnt_w] = ':';
  cnt_w += 1;
  cnt_w += _frag_head_writ_help(& buf_y[cnt_w], tot_w);
  buf_y[cnt_w] = ':';
  cnt_w += 1;
  return (cnt_w);
}

c3_w _frag_head_read_help(c3_y * buf_y, c3_w * arg_w)
{
  c3_w cnt_w = 0;
  if (0 == ((buf_y[0] & (0x1 << 7) ) >> 7)){
    * arg_w = (c3_w) buf_y[0];
    return(1);
  } else {
    c3_w siz_w = buf_y[0] & 0x7f;  /* clear top bit */
    if (siz_w == 4) {
      * arg_w = * ((c3_w *) & buf_y[cnt_w + 1]) ;
      return(5);
    } else {
      fprintf(stderr, "_frag_head_read_help(): size is 0x%x bytes (extracted from 0x%x ; only 4 bytes supported now\n", siz_w, buf_y[0]);
      u3m_bail(c3__fail);
    }
  }

}


c3_w _frag_head_read(c3_y * buf_y, c3_w * dex_w, c3_w * tot_w)
{
  c3_w cnt_w = 0;
  cnt_w += _frag_head_read_help(& buf_y[cnt_w], dex_w);
  if (':' != buf_y[cnt_w] ){
    fprintf(stderr, "_frag_head_read(): header corruption 1 missing ':' \n");
    u3m_bail(c3__fail);
  }
  cnt_w ++;

  cnt_w += _frag_head_read_help(& buf_y[cnt_w], tot_w);
  if (':' != buf_y[cnt_w] ){
    fprintf(stderr, "_frag_head_read(): header corruption 2 missing ':' \n");
    u3m_bail(c3__fail);
  }
  cnt_w ++;
  return (cnt_w);
}

/* read a fragmented record (one or more rows in underlying DB) 

   returns: loob: "all parts of a fragment found?"

*/
c3_o u3_frag_read(_frag_read read_u,
               _frag_done done_u,
               c3_w max_w,
               u3_pers* pers_u,
               c3_y **  dat_y,
               c3_w * len_w,
               mult_read_hand ** mrh_u)
{
  /* set up multi read handle (good for a noun that spans 1 fragment, or many)  */
  * mrh_u = (mult_read_hand *) c3_malloc (sizeof (mult_read_hand));
  (* mrh_u) ->han_u = NULL;
  (* mrh_u) ->dat_y = NULL;

  /* read first fragment */
  c3_y *  dt1_y;
  c3_w    ln1_w;
  void * srh_u; /* read handle for single read */

  c3_o ret_o =  read_u(pers_u,            /* IN: db handle */
                       pers_u ->pos_d,    /* IN: row id */
                       0,                 /* IN: fragment id */
                       & dt1_y,           /* OUT: set pointer to data */
                       & ln1_w,           /* OUT: set len of data */
                       & srh_u);          /* OUT: the single-read handle; need to clean it up later */

  if (c3n == ret_o){
    return(c3n);
  }
  
  /* read header from first fragment */
  c3_w frg_w;
  c3_w cnt_w;
  c3_w hed_w =  _frag_head_read(dt1_y, & frg_w, & cnt_w);


  /* the first fragment is the ONLY fragment?
     Path A:
       - keep the data in the backend DB handle
       - pass that read handle back for later cleanup
  */
  if (cnt_w == 1){
    /* return */
    * dat_y = dt1_y + hed_w;
    * len_w = ln1_w - hed_w;

    (* mrh_u) ->dat_y = NULL;
    (* mrh_u) ->han_u = srh_u;
    
    pers_u -> pos_d ++;
    
    return(ret_o);
  }

  /* multi-fragment?
     Path B:
       - malloc space for the paste-up
       - copy all fragments in
       - clean up the multiple read handles
       - pass the malloced space back for later cleanup
  */

  c3_y *  dta_y = (c3_y *) c3_malloc(cnt_w * max_w);
  c3_y *  ndx_y = dta_y;
  c3_w    lna_w = 0;

  memcpy(ndx_y, dt1_y + hed_w, ln1_w - hed_w);
  lna_w += (ln1_w - hed_w);
  ndx_y += (ln1_w - hed_w);
  done_u( srh_u );  /* clean up read handle from read of frag 0 */
  srh_u = NULL;


  c3_w fri_w; /* read fragment index */
  for (fri_w = 1; fri_w < cnt_w; fri_w ++){

    /* read next fragment */
    ret_o = read_u( pers_u,         /* IN: persistence handle */
                    pers_u ->pos_d, /* IN: row id */
                    fri_w,          /* IN: fragment id */
                    & dt1_y,        /* OUT: set pointer to data */
                    & ln1_w,        /* OUT: set len of data */
                    & srh_u);       /* OUT: the read handle; need to clean it up later */
    if (c3y != ret_o){
      fprintf(stderr, "u3_frag_read(): error reading multi-fragment at fragment %i\n\r", frg_w);
      u3m_bail(c3__fail);
    }

    /* why read the head of each fragment?
       (a) anal retentiveness
       (b) easy way to find size of head, so we can skip past 
          (N.B. size of head of frag 0 != size of head of frag 128 !!)
    */
    hed_w =  _frag_head_read(dt1_y, & frg_w, & cnt_w);
    if (frg_w != fri_w){
      fprintf(stderr, "u3_frag_read(): evt %ld: tried to read fragment %i, got fragment %i\n\r", pers_u->pos_d, fri_w, frg_w);
      u3m_bail(c3__fail);
    }
    
    memcpy(ndx_y, dt1_y + hed_w, ln1_w - hed_w);
    lna_w += (ln1_w - hed_w);
    ndx_y += (ln1_w - hed_w);

    done_u( srh_u ); /* clean up read handle from read of frag n */

  }

  /* return */
  
  * dat_y = dta_y;
  * len_w = lna_w;

  (* mrh_u) ->dat_y = dta_y;
  (* mrh_u) ->han_u = NULL;

  pers_u -> pos_d ++;
  
  return(ret_o);


}

c3_o u3_frag_read_done(mult_read_hand * mrh_u,
                    _frag_done done_u)
{
  if (mrh_u->han_u){
    done_u(mrh_u -> han_u);
  } else {
    free(mrh_u -> dat_y);
  }
  free(mrh_u);

  return(c3y);
}

/* write */


void frag_writ(c3_w max_w,          /* IN: max fragment size (0 == infinite ) */
               u3_writ* wit_u,      /* IN: writ */
               c3_d pos_d,          /* IN: row id */
               c3_y* buf_y,         /* IN: frag data (with space for header) */
               c3_y* byt_y,         /* IN: frag data (first byte of data) */
               c3_w  len_w,         /* IN: frag len */
               _writ_frag wri_u,  /* IN: persistence driver function that actually writes the frag */
               writ_test_cb test_cb
               )
{
  c3_w rem_w = len_w;
  c3_w frg_w = 0;

  c3_w cnt_w = len_w / max_w + (0 == (len_w % max_w) ? 0 : 1); /* how many fragments ? */

  /* setup fragment handle */
  u3_pers_frag * mwh_u = (u3_pers_frag *) c3_malloc(sizeof(u3_pers_frag));
  mwh_u->don_o = (c3_o   *) c3_malloc( sizeof(c3_o) * cnt_w);
  memset(mwh_u->don_o, c3n, cnt_w);
  int ret = pthread_mutex_init(& mwh_u->mut_u, NULL);
  if (0 != ret){
    fprintf(stderr, "_frag_writ(): pthread_mutex_init() failed\n");
    u3m_bail(c3__fail);
  }

  /* write first fragment right out of existing data buffer */
  c3_w frg_len_w = ((max_w == 0 )  ? rem_w :  /* if no limit, write it in 1 fragment */
                    (( rem_w > max_w ) ?
                     max_w :                  /* if len > limit, write only as big as allowed */
                     rem_w));                 /* if len < limit, write it in 1 fragment */

  /* write fragment header into buffer */
  c3_w hed_w = u3_frag_head_size(len_w,
                                 0,
                                 max_w);

  if (byt_y - buf_y != hed_w){
    fprintf(stderr, "frag_writ(): insufficient space before data to insert header: buf_y %p, byt_y %p, delta %li\n", (void *) buf_y, (void *)  byt_y, (buf_y - byt_y) );
    u3m_bail(c3__fail);
  }

  c3_w hd2_w = _frag_head_writ(buf_y, 0, cnt_w);
  if (hed_w != hd2_w){
    fprintf(stderr, "frag_writ(): header wrong size\n");
    u3m_bail(c3__fail);
  }

  /* write fragment into db */
  wri_u(wit_u,             /* IN: writ */
        pos_d,             /* IN: row id */
        0,                 /* IN: frag id */
        cnt_w,             /* IN: num frags */
        buf_y,             /* IN: frag data (with header)  */
        frg_len_w + hed_w, /* IN: frag len */
        mwh_u,             /* IN: multi-write handle */
        test_cb            /* IN: callback structure */
        );
  rem_w = rem_w - frg_len_w;
  if (rem_w == 0) {
    return;
  }

  /* remaining fragments (if any) need to be memcpy()-ed so that we can write mini headers*/

  for(frg_w = 1; frg_w < cnt_w; frg_w++){

    byt_y += frg_len_w;

    frg_len_w = ( rem_w > max_w ) ? max_w : rem_w;

    /* malloc space for fragment + header 
       Q: Why recalc hed_w?  
       A: because it differs between various fragments! */
    hed_w = u3_frag_head_size(len_w,
                              frg_w,
                              max_w);
    c3_y * frag_y = c3_malloc(frg_len_w + hed_w);
    hd2_w = _frag_head_writ(frag_y, frg_w, cnt_w);

    memcpy(frag_y + hed_w, byt_y, frg_len_w);

    wri_u(wit_u,             /* IN: writ */
          pos_d,             /* IN: row id */
          frg_w,             /* IN: fragment index */
          cnt_w,             /* IN: num frags */
          frag_y,            /* IN: frag data (with header)  */
          frg_len_w + hed_w, /* IN: frag len */
          mwh_u,
          test_cb);          /* IN: callback structure */

    rem_w = rem_w - frg_len_w;
  }
}

void u3_frag_write_check(u3_pers_writ_calb  * cbd_u)
{
  /* sanity check */

  c3_w                  frg_w = cbd_u -> frg_w;
  c3_w                  cnt_w = cbd_u -> cnt_w;
  if (frg_w > cnt_w){
    fprintf(stderr, "fond_write_cb: evt %ld, frag %u > count %u\n\r", cbd_u->wit_u->evt_d, frg_w, cnt_w);
    u3m_bail(c3__fail); 
    return;
  }
}


/* We've jut finished writing one fragment.
   We need to detect if the total write is finished.  If so:
        (1) mark the write as 100% complete
        (2) clean up resources specific to the multi-write handle

   Return code:  "all complete?"
 */

c3_o u3_frag_write_done(c3_w frg_w,
                        c3_w cnt_w,
                        u3_writ * wit_u,
                        u3_pers_frag * mwh_u)
{

  c3_o all_o = c3y;

  /* mutex protect */
  {
    int ret = pthread_mutex_lock(& mwh_u->mut_u);
    if (0 != ret){
      fprintf(stderr, "u3_frag_writ_done(): pthread_mutex_lock() failed\n");
      u3m_bail(c3__fail);
    }

    mwh_u->don_o[frg_w] = c3y;  /* ACTION 1: mark this fragment is written in the multi-write handle (NOT in the writ!) */
    c3_w itr_w;

    /* see if all fragments are done */
    for (itr_w = 0; itr_w < cnt_w; itr_w ++){
      if (mwh_u->don_o[itr_w] == c3n){
        all_o = c3n;
        break;
      }
    }

    ret = pthread_mutex_unlock(& mwh_u->mut_u);
    if (0 != ret){
      fprintf(stderr, "u3_frag_writ_done(): pthread_mutex_unlock() failed\n");
      u3m_bail(c3__fail);
    }
  }
  if (c3n == all_o){
    return(c3n);
  }

  /* 1: mark write as 100% complete */
  wit_u->ped_o = c3y; 


  /* 2: clean up the shared multi-write handle */  
  int ret = pthread_mutex_destroy(& mwh_u->mut_u);
  if (0 != ret){
    fprintf(stderr, "u3_frag_writ_done(): pthread_mutex_destroy() failed\n");
    u3m_bail(c3__fail);
  }

  free(mwh_u->don_o);     /* loob vector */
  free(mwh_u);

  return(c3y);
}
