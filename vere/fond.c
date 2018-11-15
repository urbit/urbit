/* vere/found.c
**
**  This file is in the public domain.
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

#include "vere/vere.h"

#include <fdb_c.h>

#define CLUS_NAME  "fond.db"
#define DATB_NAME  "DB" /* mandatory */
#define MAX_SIZE 20 // 65536
#define MAX_RETRY 5

/* util */

/*  Urbit stores event IDs as c3_d.
 *  FoundationDB wants them as strings.
 *  Convert urbit -> fond
 */
void _fond_sprintf_eventd(c3_d evt_d,     /* IN: event ID */
                          c3_w frg_w,     /* IN: event fragment */
                          c3_y * key_y,   /* OUT: pre-allocated array of c3_y [ ideally of size 256 ] to receive key str */
                          c3_ws * len_ws    /* OUT: pre-allocated c3_d to receive key string size */
                          )
{
  sprintf((char *) key_y, "%ld:%i", evt_d, frg_w);
  * len_ws = strlen((char *) key_y);
}

/* read */



void _fond_init_run_network()
{
  fdb_error_t err_u = fdb_run_network();
  if (0 != err_u){
    fprintf(stderr, "fond_init_run run_network: %s\n", fdb_get_error( err_u ));
    exit(-1); /* XXX */
  }

}

c3_o
_fond_init(u3_fond * fond_u, c3_c * pin_c)
{
  
  /* build file path */
  char * path_c = NULL;
  {
    c3_c * tmp_c = (c3_c *) strdup(pin_c);
    c3_c * sav_c = NULL;
    strtok_r(tmp_c, ":", & sav_c);
    fprintf(stderr, "sav_c = %s\n\r", sav_c);
    free(tmp_c);

    path_c = malloc(strlen(u3C.dir_c) + strlen(CLUS_NAME) + 2);
    sprintf(path_c, "./%s/%s", u3C.dir_c, CLUS_NAME);

    path_c = "/etc/foundationdb/fdb.cluster"; /* XXXXXXXXXXXXXX */
  
    fprintf(stderr, "\rdb path = %s\n\r", path_c);
  }



  /* setup network */
  {
    fdb_error_t err_u;

    err_u = fdb_select_api_version(FDB_API_VERSION);
    if (0 != err_u){
      fprintf(stderr, "fond_read_init set_api_version: %s\n", fdb_get_error( err_u ));
      goto read_init_error;
    }

    
    err_u = fdb_setup_network();
    if (0 != err_u){
      fprintf(stderr, "fond_read_init setup_network: %s\n", fdb_get_error( err_u ));
      goto read_init_error;
    }

    pthread_t netThread;
    pthread_create(&netThread, NULL, (void *) _fond_init_run_network, NULL);

  }

  
  /* get cluster */
  FDBCluster* clu_u = NULL;
  FDBFuture * cuf_u = NULL;
  {
    fdb_error_t err_u;
    cuf_u = fdb_create_cluster( path_c );
    err_u = fdb_future_block_until_ready( cuf_u );
    if (0 != err_u){
      fprintf(stderr, "fond_read_init create_cluster 1: %s\n", fdb_get_error( err_u ));
      goto read_init_error;
    }

    err_u =  fdb_future_get_cluster( cuf_u, & clu_u );
  
    if (0 != err_u){
      fprintf(stderr, "fond_read_init create_cluster 2: %s\n", fdb_get_error( err_u ));
      goto read_init_error;
    }
  }

  /* get database */

  FDBFuture *   daf_u = NULL;
  FDBDatabase * dab_u;
  {
    fdb_error_t err_u;
    
    daf_u = fdb_cluster_create_database( clu_u, (uint8_t *) DATB_NAME, strlen(DATB_NAME) );
    err_u = fdb_future_block_until_ready( daf_u );
    if (0 != err_u){
      fprintf(stderr, "fond_read_init create_database 1: %s\n", fdb_get_error( err_u ));
      goto read_init_error;
    }

    err_u = fdb_future_get_database( daf_u, & dab_u);
    if (0 != err_u){
      fprintf(stderr, "fond_read_init create_database 2: %s\n", fdb_get_error( err_u ));
      goto read_init_error;
    }
  }
  /* success.  Write DB handle and position into struct */
  fond_u->dab_u = dab_u;
  
  return(c3y);

 read_init_error:

  if (cuf_u){
    fdb_future_destroy( cuf_u );
  }
  if (daf_u){
    fdb_future_destroy( daf_u );
  }
  if (path_c){
    free(path_c);
  }
  
  return(c3n);


}
/* in advanced use cases, pin_c contains db usernames or passwords, e.g. "fond:user:pass", etc. */
c3_o
u3_fond_read_init(u3_pier* pir_u, c3_c * pin_c)
{

  pir_u->pin_u->fond_u = (u3_fond *) c3_malloc(sizeof (u3_sqlt));
  return(_fond_init(pir_u->pin_u->fond_u, pin_c));
}


c3_o
_fond_read_frag(FDBDatabase *  dab_u,  /* IN: FoundationDB handle */
                c3_d pos_d,       /* IN: row id */
                c3_w frg_w,       /* IN: fragment id */
                c3_y ** dat_y,    /* OUT: set pointer to data */
                c3_w * len_w,     /* OUT: set len of data */
                FDBFuture ** hand_u)   /* OUT: an opaque handle to be passed in later to a cleanup func */
{
  /* transform event ID -> key */
  c3_y key_y[256];
  c3_ws len_ws;

  _fond_sprintf_eventd(pos_d, frg_w, key_y, & len_ws);

  /* inject query, block on completion */
  FDBTransaction * tra_u;
  FDBFuture *      fut_u;
  fdb_error_t      err_u;
  err_u = fdb_database_create_transaction( dab_u, & tra_u);
  if (0 != err_u){
    fprintf(stderr, "fond_read_atom 1: %s\n", fdb_get_error( err_u ));
    goto read_error;
  }



  fut_u = fdb_transaction_get( tra_u,
                               (uint8_t const*) key_y,
                               len_ws,
                               (fdb_bool_t) 0 );

  err_u = fdb_future_block_until_ready(fut_u);
  if (0 != err_u){
    fprintf(stderr, "_fond_read_frag() 2: %s\n", fdb_get_error( err_u ));
    goto read_error;
  }

  /* read result */
  fdb_bool_t  pre_u;  /* present ? */
  c3_y * red_y = NULL;
  c3_w lan_w = 0;
  err_u = fdb_future_get_value(fut_u,
                               & pre_u,
                               (uint8_t const **) & red_y,
                               (int *) & lan_w);

  
  if (0 != err_u){
    fprintf(stderr, "_fond_read_frag() 3: %s\n", fdb_get_error( err_u ));
    goto read_error;
  }

  /* return result */
  * hand_u = fut_u;
  * dat_y = red_y;
  * len_w = lan_w;

  return ((0 == lan_w) ? c3n : c3y);

 read_error:
  if (fut_u){
    fdb_future_destroy(fut_u);
  }
  
  return(c3n);
}


typedef struct _fond_read_hand {
  FDBFuture * fut_u;
  c3_y      * dat_y;
  
} fond_read_hand;


c3_o
u3_fond_read_read(u3_pier* pir_u, c3_y **  dat_y, c3_w * len_w, void ** hand_u)
{

  FDBFuture * fut_u;

  fond_read_hand * read_u = (fond_read_hand *) malloc (sizeof (fond_read_hand));
  read_u ->fut_u = NULL;
  read_u ->dat_y = NULL;
    
  /* read first fragment */
  // fprintf(stderr, "u3_fond_read_read(): reading evnt %ld, frag 0\n\r", pir_u -> pin_u ->pos_d);

  c3_y *  dt1_y;
  c3_w    ln1_w;

  c3_o ret_o = _fond_read_frag(pir_u -> pin_u ->fond_u ->dab_u, /* IN: FoundationDB handle */
                               pir_u -> pin_u ->pos_d,          /* IN: row id */
                               0,                               /* IN: fragment id */
                               & dt1_y,                         /* OUT: set pointer to data */
                               & ln1_w,                         /* OUT: set len of data */
                               & fut_u);                        /* OUT: the fond future; nead to clean it up later */
  if (c3y != ret_o){
    return(ret_o);
  }
  
  if (ln1_w < PERS_WRIT_HEAD_SIZE ||  dt1_y[2] != ':'){
    fprintf(stderr, "u3_fond_read_read(): data is too short to have multi-fragment header\n\r");
    u3m_bail(c3__fail); 
  }
  c3_y frg_y = dt1_y[0];
  c3_y cnt_y = dt1_y[1];

  /* read first fragment */
  // fprintf(stderr, "u3_fond_read_read(): read evnt %ld, frag %i of %i\n\r", pir_u -> pin_u ->pos_d, frg_y, cnt_y);

  
  /* the first fragment is the ONLY fragment?
     Path A:
       - keep the data in the foundationDB read handle
       - pass that read handle back for later cleanup
  */
  if (cnt_y == 1){
    read_u ->fut_u = fut_u;
    * dat_y = dt1_y + PERS_WRIT_HEAD_SIZE;
    * len_w = ln1_w - PERS_WRIT_HEAD_SIZE;
    * hand_u = read_u;
    pir_u -> pin_u -> pos_d ++;
    
    return(c3y);
  }

  /* multi-fragment?
     Path B:
       - malloc space for the paste-up
       - copy all fragments in
       - clean up the multiple read handles
       - pass the malloced space back for later cleanup
  */

  c3_y *  dta_y = (c3_y *) malloc(cnt_y * MAX_SIZE);
  c3_y *  ndx_y = dta_y;
  c3_w    lna_w = 0;

  memcpy(ndx_y, dt1_y + PERS_WRIT_HEAD_SIZE, ln1_w - PERS_WRIT_HEAD_SIZE);
  lna_w += (ln1_w - PERS_WRIT_HEAD_SIZE);
  ndx_y += (ln1_w - PERS_WRIT_HEAD_SIZE);
  fdb_future_destroy( fut_u );
  
  c3_w frg_w;
  for (frg_w = 1; frg_w < cnt_y; frg_w ++){

    /* read first fragment */
    // fprintf(stderr, "u3_fond_read_read(): read evnt %ld, frag %i of %i\n\r", pir_u -> pin_u ->pos_d, frg_w, cnt_y);

    
    ret_o = _fond_read_frag(pir_u -> pin_u ->fond_u ->dab_u, /* IN: FoundationDB handle */
                            pir_u -> pin_u ->pos_d,          /* IN: row id */
                            frg_w,                           /* IN: fragment id */
                            & dt1_y,                         /* OUT: set pointer to data */
                            & ln1_w,                         /* OUT: set len of data */
                            & fut_u);                        /* OUT: the fond future; nead to clean it up later */
    if (c3y != ret_o){
      fprintf(stderr, "u3_fond_read_read(): error reading multi-fragment at fragment %i\n\r", frg_w);
      u3m_bail(c3__fail); 
    }
  
    memcpy(ndx_y, dt1_y + PERS_WRIT_HEAD_SIZE, ln1_w - PERS_WRIT_HEAD_SIZE);
    lna_w += (ln1_w - PERS_WRIT_HEAD_SIZE);
    ndx_y += (ln1_w - PERS_WRIT_HEAD_SIZE);
  
    fdb_future_destroy( fut_u );
    
  }

  /* cleanup handle */
  read_u->dat_y = dta_y;

  /* ret */
  * dat_y = dta_y; /* do not adjust for header offset; already done in memcpy() step */
  * len_w = lna_w; /* do not adjust for header offset; already done */
  * hand_u = read_u;  
  return( ret_o);

}

void
u3_fond_read_done(void * hand_u)
{
  fond_read_hand * cln_u = (fond_read_hand * ) hand_u;
  if (cln_u->fut_u){
    fdb_future_destroy( cln_u->fut_u );
  }
  if (cln_u->dat_y){
    free( cln_u->dat_y );
  }
  free(cln_u);
}

void
u3_fond_read_shut(u3_pier* pir_u)
{


}

/* write */

c3_o
u3_fond_write_init(u3_pier* pir_u, c3_c * pot_c)
{
  pir_u->pot_u->fond_u = (u3_fond *) c3_malloc(sizeof (u3_sqlt));

  u3_fond * fin_u = pir_u->pin_u->fond_u; /* FoundationDB input struct (maybe NULL) */
  u3_fond * fot_u = pir_u->pot_u->fond_u; /* FoundationDB output struct */
  
  /* quick init, if we're already using foundationDB  for input */
  if (fin_u){
    pir_u->pot_u->pos_d = 1;
    
    fot_u->dab_u = fin_u->dab_u;
    fot_u->pir_u = pir_u;
    return(c3y);
  } else {
    /* ...or slow init, if needed */
    return(_fond_init(fot_u, pot_c));
  }
}

/*
     fond_write_cb_data   # new instance used with each fragment write; passed to _fond_write_cb()
           .wrt_u
            |
            V
     u3_writ              # obvious
           .mwh_u
            |
            V
     fond_mult_writ_hand  # multi-write-handle (persistent across all fragment writes for a given writ)
*/


/* this is for use INSIDE the write, written by _fond_write_part(), and read by _fond_write_cb() */
typedef struct _fond_mult_writ_hand {
  c3_w     cnt_w;   /* total number of fragments */
  c3_o   * don_o;   /* array of flags, one per fragment. index n: has frag n been written yet? */
  c3_y   * buf_y;   /* tmp buffer to be freed after last write */

} fond_mult_writ_hand;


typedef struct _fond_write_cb_data {
  /* "true" callback data, that the callback needs for callbacky stuff */
  
  u3_writ * wit_u;  /* the writ from which this fragment comes */
  c3_w     cnt_w;   /* total number of fragments */
  c3_w     frg_w;   /* index of this fragment */
  c3_w     try_w;   /* retry count */

  /* "fake" callback data, that the callback may need for retry on certain error codes */
  c3_y * ked_y; /* key */
  c3_ws kel_ws; 
  c3_y* byt_y; /* data */
  c3_w  len_w;  

  writ_test_cb  cbf_u ; /* only for testing */
  
} fond_write_cb_data;

void _fond_write_frag_core(u3_writ *  wit_u,
                           c3_y * ked_y, /* key */
                           c3_ws kel_ws, 
                           c3_y* byt_y, /* data */
                           c3_w  len_w,  
                           fond_write_cb_data * calb_data_u
                           );


/* This function gets invoked when a foundationDB write finished.
   It does two things:

     - marks a particular fragment of a larger write as done
     - if ALL the the fragments of that write are done, marks the writ as successfully written 
*/

void _fond_write_cb(FDBFuture* fut_u,
                    void* vod_u)
{
  fond_write_cb_data  * cbd_u = (fond_write_cb_data *)    vod_u;
  fond_mult_writ_hand * mwh_u = (fond_mult_writ_hand *)   cbd_u->wit_u->mwh_u;
  c3_w                  frg_w = cbd_u -> frg_w;
  c3_w                  cnt_w = cbd_u -> cnt_w;

  if (frg_w > cnt_w){
    fprintf(stderr, "fond_write_cb: evt %ld, frag %i > count %i\n\r", cbd_u->wit_u->evt_d, frg_w, cnt_w);
    u3m_bail(c3__fail); 
    return;
  }
  
  fdb_error_t err_u = fdb_future_get_error( fut_u );

  /* FoundationDB error: COMMIT_UNKNOWN_RESULT (1021) "Transaction may or may not have committed" */
  if (1021 == err_u){
    /* retry, as per https://apple.github.io/foundationdb/javadoc/com/apple/foundationdb/Transaction.html
       ...but don't be stupid about it
    */
    if (cbd_u->try_w > MAX_RETRY){
      fprintf(stderr, "fond_write_cb: evt %ld, frag %i, result %s, 'transaction may or may not have committed' retry %i exceeds max\n\r", cbd_u->wit_u->evt_d, frg_w, fdb_get_error( err_u ), cbd_u -> try_w);
      u3m_bail(c3__fail); 
      return;
    }

    cbd_u->try_w ++;

    _fond_write_frag_core(cbd_u->wit_u,
                                                    
                          cbd_u ->ked_y,
                          cbd_u ->kel_ws,
                          
                          cbd_u -> byt_y,
                          cbd_u -> len_w,

                          cbd_u
                          
                          );

    
    /* note that we RETURN here, instead of continuing on, because we've just injected a new write and that write will have its own callback, so no need to complete this one */
    return;
  } else if (0 != err_u){
    fprintf(stderr, "fond_write_cb: evt %ld, frag %i, result %s\n\r", cbd_u->wit_u->evt_d, frg_w, fdb_get_error( err_u ));
    u3m_bail(c3__fail); 
    return;
  }

  mwh_u->don_o[frg_w] = c3y;  /* ACTION 1: mark this fragment is written */
  c3_w itr_w;
  c3_o all_o = c3y;

  /* cleanup 1/2: just this fragment */
  free(cbd_u->ked_y);     /* free key ( used by just this fragment ) */




  /* only the final (by index #) fragment thread is responsible for cleanup */
  if (frg_w != (cnt_w - 1)){
    free(cbd_u);
    return;
  }

  while(1) {
  
    /* see if all fragments are done */
    for (itr_w = 0; itr_w < cnt_w; itr_w ++){
      if (mwh_u->don_o[itr_w] == c3n){
        all_o = c3n;
        break;
      }
    }
    if (c3y == all_o){
      goto  complete;
    }
    
    sleep(1);
  }

 complete:
  
  if (c3y == all_o){
    cbd_u->wit_u->ped_o = c3y;  /* ACTION 2: mark the writ as fully written */
  }

  /* if a meta-callback is set, call it (for testing) */
  if (cbd_u->cbf_u){
    cbd_u->cbf_u(cbd_u);
  }
  
  /* cleanup 2/2: shared multi-write handle */  
  if (c3y == all_o){
    // fprintf(stderr, "fond_write_cb: evt %ld, frag %i, all fragments done : CLEANUP\n\r", cbd_u->wit_u->evt_d, frg_w);

    free(mwh_u->don_o);     /* loob vector */
    free(mwh_u->buf_y);     /* data buffer of event ( shared by all fragments ) */
    free(mwh_u); 
    free(cbd_u);
  }
}

/* used by both _fond_write_part() and in retry by _fond_write_cb() */
void _fond_write_frag_core(u3_writ *  wit_u,
                           c3_y * ked_y, /* key */
                           c3_ws kel_ws, 
                           c3_y* byt_y, /* data */
                           c3_w  len_w,  
                           fond_write_cb_data * calb_data_u
                           )
{
  
  FDBDatabase *  dab_u = wit_u->pir_u->pot_u ->fond_u ->dab_u;


  FDBTransaction* tra_u = NULL;
  fdb_error_t err_u;

  err_u = fdb_database_create_transaction( dab_u, & tra_u);
  if (0 != err_u){
    fprintf(stderr, "fond_write_frag_core 1: %s\n", fdb_get_error( err_u ));
    goto write_error;
  }

    /* write the event blob */  
  
  fdb_transaction_set(tra_u,                  /* transaction */
                      ked_y,                  /* key */
                      kel_ws,                 /* key len */
                      (uint8_t const*) byt_y, /* data */
                      (int) len_w);          /* data len */


  FDBFuture * fut_u = fdb_transaction_commit( tra_u );

  err_u =  fdb_future_set_callback( fut_u,
                                    _fond_write_cb,
                                    (void*) calb_data_u );
  if (0 != err_u){
    fprintf(stderr, "fond_write_frag_core 2: %s\n", fdb_get_error( err_u ));
    goto write_error;
  }
    
  return;

 write_error:

  u3m_bail(c3__fail); 

}

void
_fond_write_frag(u3_writ* wit_u,      /* IN: writ */
                 c3_d pos_d,          /* IN: row id */
                 c3_w frg_w,          /* IN: fragment index */
                 c3_w cnt_w,          /* IN: total fragment count */
                 c3_y* byt_y,         /* fragment */
                 c3_w  len_w,          /* IN: frag len */
                 writ_test_cb test_cb
                 )
{
  
  /* sanity check args */
  if (frg_w > 255 || cnt_w > 255 ||  frg_w > cnt_w ){
    fprintf(stderr, "fond_write_frag: problem with frag (%i) or count (%i)\n\r", frg_w, cnt_w);
    u3m_bail(c3__fail);
    return;
  }

  /* transform args */
  c3_y * ked_y = (c3_y * ) malloc(256);   /* transform event ID -> key */
  c3_ws kel_ws;
  _fond_sprintf_eventd(pos_d, frg_w, ked_y, & kel_ws);
  

  /* write header into data */
  byt_y = byt_y - PERS_WRIT_HEAD_SIZE;
  len_w += PERS_WRIT_HEAD_SIZE;
  
  byt_y[0] = frg_w & 0xff;
  byt_y[1] = cnt_w & 0xff;
  byt_y[2] = ':';
  
  
  /* setup callback data */
  fond_write_cb_data * calb_data_u = (fond_write_cb_data *) malloc(sizeof (fond_write_cb_data));

  /* "true" callback data, that the callback needs for callbacky stuff */
  calb_data_u-> wit_u = wit_u;
  calb_data_u-> cnt_w = cnt_w;
  calb_data_u-> frg_w = frg_w;
  calb_data_u-> try_w = 0;

  /* "fake" callback data, that the callback may need for retry on certain error codes */
  calb_data_u->   ked_y = ked_y; /* key */
  calb_data_u->   kel_ws = kel_ws; 
  calb_data_u->   byt_y = byt_y; /* data */
  calb_data_u->   len_w = len_w;  

  /* "speed / testing" callback func */
  calb_data_u->   cbf_u = test_cb;
  
  _fond_write_frag_core(wit_u,         /* writ */
                        ked_y, kel_ws, /* key */
                        byt_y, len_w,  /* data */
                        calb_data_u);      
}


void
u3_fond_write_write(u3_writ* wit_u,       /* IN: writ */
                    c3_d pos_d,           /* IN: row id */
                    c3_y* buf_y,          /* IN: buffer (to be freed later) */
                    c3_y* byt_y,          /* IN: data (located inside buffer above, but don't worry about that) */
                    c3_w  len_w,          /* IN: data len */
                    writ_test_cb test_cb          /* IN: void * (callback function) for testing - set NULL */
                     )
{
  
  c3_w rem_w = len_w;
  c3_w frg_w = 0;

  c3_w cnt_w = len_w / MAX_SIZE + (0 == (len_w % MAX_SIZE) ? 0 : 1);

  /* setup multi-write handle */
  fond_mult_writ_hand * mwh_u = (fond_mult_writ_hand *) malloc(sizeof(fond_mult_writ_hand));
  mwh_u->cnt_w = cnt_w;
  mwh_u->don_o = (c3_o   *) malloc( sizeof(c3_o) * cnt_w);
  memset(mwh_u->don_o, c3n, cnt_w);   /* setup the fragment flag vector */
  mwh_u -> buf_y = buf_y;

  wit_u->mwh_u = (void *) mwh_u;
  for(frg_w = 0; frg_w < cnt_w; frg_w++){
    c3_w frg_len_w = ( rem_w - MAX_SIZE > 0 ) ? MAX_SIZE : rem_w;
    _fond_write_frag(wit_u,    
                     pos_d,
                     frg_w,
                     cnt_w,
                     byt_y + (MAX_SIZE * frg_w), /* fragment */
                     frg_len_w,
                     test_cb
                     );
    rem_w = ( rem_w - MAX_SIZE > 0 ) ? rem_w - MAX_SIZE : 0;
  }
}


void
u3_fond_write_shut(u3_pier* pir_u)
{

}


