/* vere/fond.c
**
**  This file is in the public domain.
**
** BUGS:
**     - on write of multi-fragment, can't just split in place, bc 3 byte step-back to write header; need to copy
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
#define MAX_SIZE 65536  /* max size without fragmenting = 2^16 = 65,536 bytes before DB truncates data */
#define MAX_RETRY 5


c3_w u3_fond_frag_size()
{
  return(MAX_SIZE);
}


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
  char * path_c = NULL;
  
  FDBCluster* clu_u = NULL;
  FDBFuture * cuf_u = NULL;

  FDBDatabase * dab_u = NULL;
  FDBFuture *   daf_u = NULL;
  

  /* build file path */
  {
    c3_c * tmp_c = (c3_c *) strdup(pin_c);
    c3_c * sav_c = NULL;
    strtok_r(tmp_c, ":", & sav_c);
    fprintf(stderr, "sav_c = %s\n\r", sav_c);
    free(tmp_c);

    path_c = malloc(strlen(u3C.dir_c) + strlen(CLUS_NAME) + 2);
    sprintf(path_c, "./%s/%s", u3C.dir_c, CLUS_NAME);

    path_c = strdup("/etc/foundationdb/fdb.cluster"); // NOTFORCHECKIN
  
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
_fond_read_frag(u3_pers *  pers_u,  /* IN: FoundationDB handle */
                c3_d pos_d,       /* IN: row id */
                c3_w frg_w,       /* IN: fragment id */
                c3_y ** dat_y,    /* OUT: set pointer to data */
                c3_w * len_w,     /* OUT: set len of data */
                void ** opaq_u)   /* OUT: an opaque handle to be passed in later to a cleanup func */
{
  FDBDatabase *  dab_u = pers_u->fond_u->dab_u;
  
  /* transform event ID -> key */
  c3_y key_y[256];
  c3_ws len_ws;

  _fond_sprintf_eventd(pos_d, frg_w, key_y, & len_ws);

  /* inject query, block on completion */
  FDBTransaction * tra_u = NULL;
  FDBFuture *      fut_u = NULL;
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
  * opaq_u = fut_u;
  * dat_y = red_y;
  * len_w = lan_w;

  return ((0 == lan_w) ? c3n : c3y);

 read_error:
  if (fut_u){
    fdb_future_destroy(fut_u);
  }
  
  return(c3n);
}

void
_fond_read_done(void * opaq);


c3_o
u3_fond_read_read(u3_pier* pir_u, c3_y **  dat_y, c3_w * len_w, void ** hand_u)
{
  c3_o ret_o = u3_frag_read(_fond_read_frag,
                            _fond_read_done,
                            MAX_SIZE,
                            pir_u->pin_u,
                            dat_y,
                            len_w,
                            (mult_read_hand ** ) hand_u);
  return(ret_o);
}

void
_fond_read_done(void * opaq)
{
  fdb_future_destroy( (FDBFuture*) opaq );
}

void
u3_fond_read_done(void * opaq)
{
  u3_frag_read_done(opaq, _fond_read_done);
}


void
u3_fond_read_shut(u3_pier* pir_u)
{

  /* nothing */
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
     mult_writ_hand  # multi-write-handle (persistent across all fragment writes for a given writ)
*/



/* extension to u3_pers_writ_calb */
typedef struct _fond_writ_calb {

  c3_y * ked_y; /* key */
  c3_ws kel_ws; 
  c3_y* byt_y; /* data */
  c3_w  len_w;

  c3_w   try_w;		/* retry count */

  
} fond_writ_calb;

void _fond_write_frag_core(u3_writ *  wit_u,
                           c3_y *     ked_y, /* key */
                           c3_ws      kel_ws, 
                           c3_y*      byt_y, /* data */
                           c3_w       len_w,  
                           u3_pers_writ_calb * pwc_u );


/* This function gets invoked when a foundationDB write finished.
   It does two things:

     - marks a particular fragment of a larger write as done
     - if ALL the the fragments of that write are done, marks the writ as successfully written 
*/

void _fond_write_cb(FDBFuture* fut_u, void* opq_u)
{
  u3_pers_writ_calb  * pwc_u = (u3_pers_writ_calb *)    opq_u;
  u3_pers_frag *   frg_u  = pwc_u ->frg_u;
  fond_writ_calb * fon_u = (fond_writ_calb *) pwc_u ->ext_u;
  
  u3_frag_write_check(pwc_u);


  
  c3_w                  frg_w = pwc_u -> frg_w;
  c3_w                  cnt_w = pwc_u -> cnt_w;

  //  fprintf(stderr, "fond_write_cb: evt %ld, frag %u, count %u\n\r", pwc_u->wit_u->evt_d, frg_w, cnt_w);

  /* did the write succeed ? */
  fdb_error_t err_u = fdb_future_get_error( fut_u );

  /* FoundationDB error: COMMIT_UNKNOWN_RESULT (1021) "Transaction may or may not have committed" */
  if (1021 == err_u){
    /* retry, as per https://apple.github.io/foundationdb/javadoc/com/apple/foundationdb/Transaction.html
       ...but don't be stupid about it
    */
    if (fon_u->try_w > MAX_RETRY){
      fprintf(stderr, "fond_write_cb: evt %ld, frag %i, result %s, 'transaction may or may not have committed' retry %i exceeds max\n\r", pwc_u->wit_u->evt_d, frg_w, fdb_get_error( err_u ), fon_u -> try_w);
      u3m_bail(c3__fail); 
      return;
    }

    fon_u->try_w ++;

    _fond_write_frag_core(pwc_u->wit_u,
                                                    
                          fon_u-> ked_y,
                          fon_u-> kel_ws,
                          
                          fon_u-> byt_y,
                          fon_u-> len_w,

                          pwc_u
                          
                          );

    
    /* note that we RETURN here, instead of continuing on, because we've just injected a new write and that write will have its own callback, so no need to complete this one */
    return;
  } else if (0 != err_u){
    fprintf(stderr, "fond_write_cb: evt %ld, frag %i, result %s\n\r", pwc_u->wit_u->evt_d, frg_w, fdb_get_error( err_u ));
    u3m_bail(c3__fail); 
    return;
  }

  /* tell the fragmenting layer that we're done reading this fragment */
  
  c3_o don_o = u3_frag_write_done(frg_w,
                                  cnt_w,
                                  pwc_u->wit_u,
                                  frg_u);

  /* if a meta-callback is set, call it (for testing) */
  if (c3y == don_o && pwc_u->cbf_u){
    pwc_u->cbf_u(pwc_u);
  }


  
  /* cleanup callback data: (a) extended callback data */
  free(fon_u->ked_y);       /* key */
  if (frg_w != 0){          /* all but first fragment's data are in space malloced by frag.c ; clean that */
    free(fon_u->byt_y);
  }
  free(fon_u);  

  /* cleanup callback data: (b) regular callback data */
  free(pwc_u);  
  
}

/* used by both _fond_write_part() and in retry by _fond_write_cb() */
void _fond_write_frag_core(u3_writ *  wit_u,
                           c3_y * ked_y, /* key */
                           c3_ws kel_ws, 
                           c3_y* byt_y, /* data */
                           c3_w  len_w,  
                           u3_pers_writ_calb * pwc_u )
{
  FDBDatabase *  dab_u = wit_u->pir_u->pot_u ->fond_u ->dab_u;


  FDBTransaction* tra_u = NULL;
  fdb_error_t err_u;

  err_u = fdb_database_create_transaction( dab_u, & tra_u);
  if (0 != err_u){
    fprintf(stderr, "fond_write_frag_core 1: %s\n", fdb_get_error( err_u ));
    u3m_bail(c3__fail); 
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
                                    (void*) pwc_u );
  if (0 != err_u){
    fprintf(stderr, "fond_write_frag_core 2: %s\n", fdb_get_error( err_u ));
    u3m_bail(c3__fail); 
  }
    
  return;
}

void
_fond_write_frag(u3_writ* wit_u,      /* IN: writ */
                 c3_d pos_d,          /* IN: row id */
                 c3_w frg_w,          /* IN: fragment index */
                 c3_w cnt_w,          /* IN: total fragment count */
                 c3_y* byt_y,         /* IN: fragment bytes (with header) */
                 c3_w  len_w,         /* IN: frag len */
                 u3_pers_frag * mwh_u, /* IN: multi-frag handle  */
                 writ_test_cb test_cb
                 )
{

  /* transform args */
  c3_y * ked_y = (c3_y * ) malloc(256);   /* transform event ID -> key */
  c3_ws kel_ws;
  _fond_sprintf_eventd(pos_d, frg_w, ked_y, & kel_ws);

  
  /* fond specific callback data that may need for retry on certain error codes */
  fond_writ_calb * fwc_u = (fond_writ_calb *) malloc(sizeof(fond_writ_calb));
  bzero(fwc_u, sizeof(fond_writ_calb) );
  fwc_u-> ked_y = ked_y; /* key */
  fwc_u-> kel_ws = kel_ws;
  fwc_u-> byt_y = byt_y; /* data */
  fwc_u-> len_w = len_w;  
  fwc_u-> try_w = 0;
  
  /* generic callback data */
  u3_pers_writ_calb * pwc_u = (u3_pers_writ_calb *) malloc(sizeof (u3_pers_writ_calb));
  bzero(pwc_u, sizeof(u3_pers_writ_calb) );
  pwc_u-> wit_u = wit_u;
  pwc_u-> cnt_w = cnt_w;
  pwc_u-> frg_w = frg_w;
  pwc_u-> ext_u = (void *) fwc_u;
  pwc_u-> cbf_u = test_cb;
  pwc_u-> frg_u = mwh_u;
  
  _fond_write_frag_core(wit_u,         /* writ */
                        ked_y, kel_ws, /* key */
                        byt_y, len_w,  /* data */
                        pwc_u);      
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
  frag_writ(MAX_SIZE,
            wit_u,      
            pos_d,          
            buf_y,
            byt_y,         
            len_w,         
            _fond_write_frag,   /* actual fond write function */ 
            test_cb);
}



void
u3_fond_write_shut(u3_pier* pir_u)
{

}


