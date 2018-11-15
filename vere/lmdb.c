#include <uv.h>
#include <string.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>  // sysconf() - get CPU count
#include <time.h>
#include <errno.h>

#include "all.h"
#include "lmdb.h"
#include "vere/vere.h"


#define DATB_NAME  "lmdb_db"

c3_o
_lmdb_init_comn(u3_pers * pers_u, c3_c * sto_c)
{
  pers_u->lmdb_u = malloc(sizeof (u3_lmdb));
  
  c3_y * path_c = (c3_y *) malloc( strlen(u3C.dir_c) + strlen(DATB_NAME) + 2);
  sprintf((char *) path_c, "./%s/%s", (char *) u3C.dir_c, (char *) DATB_NAME);
  fprintf(stderr, "lmdb init: db path = %s\n\r", (char *) path_c);

  /* verify directory exists */
  c3_w      ret_w;
  struct stat buf_u;
  ret_w = stat( (const char *) path_c, & buf_u );
  if (0 != ret_w) {
    // fprintf(stderr, "lmdb_init_comn stat() fail : %s\n", strerror(errno));
    //  if (! S_ISDIR(buf_u.st_mode)){
    // 
    // fprintf(stderr, "doesn't exist\n");
    //
    ret_w = mkdir( (const char *) path_c, S_IRWXU | S_IRWXG | S_IROTH);
    if (0 != ret_w) {
      fprintf(stderr, "lmdb_init_comn mkdir() fail : %s\n", strerror(errno));
    }
  }
  

  /* create environment */
  ret_w = mdb_env_create(& pers_u->lmdb_u->env_u);
  if (0 != ret_w){

    fprintf(stderr, "lmdb lmdb init fail: %s\n", mdb_strerror(ret_w));
    u3m_bail(c3__fail); 
    return(c3n);
  }

    ret_w = mdb_env_set_maxdbs(pers_u->lmdb_u->env_u, 16);
  if (0 != ret_w){
    fprintf(stderr, "mdb_env_set_maxdbs() fail: %s\n", mdb_strerror(ret_w));
    u3m_bail(c3__fail); 
    return(c3n);
  }

  /* open environment */
  c3_w     flag_w = 0;
  mdb_mode_t mode_u = 0777;  /* unix file permission in octal */

  ret_w = mdb_env_open(pers_u->lmdb_u->env_u,
                       (char *) path_c,
                       flag_w,
                       mode_u);
  if (0 != ret_w){
    fprintf(stderr, "lmdb lmdb open fail: %s\n", mdb_strerror(ret_w));
    u3m_bail(c3__fail); 
    return(c3n);
  }

  
  /* create the root level transaction */
  ret_w = mdb_txn_begin(pers_u->lmdb_u->env_u,
                        (MDB_txn *) NULL,
                        0, /* flags */
                        & pers_u->lmdb_u->txn_u);
  if (0 != ret_w){
    fprintf(stderr, "lmdb lmdb txn_begin fail: %s\n", mdb_strerror(ret_w));
    u3m_bail(c3__fail); 
    return(c3n);
  }

  /* open database */
  c3_w flags_w = MDB_CREATE;
  ret_w =  mdb_dbi_open(pers_u->lmdb_u->txn_u,
                        DATB_NAME,
                        flags_w,
                        pers_u->lmdb_u->dbi_u);
  if (0 != ret_w){
    fprintf(stderr, "lmdb lmdb dbi_open fail: %s\n", mdb_strerror(ret_w));
    u3m_bail(c3__fail); 
    return(c3n);
  }

  return(c3y);
}


c3_o
_lmdb_shut_comn(u3_pers * pers_u)
{
  mdb_dbi_close(pers_u->lmdb_u->env_u,
                * pers_u->lmdb_u->dbi_u);

  return(c3y);
}

/* read */

c3_o
u3_lmdb_read_init(u3_pier* pir_u, c3_c * sto_c)
{
  /* pick out our 'db input handle', pass that in to common code */
  return( _lmdb_init_comn(pir_u ->pin_u, sto_c));

}



c3_o
u3_lmdb_read_read(u3_pier* pir_u,  c3_y ** dat_y, c3_w * len_w, void ** hand_u)
{
  u3_lmdb * lmdb_u = pir_u -> pin_u->lmdb_u;

  c3_w      ret_w;

  MDB_val key_u;
  MDB_val * val_u = (MDB_val *) malloc(sizeof(MDB_val));

  key_u.mv_data = & pir_u->pin_u->pos_d;
  key_u.mv_size = sizeof(pir_u->pin_u->pos_d);
  
  ret_w =  mdb_get(lmdb_u->txn_u,
                   * lmdb_u->dbi_u,
                   & key_u,
                   val_u);
  if (0 != ret_w){
    fprintf(stderr, "lmdb mdb_get() fail: %s\n", mdb_strerror(ret_w));
    return(c3n);
  }

  * dat_y = val_u->mv_data;
  * len_w = val_u->mv_size;
  * hand_u = val_u;
  return(c3y);
}



void
u3_lmdb_read_done(void * hand_u)
{
  MDB_val * val_u = (MDB_val *) hand_u;
  free(val_u);
}



void
u3_lmdb_read_shut(u3_pier* pir_u)
{

}



c3_o
u3_lmdb_write_init(u3_pier* pir_u, c3_c * sto_c)
{
  /* share single db handle, if for both in and out */
  if (pir_u->pin_u->lmdb_u){
    fprintf(stderr, "lmdb write init: sharing db handle with lmdb read\n\r");
    pir_u->pot_u->lmdb_u = pir_u->pin_u->lmdb_u;
    return(c3y);
  } else {
    /* pick out our 'db input handle', pass that in to common code */
    return( _lmdb_init_comn(pir_u->pin_u, sto_c));
  }
}

void
_lmdb_write(u3_writ* wit_u, c3_d pos_d, c3_y* buf_y, c3_y* byt_y, c3_w  len_w)
{
  MDB_val key_u;
  MDB_val val_u;

  /* create the leaf-node transaction */
  MDB_txn *txn_u;
  fprintf(stderr, "XXX-1\n");
  c3_w ret_w = mdb_txn_begin(wit_u->pir_u->pot_u->lmdb_u->env_u,
                             (MDB_txn *) NULL, // wit_u->pir_u->pot_u->lmdb_u->txn_u,
                             0, /* flags */
                             & txn_u);
  fprintf(stderr, "XXX-2\n");
  if (0 != ret_w){
    fprintf(stderr, "u3_lmdb_write_write()  mdb_txn_begin() fail: %s\n", mdb_strerror(ret_w));
    u3m_bail(c3__fail); 
  }


  
  key_u.mv_data = & pos_d;
  key_u.mv_size = sizeof(pos_d);

  val_u.mv_data = byt_y;
  val_u.mv_size = len_w;
  
  ret_w =  mdb_put(txn_u,  /* leaf node transaction */
                   * wit_u->pir_u->pot_u->lmdb_u->dbi_u,
                   & key_u,
                   & val_u,
                   0); /* flag */
  
  if (0 != ret_w){
    fprintf(stderr, "lmdb mdb_put() fail: %s\n", mdb_strerror(ret_w));
    mdb_txn_abort(txn_u);
    u3m_bail(c3__fail); 
  }

  ret_w = mdb_txn_commit(txn_u);
  if (0 != ret_w){
    fprintf(stderr, "lmdb txn_commit() fail: %s\n", mdb_strerror(ret_w));
    u3m_bail(c3__fail); 
  }
  
}

typedef struct _write_cb_data {
  u3_writ * wit_u;  /* the writ from which this fragment comes */
  c3_y   * buf_y;   /* tmp buffer to be freed after write */
  c3_w     len_w;
  writ_test_cb  cbf_u ; /* only for testing */
} write_cb_data;


void *
_lmdb_write_cast(void *opq_u)
{
  fprintf(stderr, "WRITE THREAD start\n\r");
  
  write_cb_data* cbd_u = (write_cb_data*) opq_u;
  _lmdb_write(cbd_u->wit_u,
              cbd_u->wit_u->evt_d,
              cbd_u->buf_y,
              cbd_u->buf_y,
              cbd_u->len_w);

  /* set the ack */
  cbd_u->wit_u->ped_o = c3y;

  /* if a meta-callback is set, call it (for testing) */
  if (cbd_u->cbf_u){
    cbd_u->cbf_u(cbd_u);
  }

  fprintf(stderr, "****************************** AFTER WRITE - PARENT THREAD\n\r");
  sleep(2); // NOTFORCHECKIN
  
  /* cleanup */
  free(cbd_u);
  
  fprintf(stderr, "WRITE THREAD end\n\r");
  pthread_exit(NULL);
  return(NULL); 
}



void
u3_lmdb_write_write(u3_writ* wit_u, c3_d pos_d, c3_y* buf_y, c3_y* byt_y, c3_w  len_w, writ_test_cb test_cb)
{
  pthread_t tid_u;
  uint32_t       ret_w;

  write_cb_data * cbd_u = (write_cb_data *) malloc(sizeof(write_cb_data));
  cbd_u->wit_u = wit_u;
  cbd_u->buf_y = buf_y + PERS_WRIT_HEAD_SIZE;
  cbd_u->len_w = len_w;
  cbd_u->cbf_u = test_cb;
  
  if (0 != (ret_w = pthread_create(& tid_u,
                                   NULL,
                                   _lmdb_write_cast,  
                                   (void *) cbd_u ))) {
    fprintf(stderr, "u3_sqlt_write_commit() : %s \n\r", strerror(ret_w));
    u3m_bail(c3__fail); 
    return;
  }

  fprintf(stderr, "launched thread\n");
  return;
}



void
u3_lmdb_write_shut(u3_pier* pir_u)
{

}



