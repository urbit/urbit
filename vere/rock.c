/* vere/rock.c
**
**  This file is in the public domain.
**
**  21 Nov 2018 on Ubuntu 16.04: max noun size = 2^20 = 1,048,576 bytes.  2^21 = segv 
**  No need for fragmenting.
**
**  N.B. RocksDB takes ownership of the buffer that you pass in to
**  u3_rock_write_write() / _rock_write_write().  As written, we free
**  that buffer in the write CB.  If you do something weird and free
**  it before then, RocksDB will still reference that memory and will
**  do weird things!
** 
** BUGS / OPEN ISSUES:
**     * uses data structure rock_write_cb_data, should use u3_pers_writ_calb
*/

#include "all.h"

#include <uv.h>
#include <string.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>  /* sysconf() - get CPU count */
#include <time.h>
#include <errno.h>

#include "vere/vere.h"


#define THREAD 1
#define DATB_NAME  "rocks_db"
#define ROCK_COMPRESSION_LEVEL rocksdb_no_compression

c3_w u3_rock_frag_size()
{
  return(0);  /* 0 == do not use fragmenting */
}


/* common initialization code, used by both read init and write init */

c3_o
_rock_init_comn(u3_pers * pers_u, c3_c * sto_c)
{
  /* persistence data struct */
  pers_u->rock_u = c3_malloc(sizeof (u3_rock));
  bzero(pers_u->rock_u, sizeof (u3_rock));

  /* set filename 
     N.B. that rocks will barf LATER if you pass in a RELATIVE path here. Absolute path only!
  */
  c3_y * cwd_c = (c3_y *) get_current_dir_name();
  
  c3_y * path_y = (c3_y *) malloc(strlen( (char *) cwd_c) + strlen(u3C.dir_c) + strlen(DATB_NAME) + 3);
  sprintf((char *) path_y, "%s/%s/%s", cwd_c, u3C.dir_c, DATB_NAME);
  /* fprintf(stderr, "rocks read init: write db path = %s\n\r", path_y); */

  
  /* path to db directory: ensure directory exists */
  c3_w      ret_w;
  struct stat buf_u;
  ret_w = stat( (const char *) path_y, & buf_u );
  if (0 != ret_w) {
    ret_w = mkdir( (const char *) u3C.dir_c, S_IRWXU | S_IRWXG | S_IROTH);
    if (0 != ret_w) {
      fprintf(stderr, "rock_init_comn mkdir(%s) 1 fail : %s\n", u3C.dir_c, strerror(errno));
    }
  }


  /* set db options  */
  pers_u->rock_u->rop_u = rocksdb_options_create();
  long cpus = sysconf(_SC_NPROCESSORS_ONLN); 
  rocksdb_options_set_compression(pers_u->rock_u->rop_u, ROCK_COMPRESSION_LEVEL);
  rocksdb_options_increase_parallelism(pers_u->rock_u->rop_u, (int)(4 * cpus));
  rocksdb_options_optimize_level_style_compaction(pers_u->rock_u->rop_u, 0);
  rocksdb_options_set_use_fsync(pers_u->rock_u->rop_u, 1);  /*  switches from fdatasync() to fsync(). Recommended for ext3 */
  

  
  /* create or open db */
  rocksdb_options_set_create_if_missing(pers_u->rock_u->rop_u, 1);
  char *err = NULL;
  pers_u->rock_u->rok_u = rocksdb_open(pers_u->rock_u->rop_u, (char *) path_y, &err);  /* should create files */
  if (err){
    fprintf(stderr, "_rock_init_comn() error = %s\n", err);
    return(c3n);
  }

  /* set write options */
  pers_u->rock_u->wri_u = rocksdb_writeoptions_create();
  rocksdb_writeoptions_set_sync(pers_u->rock_u->wri_u, 1); /* forces full sync on each write  */

  
  /* set read options */
  pers_u->rock_u->red_u= rocksdb_readoptions_create();
  

  return(c3y);

}


c3_o
_rock_shut_comn(u3_pers * pers_u)
{
  if (pers_u->rock_u->wri_u){
    rocksdb_writeoptions_destroy(pers_u->rock_u->wri_u);
    pers_u->rock_u->wri_u = NULL;
  }
  if (pers_u->rock_u->red_u){
    rocksdb_readoptions_destroy( pers_u->rock_u->red_u);
    pers_u->rock_u->red_u = NULL;
  }
  if (pers_u->rock_u->rop_u){
    rocksdb_options_destroy(pers_u->rock_u->rop_u);
    pers_u->rock_u->rop_u = NULL;
  }
  if (pers_u->rock_u->rok_u){
    rocksdb_close(pers_u->rock_u->rok_u);
    pers_u->rock_u->rok_u = NULL;
  }

  return(c3y);
}

/* read */


c3_o
u3_rock_read_init(u3_pier* pir_u, c3_c * sto_c)
{
  /* pick out our 'db input handle', pass that in to common code */
  return( _rock_init_comn(pir_u ->pin_u, sto_c));
}


c3_o
u3_rock_read_read(u3_pier* pir_u,  c3_y ** dat_y, c3_w * len_w, void ** hand_u)
{
  c3_y key_y[32];
  c3_y * err_y = NULL;
  c3_w lan_w = 0;
  c3_y * red_y = NULL;

  
  sprintf((char *) key_y, "%ld", pir_u -> pin_u ->pos_d);
  red_y = (c3_y *) rocksdb_get(pir_u->pin_u->rock_u->rok_u,             /* IN: db handle */
                               pir_u->pin_u->rock_u->red_u,             /* IN: read options */
                               (char *) key_y, strlen( (char *) key_y), /* IN: key */
                               (size_t *) & lan_w,                      /* OUT: len */
                               (char **) & err_y);                      /* OUT: error */


  if (NULL == red_y){
    return( c3n);    
  } else {
    *dat_y = red_y;
    *hand_u = *dat_y;  /* note that in rocksdb handle == data, and clean == free() */
    *len_w = lan_w;
    pir_u->pin_u->pos_d++;
    return( c3y);
  }

  
}

void
u3_rock_read_done(void * hand_u)
{
  free(hand_u);
}

void
u3_rock_read_shut(u3_pier* pir_u)
{
  /* pick out our 'db input handle', pass that in to common code */
   _rock_shut_comn(pir_u->pin_u);

}

/* write */

c3_o
u3_rock_write_init(u3_pier* pir_u, c3_c * sto_c)
{
  /* share single db handle, if for both in and out */
  if (pir_u->pin_u->rock_u){
    fprintf(stderr, "rock write init: sharing db handle with rock read\n\r");
    pir_u->pot_u->rock_u = pir_u->pin_u->rock_u;
    return(c3y);
  } else {
    /* pick out our 'db input handle', pass that in to common code */
    return( _rock_init_comn(pir_u->pin_u, sto_c));
  }
}

void
_rock_write_write(u3_writ* wit_u, c3_d pos_d, c3_y* buf_y, c3_y* byt_y, c3_w  len_w, writ_test_cb test_cb)
{


  c3_y key_y[32];
  sprintf((char *) key_y, "%ld", wit_u->evt_d);

  c3_y * err_y = NULL;
  rocksdb_put(wit_u->pir_u->pot_u->rock_u->rok_u,   /* IN: db handle */
              wit_u->pir_u->pot_u->rock_u->wri_u,   /* IN: write options */
              (char *)key_y, strlen((char *)key_y), /* IN: key */
              (char *) buf_y, len_w,                /* IN: value */
              (char **) &err_y);                    /* OUT: error */

  
  if (err_y){
    fprintf(stderr, "u3_rock_write_write() error: %s\n\r", (char *) err_y);
    u3m_bail(c3__fail); 
  }


}

typedef struct _rock_write_cb_data {
  u3_writ * wit_u;  /* the writ from which this fragment comes */
  c3_y   * buf_y;   /* tmp buffer to be freed after write */
  c3_y   * byt_y;   /* tmp buffer to be freed after write */
  c3_w     len_w;
  writ_test_cb  cbf_u ; /* only for testing */
} rock_write_cb_data;


void * _rock_write_cast(void * opq_u)
{



  /* do the write */
  rock_write_cb_data * cbd_u = (rock_write_cb_data *) opq_u;
  
  _rock_write_write(cbd_u->wit_u, cbd_u->wit_u->evt_d, cbd_u-> buf_y, cbd_u-> byt_y, cbd_u->  len_w, cbd_u-> cbf_u);

  /* set the ack */
  cbd_u->wit_u->ped_o = c3y;

  /* if a meta-callback is set, call it (for testing) */
  if (cbd_u->cbf_u){
    cbd_u->cbf_u(cbd_u);
  }

  /* cleanup */
  free(cbd_u);

#if THREAD
  pthread_exit(NULL);
#endif
  return(NULL); 

}

void
u3_rock_write_write(u3_writ* wit_u, c3_d pos_d, c3_y* buf_y, c3_y* byt_y, c3_w  len_w, writ_test_cb test_cb)
{
  pthread_t tid_u;
  c3_w ret_w;
  
  rock_write_cb_data * cbd_u = (rock_write_cb_data *) malloc(sizeof(rock_write_cb_data));
  bzero(cbd_u, sizeof(rock_write_cb_data));
  cbd_u -> wit_u = wit_u;
  cbd_u -> buf_y = buf_y;
  cbd_u -> byt_y = byt_y;
  cbd_u -> len_w = len_w;
  cbd_u -> cbf_u = test_cb;
  
#if THREAD
  if (0 != (ret_w = pthread_create(& tid_u,
                                   NULL,
                                   _rock_write_cast,  
                                   (void *) cbd_u ))) {
    fprintf(stderr, "u3_rock_write_write() pthread fail: %s\n", mdb_strerror(ret_w));
    u3m_bail(c3__fail); 

  }
#else
  _rock_write_cast((void *) cbd_u );
#endif

}

void
u3_rock_write_shut(u3_pier* pir_u)
{
  /* pick out our 'db input handle', pass that in to common code */
  _rock_shut_comn(pir_u -> pot_u);
}



