// Note that there is something very weird:
//   if we compile rockdb with DEBUG_LEVEL 0 or 1, 

// g++ -g -o rocktest rock.c  -L../../rocksdb/ -lrocksdb -lpthread -lz -lbz2 -lsnappy

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
#include "../../rocksdb/include/rocksdb/c.h"
#include "vere/vere.h"


const char DBPath[] = "/tmp/rocksdb_simple_example";


#define DATB_NAME  "rocks_db"
#define ROCKS_COMPRESSION_LEVEL rocksdb_no_compression
c3_o
_rock_init_comn(u3_pers * pers_u, c3_c * sto_c)
{
  /* path to db directory */
  char * path_c = malloc(strlen(u3C.dir_c) + strlen(DATB_NAME) + 2);
  sprintf(path_c, "./%s/%s", u3C.dir_c, DATB_NAME);
  fprintf(stderr, "rocks read init: write db path = %s\n\r", path_c);

  /* set db options  */
  rocksdb_options_t * rop_u = pers_u->rock_u->rop_u = rocksdb_options_create();
  long cpus = sysconf(_SC_NPROCESSORS_ONLN); 
  rocksdb_options_set_compression(rop_u, ROCKS_COMPRESSION_LEVEL);
  rocksdb_options_increase_parallelism(rop_u, (int)(cpus));
  rocksdb_options_optimize_level_style_compaction(rop_u, 0);

  /* set write options */
  pers_u->rock_u->wri_u = rocksdb_writeoptions_create();

  /* set read options */
  pers_u->rock_u->red_u= rocksdb_readoptions_create();
  
  /* create or open db */
  rocksdb_options_set_create_if_missing(rop_u, 1);
  char *err = NULL;
  pers_u->rock_u->rok_u = rocksdb_open(rop_u, DBPath, &err);
  if (err){
    fprintf(stderr, "_rock_init_comn() error = %s\n", err);
    return(c3n);
  }
  return(c3y);

}


c3_o
_rock_shut_comn(u3_pers * pers_u)
{
  rocksdb_writeoptions_destroy(pers_u->rock_u->wri_u);
  rocksdb_readoptions_destroy( pers_u->rock_u->red_u);
  rocksdb_options_destroy(     pers_u->rock_u->rop_u);
  rocksdb_close(               pers_u->rock_u->rok_u);

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
  c3_y * err_y;
  sprintf((char *) key_y, "%ld", pir_u -> pin_u ->pos_d);
  *dat_y = (c3_y *) rocksdb_get(pir_u->pin_u->rock_u->rok_u,             // IN: db handle
                                pir_u->pin_u->rock_u->red_u,             // IN: read options
                                (char *) key_y, strlen( (char *) key_y), // IN: key
                                (size_t *) & len_w,                                   // OUT: len
                                (char **) & err_y);                      // OUT: error
  if (err_y){
    return( c3n);    
  } else {
    *hand_u = *dat_y;  // cleanup the allocated read data later
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

  c3_y * err_y;
  rocksdb_put(wit_u->pir_u->pot_u->rock_u->rok_u,  // IN: db handle
              wit_u->pir_u->pot_u->rock_u->wri_u,  // IN: write options
              (char *)key_y, strlen((char *)key_y), // IN: key
              (char *) byt_y, len_w,                  // IN: value
              (char **) &err_y);              // OUT: error

  if (err_y){
    fprintf(stderr, "u3_rock_write_write() error: %s\n\r", err_y);
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

  fprintf(stderr, "****************************** AFTER WRITE - PARENT THREAD\n\r");
  
  /* cleanup */
  free(cbd_u);
  fprintf(stderr, "write thread exiting\n\r");
  pthread_exit(NULL);
  return(NULL); 

}

void
u3_rock_write_write(u3_writ* wit_u, c3_d pos_d, c3_y* buf_y, c3_y* byt_y, c3_w  len_w, writ_test_cb test_cb)
{
  pthread_t tid_u;
  c3_w ret_w;
  
  rock_write_cb_data * cbd_u = (rock_write_cb_data *) malloc(sizeof(rock_write_cb_data));
  cbd_u -> wit_u = wit_u;
  cbd_u -> buf_y = buf_y;
  cbd_u -> byt_y = byt_y;
  cbd_u -> len_w = len_w;
   
  if (0 != (ret_w = pthread_create(& tid_u,
                                   NULL,
                                   _rock_write_cast,  
                                   (void *) cbd_u ))) {
  }

}

void
u3_rock_write_shut(u3_pier* pir_u)
{
  /* pick out our 'db input handle', pass that in to common code */
  _rock_shut_comn(pir_u -> pot_u);
}



#if 0
#define NUM_SAMPLES 100

struct timespec ts_before[NUM_SAMPLES];
struct timespec ts_after[NUM_SAMPLES];
int main(){
  rocksdb_t *db;
  rocksdb_backup_engine_t *be;
  rocksdb_options_t *options = rocksdb_options_create();
  // Optimize RocksDB. This is the easiest way to
  // get RocksDB to perform well
  long cpus = sysconf(_SC_NPROCESSORS_ONLN);  // get # of online cores

  rocksdb_options_set_compression(options, rocksdb_zlib_compression);

  rocksdb_options_increase_parallelism(options, (int)(cpus));
  rocksdb_options_optimize_level_style_compaction(options, 0);
  // create the DB if it's not already present
  rocksdb_options_set_create_if_missing(options, 1);

  // open DB
  char *err = NULL;
  db = rocksdb_open(options, DBPath, &err);
  if (err){
    printf("err = %s\n", err);
    exit(-1);
  }


  // open Backup Engine that we will use for backing up our database
  be = rocksdb_backup_engine_open(options, DBBackupPath, &err);
  assert(!err);


  
  // Put key-value
  rocksdb_writeoptions_t *writeoptions = rocksdb_writeoptions_create();
  rocksdb_writeoptions_set_sync(writeoptions, 1); // 0 = BS; 1 = full flush 

  int ii;
  for (ii = 0 ; ii < NUM_SAMPLES; ii++){
    int ret = clock_gettime(CLOCK_REALTIME, &ts_before[ii]);
    if (0 != ret){
      printf("error 2: %s\n", strerror(errno));
    }
  
    char key[10];
    sprintf(key, "%i", ii);
    const char *value = "value";
    rocksdb_put(db, writeoptions, key, strlen(key), value, strlen(value) + 1,
                &err);
    assert(!err);

    ret = clock_gettime(CLOCK_REALTIME, &ts_after[ii]);
    if (0 != ret){
      printf("error 2: %s\n", strerror(errno));
    }
    // Get value
    rocksdb_readoptions_t *readoptions = rocksdb_readoptions_create();
    size_t len;
    char *returned_value = rocksdb_get(db, readoptions, key, strlen(key), &len, &err);
    assert(!err);
    assert(strcmp(returned_value, "value") == 0);
    free(returned_value);

  }
  

  // create new backup in a directory specified by DBBackupPath
  rocksdb_backup_engine_create_new_backup(be, db, &err);
  assert(!err);

  rocksdb_close(db);

  // If something is wrong, you might want to restore data from last backup
  rocksdb_restore_options_t *restore_options = rocksdb_restore_options_create();
  rocksdb_backup_engine_restore_db_from_latest_backup(be, DBPath, DBPath,
                                                      restore_options, &err);
  assert(!err);
  rocksdb_restore_options_destroy(restore_options);

  db = rocksdb_open(options, DBPath, &err);
  assert(!err);

  // cleanup
  rocksdb_writeoptions_destroy(writeoptions);
  //  rocksdb_readoptions_destroy(readoptions);
  rocksdb_options_destroy(options);
  rocksdb_backup_engine_close(be);
  rocksdb_close(db);

  printf("DOING MATH\n");

  long total_diff_ms = 0;


  for (ii = 0 ; ii < NUM_SAMPLES; ii++){

    long diff_ns = (((long) ts_after[ii].tv_sec - (long) ts_before[ii].tv_sec) * 1000 * 1000 * 1000) +
      (ts_after[ii].tv_nsec - ts_before[ii].tv_nsec);
    printf("  delta: %ld ms\n", (diff_ns / (1000  * 1000) ));
    total_diff_ms += (diff_ns / (1000 * 1000) );
  }

  long mean_diff_ms = (0 == NUM_SAMPLES) ? 0 : total_diff_ms / NUM_SAMPLES;

  printf("mean delta: %ld ms\n", mean_diff_ms);

  return 0;

}
#endif
