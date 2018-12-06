/* vere/sqlt.c
**
**  This file is in the public domain.
**
**  21 Nov 2018 on Ubuntu 16.04: max noun size = 2^20 = 1,048,576 bytes.  2^21 = segv
**  No need for fragmenting.
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
#include <time.h>
#include <math.h>
#include <pthread.h>
#include <sqlite3.h>

#include "vere/vere.h"


#define DATB_NAME  "sqlite.db"
#define TABL_NAME  "evnt"


c3_w u3_sqlt_frag_size()
{
  return( pow(2, 31) );
}


/* read */

c3_o
u3_sqlt_read_init(u3_pier* pir_u, c3_c * pot_c)
{

  c3_w thr_w =  sqlite3_threadsafe();
  if (0 == thr_w){
    fprintf(stderr, "sqlt read init: library is not compiled with thread-safe flags; aborting\n\r");
    u3m_bail(c3__fail); 
    return(c3n);    
  }
  
  u3_pers *  pin_u = pir_u -> pin_u;

  pin_u->sql_u = c3_malloc(sizeof (u3_sqlt));
  memset( pin_u->sql_u, 0, sizeof (u3_sqlt));

  /* set read head to 1 */
  pir_u->pin_u->pos_d = 1;

  char * path_c = malloc(strlen(u3C.dir_c) + strlen(DATB_NAME) + 2);
  sprintf(path_c, "./%s/%s", u3C.dir_c, DATB_NAME);
  fprintf(stderr, "sqlt read init: read db path = %s\n\r", path_c);

  /* open the DB */
  
  uint32_t       ret_w;
  if (SQLITE_OK != (ret_w = sqlite3_open( path_c, &  pin_u->sql_u -> sql_u ))){
    fprintf(stderr, "sqlt read init fail 1: %s\n", sqlite3_errstr(ret_w));
    u3m_bail(c3__fail); 
    return(c3n);
  }


  /* configure the DB */

  const c3_y pragmas[][100] = {
    "PRAGMA synchronous  = FULL",
    "PRAGMA journal_mode = WALL",
  };

  c3_w ii = 0;
  
  size_t len_u = sizeof(pragmas) / sizeof(pragmas[0]);
  for (ii = 0; ii < len_u ; ii ++ ){
    printf("pragma == %s\n", pragmas[ii]);
    c3_y * err_y;
    if (SQLITE_OK != (ret_w = sqlite3_exec(pin_u->sql_u -> sql_u, (const char * ) pragmas[ii], NULL, NULL, (char **) & err_y))){
      fprintf(stderr, "sqlt read init configure 1 fail: %s\n", err_y);
      u3m_bail(c3__fail); 
      return(c3n);
    }


  }
      
    
  return(c3y);
}

/* read specified row from SQL
 * 
 *   returns: row found? y/n
 */

c3_o
_sqlt_read_fragment(sqlite3 * sql_u,  /* IN: SQLite3 handle */
                c3_d pos_d,       /* IN: row id */
                c3_y ** dat_y,    /* OUT: set pointer to data */
                c3_w * len_w,     /* OUT: set len of data */
                void ** hand_u)   /* OUT: an opaque handle to be passed in later to a cleanup func */
{
  
  uint32_t       ret_w;
  sqlite3_stmt * stat_u = NULL;
  c3_y *         stat_y = (c3_y *) sqlite3_mprintf("select * from %s where event = %i", TABL_NAME, pos_d);

  if (SQLITE_OK != (ret_w = sqlite3_prepare_v2( sql_u,          /* Database handle */
                                                (char *) stat_y, /* SQL statement, UTF-8 encoded */
                                                -1,               /* Maximum length of zSql in bytes. */
                                                & stat_u,        /* OUT: Statement handle */
                                                NULL             /* OUT: Pointer to unused portion of zSql */
                                                ))){
    fprintf(stderr, "read fail 1: %s\n", sqlite3_errstr(ret_w));    
    u3m_bail(c3__fail); 
    return(c3n);

  }

  
  ret_w = sqlite3_step(stat_u);
  if (SQLITE_DONE == ret_w){
    fprintf(stderr, "_sqlt_read_fragment: no row # %ld\n\r", pos_d);
    return(c3n);
  } else if (SQLITE_ROW != ret_w) {
    fprintf(stderr, "read fail 2: %i / %s\n", ret_w, sqlite3_errstr(ret_w));
    u3m_bail(c3__fail); 
    return(c3n);
  }

  sqlite3_free(stat_y);
  
  /* retrieve bytes and len */
  c3_y* byt_y = (c3_y *) sqlite3_column_blob(stat_u, 1);
  c3_w lan_w  = (c3_w) sqlite3_column_bytes(stat_u, 1);
  if( 0 == lan_w){
    return(c3n);
  }
  
  /* return */
  *dat_y = byt_y;
  *len_w = lan_w;
  *hand_u = stat_u;
  
  return(c3y);
}

c3_o 
u3_sqlt_read_read(u3_pier* pir_u, c3_y ** dat_y, c3_w *    len_w, void ** hand_u)
{
  c3_o ret_o = _sqlt_read_fragment(pir_u->pin_u->sql_u->sql_u,
                               pir_u->pin_u->pos_d,
                               dat_y,
                               len_w,
                               hand_u);
  if (c3y == ret_o){
    pir_u->pin_u->pos_d++;
  }
  
  return( ret_o);
}

void
u3_sqlt_read_done(void * opaq_u)
{
  uint32_t       ret_w;

  sqlite3_stmt * stam_u = (sqlite3_stmt *) opaq_u;
  
  if (SQLITE_OK !=   (ret_w = sqlite3_finalize(stam_u))){
    fprintf(stderr, "fail 5: %s\n", sqlite3_errstr(ret_w));
    u3m_bail(c3__fail); 
    return;
  }

}

void
u3_sqlt_read_shut(u3_pier * pir_u)
{
}

/* write */


c3_o
u3_sqlt_write_init(u3_pier* pir_u, c3_c * pot_c)
{
  u3_pers *  pot_u = pir_u -> pot_u;
  
  /* share single SQL handle, if for both in and out */
  if (pir_u->pin_u->sql_u){
    fprintf(stderr, "sqlt write init: sharing db handle with sqlt read\n\r");
    pot_u->sql_u = pir_u->pin_u->sql_u;
  } else {

    uint32_t       ret_w;
    if (SQLITE_OK != (ret_w = sqlite3_config(SQLITE_CONFIG_MULTITHREAD))){ // SQLITE_CONFIG_SERIALIZED
      fprintf(stderr, "sqlt write init fail 1: %s\n", sqlite3_errstr(ret_w));    
      return c3n;
    }

  
    char * path_c = malloc(strlen(u3C.dir_c) + strlen(DATB_NAME) + 2);
    sprintf(path_c, "./%s/%s", u3C.dir_c, DATB_NAME);
    fprintf(stderr, "sqlt write init: write db path = %s\n\r", path_c);

  
    if (SQLITE_OK != (ret_w = sqlite3_open( path_c, &  pot_u->sql_u -> sql_u ))){
      fprintf(stderr, "sqlt write init fail 2: %s\n", sqlite3_errstr(ret_w));
      u3m_bail(c3__fail); 
      return c3n;

    }
  }
  
  c3_y * qury_y = (c3_y *) sqlite3_mprintf("create table if not exists %s (event INT, data BLOB )", TABL_NAME);
  c3_y * cbar_y = (c3_y *) "callback data - create table";
  c3_y * errm_y = NULL;
  uint32_t       ret_w;
  if (SQLITE_OK != (ret_w = sqlite3_exec(pot_u->sql_u->sql_u,         /* db handle */                           
                                (char *) qury_y,        /* query */
                                NULL,             /* callback */
                                (void *) cbar_y,        /* 1st arg to callback */
                                         (char **) & errm_y ))) { /* return arg: error msg */
    fprintf(stderr, "sqlt init fail: create table: %s\n", sqlite3_errstr(ret_w));    
    u3m_bail(c3__fail);
  }

  sqlite3_free(qury_y);


  return c3y;
      
}

void
_sqlt_write_fragment(u3_writ* wit_u, c3_y * buf_y,  c3_w len_w)
{

  c3_y * stat_y = (c3_y *) sqlite3_mprintf("INSERT INTO %s VALUES(?, ?)", TABL_NAME);

  sqlite3 *      daba_u = wit_u->pir_u -> pot_u -> sql_u -> sql_u;
  sqlite3_stmt * stat_u = NULL;
  uint32_t       ret_w;

  /* fprintf(stderr, "     ***** BEFORE WRITE\n\r"); */

  
  if (SQLITE_OK != (ret_w = sqlite3_prepare_v2( daba_u,          /* Database handle */
                                                (char *) stat_y, /* SQL statement, UTF-8 encoded */
                                                -1,               /* Maximum length of zSql in bytes. */
                                                & stat_u,        /* OUT: Statement handle */
                                                NULL             /* OUT: Pointer to unused portion of zSql */
                                                ))){
    fprintf(stderr, "write fail 1: %s\n", sqlite3_errstr(ret_w));    
    goto write_error;
  }



  /* write the event number */
  if (SQLITE_OK != (ret_w = sqlite3_bind_int( stat_u,
                                              1 ,                 /* param index */
                                              wit_u -> evt_d))){
    fprintf(stderr, "write fail 2: %s\n", sqlite3_errstr(ret_w));
    goto write_error;
  }

  /* write the event blob */

  if (SQLITE_OK != (ret_w = sqlite3_bind_blob(stat_u,
                                              2,       /* param index */ 
                                              buf_y,   /* atom data */
                                              (c3_d) len_w,   /* atom data size, in bytes */
                                              NULL))){ /* callback to clean up event */
    fprintf(stderr, "write fail 3: %s\n", sqlite3_errstr(ret_w));
    goto write_error;
  }

  if (SQLITE_DONE != (ret_w = sqlite3_step(stat_u))){
    fprintf(stderr, "write fail 4: %s\n", sqlite3_errstr(ret_w));
    goto write_error;
  }

  sqlite3_free(stat_y);
  
  if (SQLITE_OK !=   (ret_w = sqlite3_finalize(stat_u))){
    fprintf(stderr, "write fail 5: %s\n", sqlite3_errstr(ret_w));
    goto write_error;
  }

  /* fprintf(stderr, "     ***** AFTER WRITE - CHILD THREAD\n\r"); */

    
  return;

 write_error:
  u3m_bail(c3__fail); 
  return;  
}



typedef struct _sqlt_write_cb_data {
  u3_writ * wit_u;  /* the writ from which this fragment comes */
  c3_y   * buf_y;   /* tmp buffer to be freed after write */
  c3_w     len_w;
  writ_test_cb  cbf_u ; /* only for testing */
} sqlt_write_cb_data;



void *
_sqlt_write_cast(void *opq_u)
{
  sqlt_write_cb_data* cbd_u = (sqlt_write_cb_data*) opq_u;
  _sqlt_write_fragment(cbd_u->wit_u, cbd_u->buf_y, cbd_u->len_w);

  /* set the ack */
  cbd_u->wit_u->ped_o = c3y;

  /* if a meta-callback is set, call it (for testing) */
  if (cbd_u->cbf_u){
    
    cbd_u->cbf_u(cbd_u);
  } else {
    
  }

  /* cleanup */
  free(cbd_u);
  
  /* fprintf(stderr, "write thread exiting\n\r"); */
  pthread_exit(NULL);
  return(NULL); 
}

void
u3_sqlt_write_write(u3_writ* wit_u,       /* IN: writ */
                    c3_d pos_d,           /* IN: row id */
                    c3_y* buf_y,          /* IN: buffer (to be freed later) */
                    c3_y* byt_y,          /* IN: data (located inside buffer above, but don't worry about that) */
                    c3_w  len_w,          /* IN: data len */
                    writ_test_cb test_cb          /* IN: void * (callback function) for testing - set NULL */
                     )
{
  pthread_t tid_u;
  uint32_t       ret_w;

  c3_w hed_w = u3_frag_head_size(len_w, 
                                 1, 
                                 u3_sqlt_frag_size());
  hed_w= 0;
  
  sqlt_write_cb_data * cbd_u = (sqlt_write_cb_data *) malloc(sizeof(sqlt_write_cb_data));
  cbd_u->wit_u = wit_u;
  cbd_u->buf_y = buf_y + hed_w;
  cbd_u->len_w = len_w - hed_w;
  cbd_u->cbf_u = test_cb;
  
  if (0 != (ret_w = pthread_create(& tid_u,
                                   NULL,
                                   _sqlt_write_cast,  
                                   (void *) cbd_u ))) {
    fprintf(stderr, "u3_sqlt_write_commit() : %s \n\r", strerror(ret_w));
    u3m_bail(c3__fail); 
    return;
  }

  return;
}

void
u3_sqlt_write_shut(u3_pier* pir_u)
{
  if (SQLITE_OK != sqlite3_close( pir_u->pot_u->sql_u->sql_u)){
    fprintf(stderr, "sqlt_write_shut fail\n");    
  }
  return;

}




