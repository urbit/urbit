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
#include <time.h>
#include <pthread.h>
#include <sqlite3.h>

#include "vere/vere.h"

#define DATB_NAME  "data.db"
#define TABL_NAME  "evnt"




/* read */

c3_o
u3_sqlt_read_init(u3_pier* pir_u, c3_c * pot_c)
{
  u3_pers *  pin_u = pir_u -> pin_u;

  pin_u->sql_u = c3_malloc(sizeof (u3_sqlt));
  memset( pin_u->sql_u, 0, sizeof (u3_sqlt));

  /* set read head to 1 */
  pir_u->pin_u->pos_d = 1;

  char * path_c = malloc(strlen(u3C.dir_c) + strlen(DATB_NAME) + 2);
  sprintf(path_c, "%s/%s", u3C.dir_c, DATB_NAME);
  fprintf(stderr, "\rdb path = %s\n\r", path_c);


  uint32_t       ret_w;
  if (SQLITE_OK != (ret_w = sqlite3_open( path_c, &  pin_u->sql_u -> sql_u ))){
    fprintf(stderr, "sqlt read init fail 1: %s\n", uv_strerror(ret_w));
    u3m_bail(c3__fail); 
    return(c3n);
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
    fprintf(stderr, "read fail 1: %s\n", uv_strerror(ret_w));    
    u3m_bail(c3__fail); 
    return(c3n);

  }

  ret_w = sqlite3_step(stat_u);
  if ((SQLITE_ROW != ret_w) &&
      (SQLITE_DONE != ret_w))
    {
    fprintf(stderr, "read fail 2: %i / %s\n", ret_w, uv_strerror(ret_w));
    u3m_bail(c3__fail); 
    return(c3n);
  }

  /* retrieve bytes and len */
  c3_y* byt_y = (c3_y *) sqlite3_column_blob(stat_u, 1);
  c3_w lan_w  = (c3_w) sqlite3_column_bytes(stat_u, 1);
  if( 0 == lan_w){
    return(c3n);
  }

#if 0
  {
    fprintf(stderr, "read_atom %i bytes\n\r", lan_w);
    int ii;
    for (ii =0; ii < lan_w; ii++){
      fprintf(stderr, "%2x ", byt_y[ii]);
    }

  }
#endif


  
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
u3_sqlt_read_done(void * hand_u)
{
  uint32_t       ret_w;

  if (SQLITE_OK !=   (ret_w = sqlite3_finalize(hand_u))){
    fprintf(stderr, "fail 5: %s\n", uv_strerror(ret_w));
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
    pot_u->sql_u = pir_u->pin_u->sql_u;
  } else {

    uint32_t       ret_w;
    if (SQLITE_OK != (ret_w = sqlite3_config(SQLITE_CONFIG_SERIALIZED))){
      fprintf(stderr, "sqlt write init fail 1: %s\n", uv_strerror(ret_w));    
      return c3n;
    }

  
    char * path_c = malloc(strlen(u3C.dir_c) + strlen(DATB_NAME) + 2);
    sprintf(path_c, "./%s/%s", u3C.dir_c, DATB_NAME);
    fprintf(stderr, "\rdb path = %s\n\r", path_c);

  
    if (SQLITE_OK != (ret_w = sqlite3_open( path_c, &  pot_u->sql_u -> sql_u ))){
      fprintf(stderr, "sqlt write init fail 2: %s\n", uv_strerror(ret_w));
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
    fprintf(stderr, "sqlt init fail: create table: %s\n", uv_strerror(ret_w));    
    u3m_bail(c3__fail);
  }

  sqlite3_free(qury_y);


  return c3y;
      
}

void
_sqlt_write_fragment(u3_writ* wit_u, c3_y * buf_y,  c3_w len_w)
{

  /* fprintf(stderr, "_sqlt_write_fragment %ld // %s // %i\n\r", wit_u -> evt_d, buf_y, len_w); */
  
  c3_y * stat_y = (c3_y *) sqlite3_mprintf("INSERT INTO %s VALUES(?, ?)", TABL_NAME);

  sqlite3 *      daba_u = wit_u->pir_u -> pot_u -> sql_u -> sql_u;
  sqlite3_stmt * stat_u = NULL;
  uint32_t       ret_w;

  if (SQLITE_OK != (ret_w = sqlite3_prepare_v2( daba_u,          /* Database handle */
                                                (char *) stat_y, /* SQL statement, UTF-8 encoded */
                                                -1,               /* Maximum length of zSql in bytes. */
                                                & stat_u,        /* OUT: Statement handle */
                                                NULL             /* OUT: Pointer to unused portion of zSql */
                                                ))){
    fprintf(stderr, "write fail 1: %s\n", uv_strerror(ret_w));    
    goto write_error;
  }



  /* write the event number */
  if (SQLITE_OK != (ret_w = sqlite3_bind_int( stat_u,
                                              1 ,                 /* param index */
                                              wit_u -> evt_d))){
    fprintf(stderr, "write fail 2: %s\n", uv_strerror(ret_w));
    goto write_error;
  }

  /* write the event blob */

  if (SQLITE_OK != (ret_w = sqlite3_bind_blob(stat_u,
                                              2,       /* param index */ 
                                              buf_y,   /* atom data */
                                              (c3_d) len_w,   /* atom data size, in bytes */
                                              NULL))){ /* callback to clean up event */
    fprintf(stderr, "write fail 3: %s\n", uv_strerror(ret_w));
    goto write_error;
  }

  if (SQLITE_DONE != (ret_w = sqlite3_step(stat_u))){
    fprintf(stderr, "write fail 4: %s\n", uv_strerror(ret_w));
    goto write_error;
  }

  if (SQLITE_OK !=   (ret_w = sqlite3_finalize(stat_u))){
    fprintf(stderr, "write fail 5: %s\n", uv_strerror(ret_w));
    goto write_error;
  }

  return;

 write_error:
  u3m_bail(c3__fail); 
  return;  
}

typedef struct _sqlt_write_cb_data {
  u3_writ * wit_u;  /* the writ from which this fragment comes */
  c3_y   * buf_y;   /* tmp buffer to be freed after write */
  c3_w     len_w;
} sqlt_write_cb_data;



void *
_sqlt_write_fragment_cast(void *opq_u)
{
  sqlt_write_cb_data* cbd_u = (sqlt_write_cb_data*) opq_u;
  _sqlt_write_fragment(cbd_u->wit_u, cbd_u->buf_y, cbd_u->len_w);

  /* set the ack */
  cbd_u->wit_u->ped_o = c3y;

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
                    c3_w  len_w           /* IN: data len */
                     )
{
  pthread_t tid_u;
  uint32_t       ret_w;

  sqlt_write_cb_data * cbd_u = (sqlt_write_cb_data *) malloc(sizeof(sqlt_write_cb_data));
  cbd_u->wit_u = wit_u;
  cbd_u->buf_y = buf_y + PERS_WRIT_HEAD_SIZE;
  cbd_u->len_w = len_w;
  
  if (0 != (ret_w = pthread_create(& tid_u,
                                   NULL,
                                   _sqlt_write_fragment_cast,  
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




