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


int _sqlt_cb(void* opaq, int coln, char** rows, char** colp)
{
  fprintf(stderr, "\rin callback: %s\n\r", (char *) opaq);
  
  return(0);  /* success */
}

typedef struct {
  sqlite3 * datb_u;
  c3_w      iter_w;
}  hold_t;


void _sqlt_init()
{
  if (SQLITE_OK != SQLITE_CDECL sqlite3_config(SQLITE_CONFIG_SERIALIZED)){
    fprintf(stderr, "sqlt init fail: config serialized\n");    
    return;
  }

  sqlite3 * datb_u = NULL;
  char * db_str = "/tmp/foo.db";
  if (SQLITE_OK != sqlite3_open( db_str, & datb_u)){
    fprintf(stderr, "sqlt init fail: open\n");    
    return;
  }

  c3_y * crat_y = (c3_y *) "create table if not exists urbit (event INT, data TEXT )";
  c3_y * cbar_y = (c3_y *) "callback data - create table";
  c3_y * errm_y = NULL;
  if (SQLITE_OK != sqlite3_exec(datb_u,                                  
                                (char *) crat_y, 
                                & _sqlt_cb,
                                (void *) cbar_y,
                                (char **) & errm_y )) {
    fprintf(stderr, "sqlt init fail: create table\n");    
    return;
  }

}



void * _sqlt_test_one( void * void_u)
{
  hold_t * cuby_u  = ((hold_t *) void_u);
  c3_y * qury_y = (c3_y *) sqlite3_mprintf("INSERT INTO urbit VALUES(%i, 'abcd')", cuby_u -> iter_w);
  c3_y * cbar_y = (c3_y *) "callback data - insert";
  c3_y * errm_y = NULL;


    if (SQLITE_OK != sqlite3_exec(cuby_u -> datb_u,                                  
                                  (char *) qury_y, 
                                  & _sqlt_cb,
                                  (void *) cbar_y,
                                  (char **) & errm_y )) {
      fprintf(stderr, "fail 2\n");
      return(NULL);
    }

    sqlite3_free(qury_y);
    free(void_u);
    
    pthread_exit(0);
    return(NULL);
}

void _sqlt_test()
{
  if (SQLITE_OK != SQLITE_CDECL sqlite3_config(SQLITE_CONFIG_SERIALIZED)){
    fprintf(stderr, "fail 0\n");    
    return;

  }
  
  sqlite3 * datb_u = NULL;
  // char * db_str = "/tmp/foo.db";
  char * db_str = "//media/tjic/E41B-B5E6/foo.db";
  if (SQLITE_OK != sqlite3_open( db_str, & datb_u)){
    fprintf(stderr, "fail 1\n");    
    return;
  }

  struct timespec before;
  struct timespec after;
  
  int ret = clock_gettime(CLOCK_REALTIME, & before );
  if (0 != ret){
    fprintf(stderr, "before\n");
    return;
  }
  
  c3_y * crat_y = (c3_y *) "create table if not exists urbit (event INT, data TEXT )";
  c3_y * cbar_y = (c3_y *) "callback data - create table";
  c3_y * errm_y = NULL;
  if (SQLITE_OK != sqlite3_exec(datb_u,                                  
                                (char *) crat_y, 
                                & _sqlt_cb,
                                (void *) cbar_y,
                                (char **) & errm_y )) {
    fprintf(stderr, "fail 1.5\n");
    return;
  }



  c3_w iter_w = 0;
  c3_w count = 100;
  pthread_t newthread[100];
  for (iter_w = 0; iter_w < count ; iter_w ++ ){
    //    _sqlt_test_one( (void *) iter_w);

    hold_t * cuby_u =  (hold_t *)  malloc(sizeof(hold_t));
    cuby_u->datb_u = datb_u;
    cuby_u->iter_w = iter_w;
    if (0 !=  pthread_create (& newthread[iter_w],
                               NULL,
                              _sqlt_test_one,
                              (void *) cuby_u )){
      fprintf(stderr, "thread create error %i\n", iter_w);
    }

  }

  for (iter_w = 0; iter_w < count ; iter_w ++ ){
    void * ignored;
    pthread_join (newthread[iter_w], & ignored);
  }
  
  ret = clock_gettime(CLOCK_REALTIME, & after );
  if (0 != ret){
    fprintf(stderr, "after\n");
    return;
  }

  time_t diff_sec = after.tv_sec - before.tv_sec;
  long diff_nsec = after.tv_nsec - before.tv_nsec;

  fprintf(stderr, "before sec: %ld\n", before.tv_sec);
  fprintf(stderr, "after  sec: %ld\n", after.tv_sec);
  fprintf(stderr, "before nsec: %li\n", before.tv_nsec);
  fprintf(stderr, "after  nsec: %li\n", after.tv_nsec);
  
  fprintf(stderr, "sec: %ld\n", diff_sec);
  fprintf(stderr, "nsec: %li\n", diff_nsec);
  diff_nsec = diff_nsec + 1000000000 * diff_sec;
  
  fprintf(stderr, "duration: %li\n", diff_nsec);

  double per =  diff_nsec / count / 1000000;
  fprintf(stderr, "duration each: %4.2f ms \n", per );

  

  if (SQLITE_OK != sqlite3_close( datb_u)){
    fprintf(stderr, "fail 3\n");    
    return;
  }

  
  return;
}

