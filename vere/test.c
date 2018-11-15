#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <sigsegv.h>
#include <stdint.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <uv.h>
#include <errno.h>
#include <curses.h>
#include <termios.h>
#include <term.h>
#include <math.h>
#include <time.h>
#include <errno.h>


#include "all.h"
#include "vere/vere.h"




FILE * ulog;

//****************************************
//***** speed test
//****************************************

#define TEST_SIZE 1024

struct timespec ts_before[TEST_SIZE];
struct timespec ts_after[TEST_SIZE];
double times_d[TEST_SIZE];

typedef struct _sqlt_write_cb_data {
  u3_writ * wit_u;  /* the writ from which this fragment comes */
  c3_y   * buf_y;   /* tmp buffer to be freed after write */
  c3_w     len_w;
  writ_test_cb  cbf_u ; /* only for testing */
} sqlt_write_cb_data;


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


void _speed_cb(void * data)
{
  sqlt_write_cb_data * sql_data = (sqlt_write_cb_data *) data;
  c3_d evt_d = sql_data -> wit_u->evt_d;
  printf("_speed_cb: evt_d = %ld\n", evt_d);
  
  int ret = clock_gettime(CLOCK_REALTIME, &ts_after[evt_d]);
  if (0 != ret){
    printf("error 2: %s\n", strerror(errno));
  }

}

void _test_write_speed(  u3_pier * pir_u, c3_d evt_d,  c3_c * str_c)
{
  if (evt_d > (TEST_SIZE - 1)){
    printf("ERROR!!!\n");
  }
  u3_writ * wit_u = (u3_writ * ) malloc(sizeof(u3_writ));

  wit_u->pir_u = pir_u;
  wit_u->pes_o = c3n;  /* state variables */
  wit_u->ped_o = c3n;
  wit_u->ces_o = c3n;
  wit_u->ced_o = c3n;
  wit_u->evt_d = evt_d;

  int len = strlen(str_c);
  
  
  int ret = clock_gettime(CLOCK_REALTIME, & ts_before[evt_d]);
  if (0 != ret){
    printf("error 1: %s\n", strerror(errno));
  }

  wric(wit_u, wit_u->evt_d, (c3_y *) str_c,  (c3_y *) str_c + PERS_WRIT_HEAD_SIZE, len, _speed_cb);  
}

int NUM_SAMPLES = 10;

void _test_speed( char * init_str)
{
  u3_pier pir_u;

  
  
  _pier_init_read(& pir_u, init_str);
  _pier_init_writ(& pir_u, init_str);

  c3_d evt_d;
  char * const_str = "___abcdef";
  for (evt_d = 0 ; evt_d < NUM_SAMPLES ; evt_d ++){
    char * test_str = strdup(const_str);
    _test_write_speed(& pir_u, evt_d, test_str);
    free(test_str);
  }

  printf("DONE WRITING %ld\n", evt_d);

  #if 1
  sleep(10);
  #else
  volatile int jj=0;
  while (1 != jj){
    fprintf(stderr, "...sleep...\n\r");
    sleep(1);
  }
  #endif

  printf("DOING MATH %ld\n", evt_d);

  long total_diff_ms = 0;


  for (evt_d = 0 ; evt_d < NUM_SAMPLES ; evt_d ++){
    long diff_ns = (((long) ts_after[evt_d].tv_sec - (long) ts_before[evt_d].tv_sec) * 1000 * 1000 * 1000) +
      (ts_after[evt_d].tv_nsec - ts_before[evt_d].tv_nsec);
    printf("evt_d %ld   delta: %ld ms\n", evt_d, (diff_ns / (1000  * 1000) ));
    total_diff_ms += (diff_ns / (1000 * 1000) );
  }
  
  long mean_diff_ms = (0 == NUM_SAMPLES) ? 0 : total_diff_ms / NUM_SAMPLES;
  
  printf("mean delta: %ld ms\n", mean_diff_ms);
}

void _test_speed_sqlt()
{
  printf("********** SQLite speed test\n");
  _test_speed("s");
}

void _test_speed_lmdb()
{
  printf("********** LMDB speed test\n");
  _test_speed("l");
}

void _test_speed_fond()
{
  printf("********** FoundationDB speed test\n");
  _test_speed("f");
}

void _test_speed_disk()
{
  printf("********** Disk speed test\n");
  _test_speed("d");
}



//****************************************
//***** correctness test
//****************************************

void _test_write(int len,  u3_pier* pir_u)
{
  u3_writ * wit_u = (u3_writ * ) malloc(sizeof(u3_writ));


  wit_u->pir_u = pir_u;

  wit_u->pes_o = c3n;  /* state variables */
  wit_u->ped_o = c3n;
  wit_u->ces_o = c3n;
  wit_u->ced_o = c3n;

  wit_u->evt_d = len;

  c3_c * str_c = (c3_c *) malloc(len + 1 + PERS_WRIT_HEAD_SIZE);
  // memset( (void *) str_c, 'a' + ii - 1, len);
  int ss = 0;
  int tt;
  int index = PERS_WRIT_HEAD_SIZE;
  char c = 'a';
  while(1)  {
    char d = c + ss;
    ss ++;
    for (tt = 0 ; tt < ss ; tt++){
      str_c[index] = d;
      index ++;
      if (index == (len - 1 + PERS_WRIT_HEAD_SIZE)){
        goto write_done;
      }
    }
  }
 write_done:
    
    
  str_c[index] = 0;

  //  u3_fond_write_write(wit_u, wit_u->evt_d, (c3_y *) str_c,  (c3_y *) str_c + PERS_WRIT_HEAD_SIZE, len);
  wric(wit_u, wit_u->evt_d, (c3_y *) str_c,  (c3_y *) str_c + PERS_WRIT_HEAD_SIZE, len, NULL);
}

void _test_read( u3_pier* pir_u, c3_y * expect_y)
{
    c3_y * dat_y;
    c3_w  len_w;
    void * opaq_u;

    rere(pir_u, & dat_y, & len_w, & opaq_u);

    // fprintf(stderr, "out: %s\n\r", dat_y);

    if (0 == strcmp((char*) dat_y, (char*) expect_y)){
      printf("success for %ld\n", pir_u ->pin_u -> pos_d);      
    } else {
      printf("error for %ld\n", pir_u ->pin_u -> pos_d);
      printf("  expected size %ld / actual size %ld\n", strlen( (char *) expect_y), strlen( (char *) dat_y));
      printf("  expected str %s\n", (char *) expect_y);
      printf("  actual str   %s\n", (char *) dat_y);

    }

#if 1
    sleep(1);
#else
  fprintf(stderr, "about to sleep #2 ; before read cleanup\n\r");
  volatile int kk = 0;
  while (0 == kk){
    // for(kk=0; kk < 2 ; kk++){
    fprintf(stderr, "TEST: sleep ...\n\r");
    sleep(1);
  }

  fprintf(stderr, "TEST: sleep done\n\r");
#endif
    
    rede(opaq_u);
}

void _test_core(  u3_pier* pir_u)
{
  
  _test_write(4, pir_u);
  //  _test_write(10, pir_u);
  //  _test_write(100, pir_u);

#if 1
  sleep(30);
#else
  fprintf(stderr, "about to sleep #3 ; before reads\n");
  volatile int jj = 0;
  while (0 == jj){
    // for(jj=0; jj < 2 ; jj++){
    fprintf(stderr, "TEST: sleep ...\n\r");
    sleep(1);
  }

  fprintf(stderr, "TEST: sleep done\n\r");
#endif

  pir_u ->pin_u -> pos_d = 4;
  _test_read(pir_u, (c3_y * ) "abb");
    
  //  pir_u ->pin_u -> pos_d = 10;
  //  _test_read(pir_u, (c3_y * ) "abbcccddd");
  //    
  //  pir_u ->pin_u -> pos_d = 100;
  //  _test_read(pir_u, (c3_y *) "abbcccddddeeeeeffffffggggggghhhhhhhhiiiiiiiiijjjjjjjjjjkkkkkkkkkkkllllllllllllmmmmmmmmmmmmmnnnnnnnn");

}

void _test_fond()
{
  printf("******************** fond\n");
  u3_pier pir_u;
  
  _pier_init_read(& pir_u, "f");
  _pier_init_writ(& pir_u, "f");

  _test_core( & pir_u);
}


void _test_lmdb()
{
  printf("******************** lmdb\n");
  u3_pier pir_u;
  
  _pier_init_read(& pir_u, "l");
  _pier_init_writ(& pir_u, "l");

  _test_core( & pir_u);
}



void _test_sqlt()
{
  printf("******************** sqlt\n");
  u3_pier pir_u;
    
  _pier_init_read(& pir_u, "s");
  _pier_init_writ(& pir_u, "s");

  _test_core(& pir_u);
}

c3_i
main(c3_i   argc,
     c3_c** argv)
{
  
  if (argc > 1){
    NUM_SAMPLES = atoi(argv[1]);
  } 
  printf("num samples = %i\n", NUM_SAMPLES);
    
  
#if 0
  fprintf(stderr, "******************************\n");
  fprintf(stderr, "about to sleep for gdb in main.c - PID = %i\n\r", getpid());
  volatile int ii=0;
  while (1 != ii){
    fprintf(stderr, "...sleep...\n\r");
    sleep(1);
  }
#endif

  u3C.dir_c = "~zod";

  // _test_lmdb();
   _test_speed_lmdb();
  
  // _test_sqlt();
  // _test_speed_sqlt();


    


}
