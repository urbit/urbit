/* vere/test.c
**
**  This file is in the public domain.
**
**  BUGS:
**     - need facility to wipe out database contents from previous runs before new run (bugs!)
*/

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

FILE * ulog;  /* not used in this file, but needed for linking */


//****************************************
//***** speed test
//****************************************

#define TEST_SIZE 1024
#define WIDTH_DISPLAY_LEN 30

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

  c3_w len_w = strlen(str_c);
  c3_w hed_w = u3_frag_head_size(len_w, 
                                 1, 
                                 u3_lmdb_frag_size());


  u3_writ * wit_u = (u3_writ * ) malloc(sizeof(u3_writ));

  wit_u->pir_u = pir_u;
  wit_u->pes_o = c3n;  /* state variables */
  wit_u->ped_o = c3n;
  wit_u->ces_o = c3n;
  wit_u->ced_o = c3n;
  wit_u->evt_d = evt_d;


  
  
  int ret = clock_gettime(CLOCK_REALTIME, & ts_before[evt_d]);
  if (0 != ret){
    printf("error 1: %s\n", strerror(errno));
  }

  wric(wit_u, wit_u->evt_d, (c3_y *) str_c,  (c3_y *) str_c + hed_w, len_w, _speed_cb);  
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

  
#if 0
  volatile int jj=0;
  while (1 != jj){
    fprintf(stderr, "...sleep...\n\r");
    sleep(1);
  }

#else
  int sleep_i = 10;
  fprintf(stderr, "about to sleep for %i seconds before (speed) reads\n", sleep_i);
  sleep(sleep_i);

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

  int loop;

  for(loop = 0 ; loop < NUM_SAMPLES ; loop ++){
    c3_y * dat_y;
    c3_w  len_w;
    void * opaq_u;

    pir_u.pin_u->pos_d = loop;
    c3_o ret_o = rere(& pir_u, & dat_y, & len_w, & opaq_u);
    if (c3n == ret_o){
      printf("read failure for %ld - no read found\n", pir_u.pin_u -> pos_d); 
    } else {
      fprintf(stderr, "read %i: %s\n", loop, dat_y);
    }
  }

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

void _test_write(int len_w,  u3_pier* pir_u)
{
  u3_writ * wit_u = (u3_writ * ) malloc(sizeof(u3_writ));


  wit_u->pir_u = pir_u;

  wit_u->pes_o = c3n;  /* state variables */
  wit_u->ped_o = c3n;
  wit_u->ces_o = c3n;
  wit_u->ced_o = c3n;

  wit_u->evt_d = len_w;

  c3_w hed_w = u3_frag_head_size(len_w, 
                                 1, 
                                u3_lmdb_frag_size());


  
  c3_c * str_c = (c3_c *) malloc(len_w + 1 + hed_w);
  int ss = 0;
  int tt;
  int index = hed_w;
  char c = 'a';
  while(1)  {
    char d = c + ss;
    ss ++;
    for (tt = 0 ; tt < ss ; tt++){
      str_c[index] = d;
      index ++;
      if (index == (len_w - 1 + hed_w)){
        goto write_done;
      }
    }
  }
 write_done:
    
    
  str_c[index] = 0;

  wric(wit_u, wit_u->evt_d, (c3_y *) str_c,  (c3_y *) str_c + hed_w, len_w, NULL);
}

void _test_read( u3_pier* pir_u, c3_y * expect_y)
{
    c3_y * dat_y;
    c3_w  len_w;
    void * opaq_u;

    c3_o ret_o = rere(pir_u, & dat_y, & len_w, & opaq_u);

    // fprintf(stderr, "out: %s\n\r", dat_y);
    if (c3n == ret_o){
      printf("read failure for %ld - no read found\n", pir_u ->pin_u -> pos_d);          
    } else if (0 == strcmp((char*) dat_y, (char*) expect_y)){
      printf("success for %ld\n", pir_u ->pin_u -> pos_d);      
    } else {
      printf("error for %ld\n", pir_u ->pin_u -> pos_d);
      printf("  expected size %ld / actual size %ld\n", strlen( (char *) expect_y), strlen( (char *) dat_y));
      printf("  expected str %s\n", (char *) expect_y);
      printf("  actual str   %s\n", (char *) dat_y);

    }

    if (c3y == ret_o) {
      rede(opaq_u);
    }
}

void _test_core(  u3_pier* pir_u)
{
  
  _test_write(4, pir_u);
  _test_write(10, pir_u);
  _test_write(100, pir_u);

#if 1
  int sleep_i = 10;
  fprintf(stderr, "about to sleep for %i seconds before reads\n", sleep_i);
  sleep(sleep_i);
#else
  fprintf(stderr, "about to sleep for GDB breakpoint before reads\n");
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
    
  pir_u ->pin_u -> pos_d = 10;
  _test_read(pir_u, (c3_y * ) "abbcccddd");
     
  pir_u ->pin_u -> pos_d = 100;
  _test_read(pir_u, (c3_y *) "abbcccddddeeeeeffffffggggggghhhhhhhhiiiiiiiiijjjjjjjjjjkkkkkkkkkkkllllllllllllmmmmmmmmmmmmmnnnnnnnn");

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

//****************************************
//***** width test
//****************************************

void _test_width(u3_pier* pir_u)
{
  c3_w exp_w;
  // malloc() fails on 2^24
  for (exp_w = 9; exp_w < 24 ; exp_w ++){

    c3_w wid_w = (c3_w) pow((double) 2, (double) exp_w);
        
    // calc header size
    c3_w max_w = wrze(); 
    c3_w hed_w = u3_frag_head_size(wid_w, 1, max_w);
    
    // write
    fprintf(stderr, "--------------------\n");
    fprintf(stderr, "2^%i = width %i\n", exp_w, wid_w);
    c3_y * buf_y = (c3_y *) malloc(wid_w + hed_w);
    c3_y * byt_y = buf_y + hed_w;
    
    /* pick fill character */
    c3_y chr_y = 'a' + exp_w - 1;
    
    memset(buf_y, (int) 0, (size_t) hed_w);         // zero out header
    memset(byt_y, (int) chr_y, (size_t) wid_w); // fill body with character
    byt_y[ (wid_w > WIDTH_DISPLAY_LEN ? WIDTH_DISPLAY_LEN : wid_w) - 1 ] = 0; // terminator at pos 10 

    int ii;
    for (ii = 0; ii < ( (wid_w > WIDTH_DISPLAY_LEN ? WIDTH_DISPLAY_LEN : wid_w) - 1 )  ; ii++){
      byt_y[ii] = 'a' + ii;
    }
    
    u3_writ wit_u;
    wit_u.pir_u = pir_u;
    wit_u.pes_o = c3n; 
    wit_u.ped_o = c3n;
    wit_u.ces_o = c3n;
    wit_u.ced_o = c3n;
    wit_u.evt_d = exp_w;

    wric(& wit_u, wit_u.evt_d, buf_y, byt_y, wid_w, NULL);  

    
    // wait for write to complete

    c3_w cnt_w = 0;
    while (c3n == wit_u.ped_o ){
      sleep(1);
      cnt_w ++;
      if (cnt_w >= 100000){       // NOTFORCHECKIN
        fprintf(stderr, "    timeout\n");
        return;
      }
    }
    //    free(str_y); // free only after write has succeeded - this line is removed bc if present, get segv later

    // read
    
    fprintf(stderr, "about to read; wit.ped_o == %s\n", ((c3n == wit_u.ped_o) ?  "n" : "y")  ); 
    
    c3_y * dat_y;
    c3_w len_w;
    void * opaq_u;

    pir_u->pin_u->pos_d = exp_w;

    
    c3_o  ret_o = rere(pir_u, & dat_y, & len_w, & opaq_u) ;
    fprintf(stderr, "read success? %s\n", c3y == ret_o ? "yes" : "no");
    if ( c3n == ret_o ){
      fprintf(stderr, "  ** FAIL\n");
      exit(-1);
    }

    if (len_w != wid_w){
      fprintf(stderr, "  ** FAIL: wrote %i bytes, read %i bytes \n", wid_w, len_w);
      exit(-1);
    }

    // byt_y[ (wid_w > 10 ? 10 : wid_w) - 1 ] = 0; // terminator at pos 10
    //  fprintf(stderr, "read byte 0 = %c\n", (char) dat_y[0]);
    //  fprintf(stderr, "read byte 1 = %c\n", (char) dat_y[1]);
    //  fprintf(stderr, "read first 10 bytes = %s\n", dat_y);
    // if (dat_y[len_w - 2] != chr_y){
    //  fprintf(stderr, "  ** FAIL: byte %i expected to be '%c' but was '%c'\n", len_w - 2, chr_y, dat_y[len_w - 2]);
      // exit(-1);
    // }

    if (0 != strcmp( (char *) dat_y, (char *) byt_y)){
      fprintf(stderr, "  ** FAIL: wrote %s, read %s \n", dat_y, byt_y);
      exit(-1);

    }
    
    rede(opaq_u);
    
  }
}

void _width_sqlt()
{
  printf("******************** sqlt width\n");
  u3_pier pir_u;
  _pier_init_read(& pir_u, "s");
  _pier_init_writ(& pir_u, "s");

  _test_width(& pir_u);
}

void _width_rock()
{
  printf("******************** rock width\n");
  u3_pier pir_u;
  _pier_init_read(& pir_u, "r");
  _pier_init_writ(& pir_u, "r");

  _test_width(& pir_u);
}

void _width_lmdb()
{
  printf("******************** lmdb width\n");
  u3_pier pir_u;
  _pier_init_read(& pir_u, "r");
  _pier_init_writ(& pir_u, "r");

  _test_width(& pir_u);
}

void _width_fond()
{
  printf("******************** fond width\n");
  u3_pier pir_u;
  _pier_init_read(& pir_u, "f");
  _pier_init_writ(& pir_u, "f");

  _test_width(& pir_u);
}

void _head_dump(c3_y * buf_y)
{
  int ii;
  for (ii = 0; ii < 32; ii ++){
    fprintf(stderr, "%x ", buf_y[ii]);
    if (0 == (ii + 1) % 4){
      fprintf(stderr, "\n\r");
    }
  }

}

void _head_test()
{
  c3_y buf_y[1024];
  bzero((void *) buf_y, 1024);

  /* single byte sizes */
  if (1) {
    c3_w ret_w = _frag_head_writ(buf_y, (c3_w) 1, (c3_w) 99);
    fprintf(stderr, "ret_w = %i\n\r", ret_w);
    // _head_dump(buf_y);

    c3_w dex_w;
    c3_w tot_w;
    ret_w =  _frag_head_read(buf_y, & dex_w, & tot_w);
    fprintf(stderr, "ret_w = %i\n\r", ret_w);
    fprintf(stderr, "dex_w = %i\n\r", dex_w);
    fprintf(stderr, "tot_w = %i\n\r", tot_w);

    assert(ret_w = 4);
    assert(dex_w = 1);
    assert(tot_w = 99);
  }

  /* 2-4 byte  sizes */
  {
    c3_w ret_w = _frag_head_writ(buf_y, (c3_w) 0x123, (c3_w) 0x1234567);
    fprintf(stderr, "ret_w = %i\n\r", ret_w);
    // _head_dump(buf_y);

    c3_w dex_w;
    c3_w tot_w;
    ret_w =  _frag_head_read(buf_y, & dex_w, & tot_w);
    fprintf(stderr, "ret_w = %i\n\r", ret_w);
    fprintf(stderr, "dex_w = 0x%x\n\r", dex_w);
    fprintf(stderr, "tot_w = 0x%x\n\r", tot_w);

    assert(ret_w = 12);
    assert(dex_w = 0x123);
    assert(tot_w = 0x1234567);

  }

  
}

//****************************************
//***** main
//****************************************

c3_i
main(c3_i   argc,
     c3_c** argv)
{

  int attach = 0 ;

  printf("argc %i, argv[1] %s\n", argc, argv[1]);
  if (argc == 2 && (0 == strcmp(argv[1], "attach"))){
    attach = 1;
    fprintf(stderr, "attach\n");
  } else {
    fprintf(stderr, "NO attach\n");
  }
   
  
  if (1 == attach){
    fprintf(stderr, "******************************\n");
    fprintf(stderr, "about to sleep for gdb in main.c - PID = %i\n\r", getpid());
    volatile int ii=0;
    while (1 != ii){
      fprintf(stderr, "...sleep...\n\r");
      sleep(1);
    }
  } else {

    sleep(1);
  }

  u3C.dir_c = "~zod";

  /* test header ==================== */

  // _head_test();
  
  /* width ==================== */
  
  // _width_fond();
  // _width_lmdb();
  _width_rock();
  // _width_sqlt();  // DONE


  /* speed ==================== */
  
  // _test_speed_fond();
  // _test_speed_lmdb();
  // _test_speed_rock();
  // _test_speed_sqlt();

  /* test ==================== */

  // _test_sqlt();
  // _test_lmdb();

    


}
