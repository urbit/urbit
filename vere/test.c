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

#include "all.h"
#include "vere/vere.h"

#include <math.h>  // NOTFORCHECKIN - TESTING

FILE * ulog;

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
  wric(wit_u, wit_u->evt_d, (c3_y *) str_c,  (c3_y *) str_c + PERS_WRIT_HEAD_SIZE, len);
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
  
// NOTFORCHECKIN  _test_write(4, pir_u);
// NOTFORCHECKIN  _test_write(10, pir_u);
  _test_write(100, pir_u);

#if 0
  sleep(1);
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
// NOTFORCHECKIN  _test_read(pir_u, (c3_y * ) "abb");
    
  pir_u ->pin_u -> pos_d = 10;
// NOTFORCHECKIN  _test_read(pir_u, (c3_y * ) "abbcccddd");
    
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

#if 1
  fprintf(stderr, "about to sleep for gdb in main.c - PID = %i\n\r", getpid());
  volatile int ii=0;
  while (1 != ii){
    fprintf(stderr, "...sleep...\n\r");
    sleep(1);
  }
#endif
  
  u3C.dir_c = "/home/tjic/bus/tlon/urbit_cc_merge";
  _test_fond();
  _test_sqlt();

}
