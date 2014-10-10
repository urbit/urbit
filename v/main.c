/* v/main.c
**
** This file is in the public domain.
*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <setjmp.h>
#include <signal.h>
#include <gmp.h>
#include <stdint.h>
#include <uv.h>
#include <sigsegv.h>
#include <curses.h>
#include <termios.h>
#include <term.h>
#include <dirent.h>

#define U2_GLOBAL
#define C3_GLOBAL
#include "all.h"
#include "v/vere.h"

/* _main_readw(): parse a word from a string.
*/
static u3_bean
_main_readw(const c3_c* str_c, c3_w max_w, c3_w* out_w)
{
  c3_c* end_c;
  c3_w  par_w = strtoul(str_c, &end_c, 0);

  if ( *str_c != '\0' && *end_c == '\0' && par_w < max_w ) {
    *out_w = par_w;
    return u3_yes;
  }
  else return u3_no;
}

/* _main_getopt(): extract option map from command line.
*/
static u3_bean
_main_getopt(c3_i argc, c3_c** argv)
{
  c3_i ch_i;
  c3_w arg_w;

  u3_Host.ops_u.abo = u3_no;
  u3_Host.ops_u.bat = u3_no;
  u3_Host.ops_u.gab = u3_no;
  u3_Host.ops_u.loh = u3_no;
  u3_Host.ops_u.dem = u3_no;
  u3_Host.ops_u.fog = u3_no;
  u3_Host.ops_u.fak = u3_no;
  u3_Host.ops_u.pro = u3_no;
  u3_Host.ops_u.veb = u3_yes;
  u3_Host.ops_u.nuu = u3_no;
  u3_Host.ops_u.mem = u3_no;
  u3_Host.ops_u.kno_w = DefaultKernel;

  while ( (ch_i = getopt(argc, argv, "I:X:f:k:l:n:p:r:LabcdgqvFM")) != -1 ) {
    switch ( ch_i ) {
      case 'M': {
        u3_Host.ops_u.mem = u3_yes;
        break;
      }
      case 'I': {
        u3_Host.ops_u.imp_c = strdup(optarg);
        break;
      }
      case 'X': {
        if ( 0 != strcmp("wtf", optarg) ) {
          return u3_no;
        } else u3_Host.ops_u.fog = u3_yes;
        break;
      }
      case 'f': {
        if ( u3_no == _main_readw(optarg, 100, &u3_Host.ops_u.fuz_w) ) {
          return u3_no;
        }
        break;
      }
      case 'k': {
        if ( u3_no == _main_readw(optarg, 256, &u3_Host.ops_u.kno_w) ) {
          return u3_no;
        }
        break;
      }
      case 'l': {
        if ( u3_no == _main_readw(optarg, 65536, &arg_w) ) {
          return u3_no;
        } else u3_Host.ops_u.rop_s = arg_w;
        break;
      }
      case 'n': {
        u3_Host.ops_u.nam_c = strdup(optarg);
        break;
      }
      case 'p': {
        if ( u3_no == _main_readw(optarg, 65536, &arg_w) ) {
          return u3_no;
        } else u3_Host.ops_u.por_s = arg_w;
        break;
      }
      case 'r': {
        u3_Host.ops_u.raf_c = strdup(optarg);
        break;
      }
      case 'L': { u3_Host.ops_u.loh = u3_yes; break; }
      case 'F': {
        u3_Host.ops_u.loh = u3_yes;
        u3_Host.ops_u.fak = u3_yes;
        break;
      }
      case 'a': { u3_Host.ops_u.abo = u3_yes; break; }
      case 'b': { u3_Host.ops_u.bat = u3_yes; break; }
      case 'c': { u3_Host.ops_u.nuu = u3_yes; break; }
      case 'd': { u3_Host.ops_u.dem = u3_yes; break; }
      case 'g': { u3_Host.ops_u.gab = u3_yes; break; }
      case 'q': { u3_Host.ops_u.veb = u3_no; break; }
      case 'v': { u3_Host.ops_u.veb = u3_yes; break; }
      case '?': default: {
        return u3_no;
      }
    }
  }

  if ( u3_Host.ops_u.rop_s == 0 && u3_Host.ops_u.raf_c != 0 ) {
    fprintf(stderr, "The -r flag requires -l.\n");
    return u3_no;
  }

  if ( u3_yes == u3_Host.ops_u.bat ) {
    u3_Host.ops_u.dem = u3_yes;
    u3_Host.ops_u.nuu = u3_yes;
  }


  if ( u3_Host.ops_u.nam_c == 0 ) {
    u3_Host.ops_u.nam_c = getenv("HOSTNAME");
    if ( u3_Host.ops_u.nam_c == 0 ) {
      c3_w len_w = sysconf(_SC_HOST_NAME_MAX) + 1;

      u3_Host.ops_u.nam_c = c3_malloc(len_w);
      if ( 0 != gethostname(u3_Host.ops_u.nam_c, len_w) ) {
        perror("gethostname");
        exit(1);
      }
    }
  }

  if ( argc != (optind + 1) ) {
    return u3_no;
  } else {
    {
      c3_c* ash_c;

      if ( (ash_c = strrchr(argv[optind], '/')) && (ash_c[1] == 0) ) {
        *ash_c = 0;
      }
    }

    u3_Host.cpu_c = strdup(argv[optind]);
    return u3_yes;
  }
}

/* u3_ve_usage(): print usage and exit.
*/
static void
u3_ve_usage(c3_i argc, c3_c** argv)
{
  fprintf(stderr, "%s: usage: [-v] [-k stage] [-p ames_port] computer\n",
                  argv[0]);
  exit(1);
}

#if 0
/* u3_ve_panic(): panic and exit.
*/
static void
u3_ve_panic(c3_i argc, c3_c** argv)
{
  fprintf(stderr, "%s: gross system failure\n", argv[0]);
  exit(1);
}
#endif

/* u3_ve_sysopt(): apply option map to system state.
*/
static void
u3_ve_sysopt()
{
  u3_Local = strdup(u3_Host.cpu_c);
  u3_System = U2_LIB;
  u3_Flag_Abort = u3_Host.ops_u.abo;
  u3_Flag_Garbage = u3_Host.ops_u.gab;
  u3_Flag_Profile = u3_Host.ops_u.pro;
  u3_Flag_Verbose = u3_Host.ops_u.veb;
}

#if 0
static jmp_buf Signal_buf;
#ifndef SIGSTKSZ
# define SIGSTKSZ 16384
#endif
static uint8_t Sigstk[SIGSTKSZ];

volatile enum { sig_none, sig_overflow, sig_interrupt } Sigcause;

static void _main_cont(void *arg1, void *arg2, void *arg3)
{
  (void)(arg1);
  (void)(arg2);
  (void)(arg3);
  siglongjmp(Signal_buf, 1);
}

static void
overflow_handler(int emergency, stackoverflow_context_t scp)
{
  if ( 1 == emergency ) {
    write(2, "stack emergency\n", strlen("stack emergency" + 2));
    exit(1);
  } else {
    Sigcause = sig_overflow;
    sigsegv_leave_handler(_main_cont, NULL, NULL, NULL);
  }
}

  //  Install signal handlers and set buffers.
  //
  //  Note that we use the sigmask-restoring variant.  Essentially, when
  //  we get a signal, we force the system back into the just-booted state.
  //  If anything goes wrong during boot (above), it's curtains.
  {
    if ( 0 != sigsetjmp(Signal_buf, 1) ) {
      switch ( Sigcause ) {
        case sig_overflow: printf("[stack overflow]\r\n"); break;
        case sig_interrupt: printf("[interrupt]\r\n"); break;
        default: printf("[signal error!]\r\n"); break;
      }
      Sigcause = sig_none;

      signal(SIGINT, SIG_DFL);
      stackoverflow_deinstall_handler();

      //  Print the trace, do a GC, etc.
      //
      //  This is half-assed at present, so we exit.
      //
      u3_lo_sway(0, u3k(u3_wire_tax(u3_Wire)));

      u3_lo_bail(u3A);

      exit(1);
    }
    if ( -1 == stackoverflow_install_handler
        (overflow_handler, Sigstk, SIGSTKSZ) )
    {
      fprintf(stderr, "overflow_handler: install failed\n");
      exit(1);
    }
    signal(SIGINT, interrupt_handler);
    signal(SIGIO, SIG_IGN);
  }

static void
interrupt_handler(int x)
{
  Sigcause = sig_interrupt;
  longjmp(Signal_buf, 1);
}
#endif

// #define GRAB

c3_i
main(c3_i   argc,
     c3_c** argv)
{
  //  Parse options.
  //
  if ( u3_no == _main_getopt(argc, argv) ) {
    u3_ve_usage(argc, argv);
    return 1;
  }

  u3_ve_sysopt();

  printf("~\n");
  printf("welcome.\n");
  printf("vere: urbit home is %s\n", u3_Host.cpu_c);
  printf("vere: hostname is %s\n", u3_Host.ops_u.nam_c);

  if ( u3_yes == u3_Host.ops_u.dem && u3_no == u3_Host.ops_u.bat ) {
    printf("vere: running as daemon\n");
  }

  //  Seed prng. Don't panic -- just for fuzz testing.
  //
  srand(getpid());

  //  Instantiate process globals.
  {
    /*  Boot the image and checkpoint.
    */
    u3_ce_boot(u3_Host.ops_u.nuu, u3_Host.cpu_c);

    /*  Start Arvo.
    */
#if 0
    {
      struct timeval tim_tv;
      u3_noun        now;

      gettimeofday(&tim_tv, 0);
      now = u3_time_in_tv(&tim_tv);

      u3_cv_start(now);
    }
#endif
#if 0
    /*  Initial checkpoint.
    */
    if ( u3_so(u3_Host.ops_u.nuu) ) {
      printf("about to save.\r\n");
      u3_ce_save();
      printf("saved.\r\n");
      exit(1);
    }
#endif
  }

#ifdef GRAB
  {
    u3_noun fur = u3_cv_wish("(dec 199)");

    u3_cm_p("fur", fur);
    u3z(fur);
  }

  u3_ce_grab("main");
#else
#if 1
  u3_lo_loop();
#else
  {
    u3_noun imp, num;
   
    u3_leak_on(1);
    imp = u3_ci_string(u3_Host.ops_u.imp_c);
    u3_leak_on(2);
    num = u3_dc("slaw", 'p', imp);
    u3z(num);
    u3_leak_off;

    u3_ce_grab("init");
  }
#endif
#endif
  return 0;
}
