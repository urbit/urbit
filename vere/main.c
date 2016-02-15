/* v/main.c
**
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
#include <openssl/ssl.h>

#define U3_GLOBAL
#define C3_GLOBAL
#include "all.h"
#include "vere/vere.h"

/* _main_readw(): parse a word from a string.
*/
static u3_noun
_main_readw(const c3_c* str_c, c3_w max_w, c3_w* out_w)
{
  c3_c* end_c;
  c3_w  par_w = strtoul(str_c, &end_c, 0);

  if ( *str_c != '\0' && *end_c == '\0' && par_w < max_w ) {
    *out_w = par_w;
    return c3y;
  }
  else return c3n;
}

static c3_c hostbuf[2048];  // kill me

/* _main_presig(): prefix optional sig.
*/
c3_c* 
_main_presig(c3_c* txt_c)
{
  c3_c* new_c = malloc(2 + strlen(txt_c));

  if ( '~' == *txt_c ) {
    strcpy(new_c, txt_c);
  } else {
    new_c[0] = '~';
    strcpy(new_c + 1, txt_c);
  }
  return new_c;
}

/* _main_getopt(): extract option map from command line.
*/
static u3_noun
_main_getopt(c3_i argc, c3_c** argv)
{
  c3_i ch_i;
  c3_w arg_w;

  u3_Host.ops_u.abo = c3n;
  u3_Host.ops_u.bat = c3n;
  u3_Host.ops_u.gab = c3n;
  u3_Host.ops_u.loh = c3n;
  u3_Host.ops_u.dem = c3n;
  u3_Host.ops_u.fog = c3n;
  u3_Host.ops_u.fak = c3n;
  u3_Host.ops_u.tex = c3n;
  u3_Host.ops_u.pro = c3n;
  u3_Host.ops_u.dry = c3n;
  u3_Host.ops_u.veb = c3n;
  u3_Host.ops_u.qui = c3n;
  u3_Host.ops_u.nuu = c3n;
  u3_Host.ops_u.mem = c3n;
  u3_Host.ops_u.rep = c3n;
  u3_Host.ops_u.kno_w = DefaultKernel;

  while ( (ch_i=getopt(argc, argv,"B:I:w:t:f:k:l:n:p:r:LabcdgqvxFMPDXR")) != -1 ) {
    switch ( ch_i ) {
      case 'M': {
        u3_Host.ops_u.mem = c3y;
        break;
      }
      case 'B': {
        u3_Host.ops_u.pil_c = strdup(optarg);
        break;
      }
      case 'I': {
        u3_Host.ops_u.imp_c = _main_presig(optarg);
        break;
      }
      case 'w': {
        u3_Host.ops_u.who_c = _main_presig(optarg);
        u3_Host.ops_u.nuu = c3y;
        break;
      }
      case 't': {
        u3_Host.ops_u.tic_c = _main_presig(optarg);
        break;
      }
      case 'x': {
        u3_Host.ops_u.tex = c3y;
        break;
      }
      case 'X': {
        u3_Host.ops_u.fog = c3y;
        break;
      }
      case 'f': {
        if ( c3n == _main_readw(optarg, 100, &u3_Host.ops_u.fuz_w) ) {
          return c3n;
        }
        break;
      }
      case 'k': {
        if ( c3n == _main_readw(optarg, 256, &u3_Host.ops_u.kno_w) ) {
          return c3n;
        }
        break;
      }
      case 'l': {
        if ( c3n == _main_readw(optarg, 65536, &arg_w) ) {
          return c3n;
        } else u3_Host.ops_u.rop_s = arg_w;
        break;
      }
      case 'n': {
        u3_Host.ops_u.nam_c = strdup(optarg);
        break;
      }
      case 'p': {
        if ( c3n == _main_readw(optarg, 65536, &arg_w) ) {
          return c3n;
        } else u3_Host.ops_u.por_s = arg_w;
        break;
      }
      case 'r': {
        u3_Host.ops_u.raf_c = strdup(optarg);
        break;
      }
      case 'R': {
        u3_Host.ops_u.rep = c3y;
        return c3y;
      }
      case 'L': { u3_Host.ops_u.loh = c3y; break; }
      case 'F': {
        u3_Host.ops_u.loh = c3y;
        u3_Host.ops_u.fak = c3y;
        break;
      }
      case 'a': { u3_Host.ops_u.abo = c3y; break; }
      case 'b': { u3_Host.ops_u.bat = c3y; break; }
      case 'c': { u3_Host.ops_u.nuu = c3y; break; }
      case 'd': { u3_Host.ops_u.dem = c3y; break; }
      case 'g': { u3_Host.ops_u.gab = c3y; break; }
      case 'P': { u3_Host.ops_u.pro = c3y; break; }
      case 'D': { u3_Host.ops_u.dry = c3y; break; }
      case 'q': { u3_Host.ops_u.qui = c3y; break; }
      case 'v': { u3_Host.ops_u.veb = c3y; break; }
      case '?': default: {
        return c3n;
      }
    }
  }

  if ( u3_Host.ops_u.rop_s == 0 && u3_Host.ops_u.raf_c != 0 ) {
    fprintf(stderr, "The -r flag requires -l.\n");
    return c3n;
  }

  if ( u3_Host.ops_u.tic_c == 0 && u3_Host.ops_u.who_c != 0 ) {
      c3_c tic_c[29];
      printf("your ticket: ~");
      scanf("%28s",tic_c);
      u3_Host.ops_u.tic_c = _main_presig(tic_c);
  }

  if ( c3y == u3_Host.ops_u.bat ) {
    u3_Host.ops_u.dem = c3y;
    u3_Host.ops_u.nuu = c3y;
  }

  if ( u3_Host.ops_u.nuu != c3y && u3_Host.ops_u.pil_c != 0) {
    fprintf(stderr, "-B only makes sense when bootstrapping a new instance\n");
    return c3n;
  }

  if ( u3_Host.ops_u.pil_c != 0 ) {
    struct stat s;
    if ( stat(u3_Host.ops_u.pil_c, &s) != 0 ) {
      fprintf(stderr, "pill %s not found\n", u3_Host.ops_u.pil_c);
      return c3n;
    }
  }

  if ( u3_Host.ops_u.nuu == c3y && u3_Host.ops_u.pil_c == 0) {
    struct stat s;
    if ( stat("urbit.pill", &s) == 0 ) {
      u3_Host.ops_u.pil_c = strdup("urbit.pill");
#ifdef U3_LIB
    } else if ( stat(U3_LIB"/urbit.pill", &s) == 0 ) {
      u3_Host.ops_u.pil_c = strdup(U3_LIB"/urbit.pill");
#endif
    } else {
      fprintf(stderr, "Could not find urbit.pill\n");
      return c3n;
    }
  }

  if ( u3_Host.ops_u.nam_c == 0 ) {
    u3_Host.ops_u.nam_c = getenv("HOSTNAME");
    if ( u3_Host.ops_u.nam_c == 0 ) {
      c3_w len_w = sysconf(_SC_HOST_NAME_MAX) + 1;

      u3_Host.ops_u.nam_c = hostbuf;
      if ( 0 != gethostname(u3_Host.ops_u.nam_c, len_w) ) {
        perror("gethostname");
        exit(1);
      }
    }
  }

  if ( argc != (optind + 1) && u3_Host.ops_u.who_c != 0 ) {
    u3_Host.dir_c = strdup(1 + u3_Host.ops_u.who_c);
  }

  if ( argc != (optind + 1) ) {
    return u3_Host.dir_c ? c3y : c3n;
  } else {
    {
      c3_c* ash_c;

      if ( (ash_c = strrchr(argv[optind], '/')) && (ash_c[1] == 0) ) {
        *ash_c = 0;
      }
    }

    u3_Host.dir_c = strdup(argv[optind]);
    return c3y;
  }
}

/* u3_ve_usage(): print usage and exit.
*/
static void
u3_ve_usage(c3_i argc, c3_c** argv)
{
#if 0
  c3_c *use_c[] = {"Usage: %s [options...] computer\n",
    "-c pier       Create a new urbit in pier/\n",
    "-w name       Immediately upgrade to ~name\n",
    "-t ticket     Use ~ticket automatically\n",
    "-I galaxy     Start as ~galaxy\n",
    "-F            Fake keys\n",
    "-L            Local-only network\n",
    "-n host       Set unix hostname\n",
    "-p ames_port  Set the HTTP port to bind to\n",
    "-v            Verbose\n",
    "-q            Quiet\n",
    "-D            Recompute from events\n",
    "-P            Profiling\n",
    "-b            Batch create\n",
    "-d            Daemon mode\n",
    "-g            Set GC flag\n",
    "-x            Exit immediately\n",
    "-r host       Initial peer address\n",
    "-l port       Initial peer port\n",
    "-M            Memory madness\n",
    "-f            Fuzz testing\n",
    "-k stage      Start at Hoon kernel version stage\n",
    "-R            Report urbit build info\n",
    "-Xwtf         Skip last event\n"};
#else
  c3_c *use_c[] = {
    "simple usage: \n",
    "   %s -c <mycomet> to create a comet (anonymous urbit)\n",
    "   %s -w <myplanet> -t <myticket> if you have a ticket\n",
    "   %s <myplanet or mycomet> to restart an existing urbit\n",
    0
  };
#endif

  c3_i i;
  for ( i=0; use_c[i]; i++ ) {
    fprintf(stderr, use_c[i], argv[0]);
  }
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
  u3_Local = strdup(u3_Host.dir_c);
  u3_System = U3_LIB;
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

#define GRAB

static void
report(void)
{
  printf("---------\nLibraries\n---------\n");
  printf("gmp: %s\n", gmp_version);
  printf("sigsegv: %d.%d\n", (libsigsegv_version >> 8) & 0xff, libsigsegv_version & 0xff);
  printf("openssl: %s\n", SSLeay_version(SSLEAY_VERSION));
  printf("curses: %s\n", curses_version());
  printf("libuv: %s\n", uv_version_string());
}

void
_stop_exit(c3_i int_i)
{
  fprintf(stderr, "\r\n[received keyboard stop signal, exiting]\r\n");
  u3_lo_bail();
}

c3_i
main(c3_i   argc,
     c3_c** argv)
{
  //  Parse options.
  //
  if ( c3n == _main_getopt(argc, argv) ) {
    u3_ve_usage(argc, argv);
    return 1;
  }

  if ( c3y == u3_Host.ops_u.rep ) {
    report();
    return 0;
  }

  if ( c3y == u3_Host.ops_u.nuu ) {
    struct stat s;
    if ( !stat(u3_Host.dir_c, &s) ) {
      fprintf(stderr, "tried to create, but %s already exists\n", u3_Host.dir_c);
      fprintf(stderr, "normal usage: %s %s\n", argv[0], u3_Host.dir_c);
      exit(1);
    }
  } else {
    struct stat s;
    if ( -1 == stat(u3_Host.dir_c, &s) ) {
      fprintf(stderr, "%s: urbit not found\n", u3_Host.dir_c);
      u3_ve_usage(argc, argv);
    }
  }

#if 0
  if ( 0 == getuid() ) {
    chroot(u3_Host.dir_c);
    u3_Host.dir_c = "/";
  }
#endif
  u3_ve_sysopt();

  //  Block profiling signal, which should be delievered to exactly one thread.
  //
  if ( _(u3_Host.ops_u.pro) ) {
    sigset_t set;

    sigemptyset(&set);
    sigaddset(&set, SIGPROF);
    if ( 0 != pthread_sigmask(SIG_BLOCK, &set, NULL) ) {
      perror("pthread_sigmask");
      exit(1);
    }
  }

  //  Handle SIGTSTP as if it was SIGTERM.
  //
  signal(SIGTSTP, _stop_exit);

  printf("~\n");
  //  printf("welcome.\n");
  printf("urbit: home is %s\n", u3_Host.dir_c);
  // printf("vere: hostname is %s\n", u3_Host.ops_u.nam_c);

  if ( c3y == u3_Host.ops_u.dem && c3n == u3_Host.ops_u.bat ) {
    printf("urbit: running as daemon\n");
  }

  //  Seed prng. Don't panic -- just for fuzz testing.
  //
  srand(getpid());

  //  Instantiate process globals.
  {
    /*  Boot the image and checkpoint.  Set flags.
    */
    {
      /*  Set pier directory.
      */
      u3C.dir_c = u3_Host.dir_c;

      /*  Set GC flag.
      */
      if ( _(u3_Host.ops_u.gab) ) {
        u3C.wag_w |= u3o_debug_ram;
      }

      /*  Set profile flag.
      */
      if ( _(u3_Host.ops_u.pro) ) {
        u3C.wag_w |= u3o_debug_cpu;
      }

      /*  Set verbose flag.
      */
      if ( _(u3_Host.ops_u.veb) ) {
        u3C.wag_w |= u3o_verbose;
      }

      /*  Set quiet flag.
      */
      if ( _(u3_Host.ops_u.qui) ) {
        u3C.wag_w |= u3o_quiet;
      }
      
      /*  Set dry-run flag.
      */
      if ( _(u3_Host.ops_u.dry) ) {
        u3C.wag_w |= u3o_dryrun;
      }
    }
    u3m_boot(u3_Host.ops_u.nuu, u3_Host.ops_u.gab, u3_Host.dir_c);

    /*  Start Arvo.
    */
#if 1
    {
      struct timeval tim_tv;
      u3_noun        now;

      gettimeofday(&tim_tv, 0);
      now = u3_time_in_tv(&tim_tv);

      u3v_start(now);
    }
#endif
#if 0
    /*  Initial checkpoint.
    */
    if ( _(u3_Host.ops_u.nuu) ) {
      printf("about to save.\r\n");
      u3e_save();
      printf("saved.\r\n");
    }
#endif
  }

  // u3e_grab("main", u3_none);
  //
  u3_lo_loop();

  return 0;
}
