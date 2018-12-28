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
#include <limits.h>
#include <uv.h>
#include <sigsegv.h>
#include <curses.h>
#include <termios.h>
#include <term.h>
#include <dirent.h>
#include <openssl/ssl.h>
#include <openssl/rand.h>

#include "h2o.h"

#define U3_GLOBAL
#define C3_GLOBAL
#include "all.h"
#include "vere/vere.h"

/* Require unsigned char
 */
STATIC_ASSERT(( 0 == CHAR_MIN && UCHAR_MAX == CHAR_MAX ), "unsigned char required");

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
  u3_Host.ops_u.dem = c3n;
  u3_Host.ops_u.dry = c3n;
  u3_Host.ops_u.etn = c3n;
  u3_Host.ops_u.gab = c3n;
  u3_Host.ops_u.git = c3n;
  u3_Host.ops_u.has = c3n;
  u3_Host.ops_u.net = c3y;
  u3_Host.ops_u.nuu = c3n;
  u3_Host.ops_u.pro = c3n;
  u3_Host.ops_u.qui = c3n;
  u3_Host.ops_u.rep = c3n;
  u3_Host.ops_u.tex = c3n;
  u3_Host.ops_u.veb = c3n;
  u3_Host.ops_u.kno_w = DefaultKernel;

  u3_Host.ops_u.rop_s = 0;
  u3_Host.ops_u.raf_c = 0;
  u3_Host.ops_u.nam_c = 0;

  while ( (ch_i=getopt(argc, argv,"G:B:K:A:H:w:u:j:e:E:f:F:k:p:LabcdgqstvxPDRS")) != -1 ) {
    switch ( ch_i ) {
      case 'B': {
        u3_Host.ops_u.pil_c = strdup(optarg);
        break;
      }
      case 'G': {
        u3_Host.ops_u.gen_c = strdup(optarg);
        break;
      }
      case 'A': {
        u3_Host.ops_u.arv_c = strdup(optarg);
        break;
      }
      case 'H': {
        u3_Host.ops_u.dns_c = strdup(optarg);
        break;
      }
      case 'e': {
        u3_Host.ops_u.eth_c = strdup(optarg);
        break;
      }
      case 'E': {
        u3_Host.ops_u.ets_c = strdup(optarg);
        break;
      }
      case 'F': {
        u3_Host.ops_u.fak_c = _main_presig(optarg);
        break;
      }
      case 'w': {
        u3_Host.ops_u.who_c = _main_presig(optarg);
        u3_Host.ops_u.nuu = c3y;
        break;
      }
      case 'u': {
        u3_Host.ops_u.url_c = strdup(optarg);
        break;
      }
      case 'j': {
        u3_Host.ops_u.json_file_c = strdup(optarg);
        break;
      }
      case 'x': {
        u3_Host.ops_u.tex = c3y;
        break;
      }
      case 'f': {
        if ( c3n == _main_readw(optarg, 100, &u3_Host.ops_u.fuz_w) ) {
          return c3n;
        }
        break;
      }
      case 'K': {
        if ( c3n == _main_readw(optarg, 256, &u3_Host.ops_u.kno_w) ) {
          return c3n;
        }
        break;
      }
      case 'k': {
        u3_Host.ops_u.key_c = strdup(optarg);
        break;
      }
      case 'p': {
        if ( c3n == _main_readw(optarg, 65536, &arg_w) ) {
          return c3n;
        } else u3_Host.ops_u.por_s = arg_w;
        break;
      }
      case 'R': {
        u3_Host.ops_u.rep = c3y;
        return c3y;
      }
      case 'L': { u3_Host.ops_u.net = c3n; break; }
      case 'a': { u3_Host.ops_u.abo = c3y; break; }
      case 'b': { u3_Host.ops_u.bat = c3y; break; }
      case 'c': { u3_Host.ops_u.nuu = c3y; break; }
      case 'd': { u3_Host.ops_u.dem = c3y; break; }
      case 'g': { u3_Host.ops_u.gab = c3y; break; }
      case 'P': { u3_Host.ops_u.pro = c3y; break; }
      case 'D': { u3_Host.ops_u.dry = c3y; break; }
      case 'q': { u3_Host.ops_u.qui = c3y; break; }
      case 'v': { u3_Host.ops_u.veb = c3y; break; }
      case 's': { u3_Host.ops_u.git = c3y; break; }
      case 'S': { u3_Host.ops_u.has = c3y; break; }
      case 't': { u3_Host.ops_u.etn = c3y; break; }
      case '?': default: {
        return c3n;
      }
    }
  }

#if defined(U3_OS_bsd)
  if (u3_Host.ops_u.pro == c3y) {
    fprintf(stderr, "profiling isn't yet supported on BSD\r\n");
    return c3n;
  }
#endif

  if ( 0 != u3_Host.ops_u.fak_c ) {
    if ( 28 < strlen(u3_Host.ops_u.fak_c) ) {
      fprintf(stderr, "fake comets are disallowed\r\n");
      return c3n;
    }

    u3_Host.ops_u.who_c = strdup(u3_Host.ops_u.fak_c);
    u3_Host.ops_u.has = c3y;  /* no battery hashing on fake ships. */
    u3_Host.ops_u.net = c3n;  /* no networking on fake ships. */
    u3_Host.ops_u.nuu = c3y;
  }

  c3_t imp_t = ( (0 != u3_Host.ops_u.who_c) && (4 == strlen(u3_Host.ops_u.who_c)) );

  if ( u3_Host.ops_u.ets_c == 0 && c3y == u3_Host.ops_u.etn ) {
    fprintf(stderr, "can't trust Ethereum snapshot without specifying "
                    "snapshot with -E\n");
    return c3n;
  }

  if ( (0 == u3_Host.ops_u.fak_c) && (0 == u3_Host.ops_u.eth_c) && imp_t ) {
    fprintf(stderr, "can't create a new galaxy without specifying "
                    "the Ethereum gateway with -e\n");
    return c3n;
  }

  if ( u3_Host.ops_u.gen_c != 0 && u3_Host.ops_u.nuu == c3n ) {
    fprintf(stderr, "-G only makes sense when bootstrapping a new instance\n");
    return c3n;
  }

  if ( c3y == u3_Host.ops_u.bat ) {
    u3_Host.ops_u.dem = c3y;
    u3_Host.ops_u.nuu = c3y;
  }

  if ( u3_Host.ops_u.nuu != c3y && u3_Host.ops_u.dns_c != 0) {
    fprintf(stderr, "-H only makes sense when bootstrapping a new instance\n");
    return c3n;
  }

  if ( u3_Host.ops_u.nuu != c3y && u3_Host.ops_u.pil_c != 0) {
    fprintf(stderr, "-B only makes sense when bootstrapping a new instance\n");
    return c3n;
  }

  if ( u3_Host.ops_u.nuu != c3y && u3_Host.ops_u.key_c != 0) {
    fprintf(stderr, "-k only makes sense when bootstrapping a new instance\n");
    return c3n;
  }

  if ( u3_Host.ops_u.nuu != c3y && u3_Host.ops_u.url_c != 0 ) {
    fprintf(stderr, "-u only makes sense when bootstrapping a new instance\n");
    return c3n;

  } else if ( u3_Host.ops_u.nuu == c3y
           && u3_Host.ops_u.url_c == 0
           && u3_Host.ops_u.git == c3n ) {

    u3_Host.ops_u.url_c = "https://bootstrap.urbit.org/urbit-" URBIT_VERSION ".pill";

  } else if ( u3_Host.ops_u.nuu == c3y
           && u3_Host.ops_u.url_c == 0
           && u3_Host.ops_u.arv_c == 0 ) {

    fprintf(stderr, "-s only makes sense with -A\n");
    return c3n;
  }

  if ( u3_Host.ops_u.pil_c != 0 ) {
    struct stat s;
    if ( stat(u3_Host.ops_u.pil_c, &s) != 0 ) {
      fprintf(stderr, "pill %s not found\n", u3_Host.ops_u.pil_c);
      return c3n;
    }
  }

  if ( u3_Host.ops_u.key_c != 0 ) {
    struct stat s;
    if ( stat(u3_Host.ops_u.key_c, &s) != 0 ) {
      fprintf(stderr, "keyfile %s not found\n", u3_Host.ops_u.key_c);
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
  c3_c *use_c[] = {
    "Urbit: a personal server operating function\n",
    "https://urbit.org\n",
    "Version " URBIT_VERSION "\n",
    "\n",
    "Usage: %s [options...] ship_name\n",
    "where ship_name is a @p phonetic representation of an urbit address\n",
    "without the leading '~', and options is some subset of the following:\n",
    "\n",
    "-A dir        Use dir for initial galaxy sync\n",
    "-B pill       Bootstrap from this pill\n",
    "-b            Batch create\n",
    "-c pier       Create a new urbit in pier/\n",
    "-D            Recompute from events\n",
    "-d            Daemon mode\n",
    "-e url        Ethereum gateway\n",
    "-F ship       Fake keys; also disables networking\n",
    "-f            Fuzz testing\n",
    "-g            Set GC flag\n",
    "-j file       Create json trace file\n",
    "-K stage      Start at Hoon kernel version stage\n",
    "-k keys       Private key file\n",
    "-L            local networking only\n",
    "-P            Profiling\n",
    "-p ames_port  Set the ames port to bind to\n",
    "-q            Quiet\n",
    "-R            Report urbit build info\n",
    "-S            Disable battery hashing\n",
    "-s            Pill URL from arvo git hash\n",
    "-u url        URL from which to download pill\n",
    "-v            Verbose\n",
    "-w name       Boot as ~name\n",
    "-x            Exit immediately\n",
    "\n",
    "Development Usage:\n",
    "   To create a development ship, use a fakezod:\n",
    "   %s -F zod -A /path/to/arvo/folder -B /path/to/pill -c zod\n",
    "\n",
    "   For more information about developing on urbit, see:\n",
    "   https://github.com/urbit/urbit/blob/master/CONTRIBUTING.md\n",
    "\n",
    "Simple Usage: \n",
    "   %s -c <mycomet> to create a comet (anonymous urbit)\n",
    "   %s -w <myplanet> -t <myticket> if you have a ticket\n",
    "   %s <myplanet or mycomet> to restart an existing urbit\n",
    0
  };

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
  printf("libh2o: %d.%d.%d\n", H2O_LIBRARY_VERSION_MAJOR, H2O_LIBRARY_VERSION_MINOR, H2O_LIBRARY_VERSION_PATCH);
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
  printf("urbit %s\n", URBIT_VERSION);
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

      /*  Set hashboard flag
      */
      if ( _(u3_Host.ops_u.has) ) {
        u3C.wag_w |= u3o_hashless;
      }

      /*  Set tracing flag
      */
      if ( u3_Host.ops_u.json_file_c ) {
        u3C.wag_w |= u3o_trace;
        u3t_trace_open(u3_Host.ops_u.json_file_c);
      }
    }
    u3m_boot(u3_Host.ops_u.nuu,
             u3_Host.ops_u.gab,
             u3_Host.dir_c,
             u3_Host.ops_u.pil_c,
             u3_Host.ops_u.url_c,
             u3_Host.ops_u.arv_c);

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

  SSL_library_init();
  SSL_load_error_strings();

  // u3e_grab("main", u3_none);
  //
  u3_lo_loop();

  return 0;
}
