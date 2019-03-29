/* vere/main.c
**
*/
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <limits.h>
#include <uv.h>
#include <sigsegv.h>
#include <ncurses/curses.h>
#include <termios.h>
#include <ncurses/term.h>
#include <dirent.h>
#include <openssl/ssl.h>
#include <openssl/rand.h>
#include <h2o.h>

#define U3_GLOBAL
#define C3_GLOBAL
#include "all.h"
#include "vere/vere.h"

/* Require unsigned char
 */
STATIC_ASSERT(( 0 == CHAR_MIN && UCHAR_MAX == CHAR_MAX ),
              "unsigned char required");

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
  u3_Host.ops_u.can = c3n;
  u3_Host.ops_u.dem = c3n;
  u3_Host.ops_u.dry = c3n;
  u3_Host.ops_u.etn = c3n;
  u3_Host.ops_u.gab = c3n;
  u3_Host.ops_u.git = c3n;

  //  always disable hashboard
  //  XX temporary, remove once hashes are added
  //
  u3_Host.ops_u.has = c3y;

  u3_Host.ops_u.net = c3y;
  u3_Host.ops_u.nuu = c3n;
  u3_Host.ops_u.pro = c3n;
  u3_Host.ops_u.qui = c3n;
  u3_Host.ops_u.rep = c3n;
  u3_Host.ops_u.tex = c3n;
  u3_Host.ops_u.tra = c3n;
  u3_Host.ops_u.veb = c3n;
  u3_Host.ops_u.kno_w = DefaultKernel;

  while ( -1 != (ch_i=getopt(argc, argv,
                 "G:J:B:K:A:H:w:u:e:E:f:F:k:m:p:LjabcCdgqstvxPDRS")) )
  {
    switch ( ch_i ) {
      case 'J': {
        u3_Host.ops_u.lit_c = strdup(optarg);
        break;
      }
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
        u3_Host.ops_u.net   = c3n;
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
      case 'm': {
        u3_Host.ops_u.sap_c = strdup(optarg);
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
      case 'j': { u3_Host.ops_u.tra = c3y; break; }
      case 'a': { u3_Host.ops_u.abo = c3y; break; }
      case 'b': { u3_Host.ops_u.bat = c3y; break; }
      case 'c': { u3_Host.ops_u.nuu = c3y; break; }
      case 'C': { u3_Host.ops_u.can = c3y; break; }
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
  }

  if ( c3y == u3_Host.ops_u.bat ) {
    u3_Host.ops_u.dem = c3y;
    u3_Host.ops_u.nuu = c3y;
  }

  //  make -c optional
  //
  if ( c3n == u3_Host.ops_u.nuu ) {
    struct stat s;
    if ( 0 != stat(u3_Host.dir_c, &s) ) {
      u3_Host.ops_u.nuu = c3y;
    }
  }

  c3_t imp_t = ((0 != u3_Host.ops_u.who_c) &&
                (4 == strlen(u3_Host.ops_u.who_c)));

  if ( u3_Host.ops_u.gen_c != 0 && u3_Host.ops_u.nuu == c3n ) {
    fprintf(stderr, "-G only makes sense when bootstrapping a new instance\n");
    return c3n;
  }

  if ( u3_Host.ops_u.nuu != c3y && u3_Host.ops_u.who_c != 0) {
    fprintf(stderr, "-w only makes sense when creating a new ship\n");
    return c3n;
  }

  if ( u3_Host.ops_u.nuu != c3y && u3_Host.ops_u.pil_c != 0) {
    fprintf(stderr, "-B only makes sense when creating a new ship\n");
    return c3n;
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
  }

  if ( u3_Host.ops_u.nuu != c3y && u3_Host.ops_u.sap_c != 0 ) {
    fprintf(stderr, "-m only makes sense when bootstrapping a new instance\n");
    return c3n;
  }

  if ( u3_Host.ops_u.fak_c != 0 && u3_Host.ops_u.sap_c != 0 ) {
    fprintf(stderr, "-m and -F cannot be used together\n");
    return c3n;
  }

  if ( u3_Host.ops_u.ets_c != 0 && u3_Host.ops_u.sap_c != 0 ) {
    fprintf(stderr, "-m and -E cannot be used together\n");
    return c3n;
  }
  if ( u3_Host.ops_u.can == c3y && u3_Host.ops_u.sap_c != 0 ) {
    fprintf(stderr, "-m and -C cannot be used together\n");
    return c3n;
  }
  if ( u3_Host.ops_u.can == c3y && u3_Host.ops_u.ets_c != 0 ) {
    fprintf(stderr, "-C and -E cannot be used together\n");
    return c3n;
  }

  if ( u3_Host.ops_u.eth_c == 0 && imp_t ) {
    u3_Host.ops_u.eth_c = "http://eth-mainnet.urbit.org:8545";
  }

  if ( u3_Host.ops_u.sap_c == 0 && u3_Host.ops_u.can == c3n ) {

    u3_Host.ops_u.sap_c =
        "https://bootstrap.urbit.org/urbit-" URBIT_VERSION ".snap";
  }

  if ( u3_Host.ops_u.url_c != 0 && u3_Host.ops_u.pil_c != 0 ) {
    fprintf(stderr, "-B and -u cannot be used together\n");
    return c3n;
  }
  else if ( u3_Host.ops_u.nuu == c3y
           && u3_Host.ops_u.url_c == 0
           && u3_Host.ops_u.git == c3n ) {
    u3_Host.ops_u.url_c =
      "https://bootstrap.urbit.org/urbit-" URBIT_VERSION ".pill";
  }
  else if ( u3_Host.ops_u.nuu == c3y
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

  return c3y;
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
    // XX find a way to re-enable
    // "-A dir        Use dir for initial galaxy sync\n",
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
    // XX find a way to re-enable
    // "-s            Pill URL from arvo git hash\n",
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
    "   %s -c <my-comet> to create a comet (anonymous urbit)\n",
    "   %s -w <my-planet> -k <my-key-file> if you own a planet\n",
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

      u3_pier_exit(u3A);

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
  printf("sigsegv: %d.%d\n",
         (libsigsegv_version >> 8) & 0xff,
         libsigsegv_version & 0xff);
  printf("openssl: %s\n", SSLeay_version(SSLEAY_VERSION));
  printf("curses: %s\n", curses_version());
  printf("libuv: %s\n", uv_version_string());
  printf("libh2o: %d.%d.%d\n",
         H2O_LIBRARY_VERSION_MAJOR,
         H2O_LIBRARY_VERSION_MINOR,
         H2O_LIBRARY_VERSION_PATCH);
}

void
_stop_exit(c3_i int_i)
{
  fprintf(stderr, "\r\n[received keyboard stop signal, exiting]\r\n");
  //  XX crashes if we haven't started a pier yet
  //
  u3_pier_exit();
}

/*
  This is set to the the write-end of a pipe when Urbit is started in
  daemon mode. It's meant to be used as a signal to the parent process
  that the child process has finished booting.
*/
static c3_i _child_process_booted_signal_fd = -1;

/*
  This should be called whenever the ship has been booted enough to
  handle commands from automation code. Specifically, once the Eyre's
  `chis` interface is up and running.

  In daemon mode, this signals to the parent process that it can
  exit. Otherwise, it does nothing.

  Once we've sent a signal with `write`, we close the file descriptor
  and overwrite the global to make it impossible to accidentally do
  this twice.
*/
static void _on_boot_completed_cb() {
  c3_c buf[2] = {0,0};

  if ( -1 == _child_process_booted_signal_fd ) {
    return;
  }

  if ( 0 == write(_child_process_booted_signal_fd, buf, 1) ) {
    c3_assert(!"_on_boot_completed_cb: Can't write to parent FD");
  }

  close(_child_process_booted_signal_fd);
  _child_process_booted_signal_fd = -1;
}

/*
  In daemon mode, run the urbit as a background process, but don't
  exit from the parent process until the ship is finished booting.

  We use a pipe to communicate between the child and the parent. The
  parent waits for the child to write something to the pipe and
  then exits. If the pipe is closed with nothing written to it, get
  the exit status from the child process and also exit with that status.

  We want the child to write to the pipe once it's booted, so we put
  `_on_boot_completed_cb` into `u3_Host.bot_f`, which is NULL in
  non-daemon mode. That gets called once the `chis` service is
  available.

  In both processes, we are good fork() citizens, and close all unused
  file descriptors. Closing `pipefd[1]` in the parent process is
  especially important, since the pipe needs to be closed if the child
  process dies. When the pipe is closed, the read fails, and that's
  how we know that something went wrong.

  There are some edge cases around `WEXITSTATUS` that are not handled
  here, but I don't think it matters.
*/
static void
_fork_into_background_process()
{
  c3_i pipefd[2];

  if ( 0 != pipe(pipefd) ) {
    c3_assert(!"Failed to create pipe");
  }

  pid_t childpid = fork();

  if ( 0 == childpid ) {
    close(pipefd[0]);
    _child_process_booted_signal_fd = pipefd[1];
    u3_Host.bot_f = _on_boot_completed_cb;
    return;
  }

  close(pipefd[1]);
  close(0);
  close(1);
  close(2);

  c3_c buf[2] = {0,0};
  if ( 1 == read(pipefd[0], buf, 1) ) {
    exit(0);
  }

  c3_i status;
  wait(&status);
  exit(WEXITSTATUS(status));
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

  //  Set `u3_Host.wrk_c` to the worker executable path.
  c3_i worker_exe_len = 1 + strlen(argv[0]) + strlen("-worker");
  u3_Host.wrk_c = c3_malloc(worker_exe_len);
  snprintf(u3_Host.wrk_c, worker_exe_len, "%s-worker", argv[0]);

  if ( c3y == u3_Host.ops_u.dem ) {
    _fork_into_background_process();
  }

  if ( c3y == u3_Host.ops_u.rep ) {
    report();
    return 0;
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

  // prints the absolute path of the pier
  //
  c3_c* abs_c = realpath(u3_Host.dir_c, 0);

  // if the ship is being booted, we use realpath(). Otherwise, we use getcwd()
  // with a memory-allocation loop
  //
  if (abs_c == NULL) {
    c3_i mprint_i = 1000;
    abs_c = c3_malloc(mprint_i);

    // allocates more memory as needed if the path is too large
    //
    while ( abs_c != getcwd(abs_c, mprint_i) ) {
      free(abs_c);
      mprint_i *= 2;
      abs_c = c3_malloc(mprint_i);
    }
    printf("boot: home is %s/%s\n", abs_c, u3_Host.dir_c);
    free(abs_c);
  } else {
    printf("boot: home is %s\n", abs_c);
    free(abs_c);
  }
  // printf("vere: hostname is %s\n", u3_Host.ops_u.nam_c);

  if ( c3y == u3_Host.ops_u.dem && c3n == u3_Host.ops_u.bat ) {
    printf("boot: running as daemon\n");
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
      if ( _(u3_Host.ops_u.tra) ) {
        u3C.wag_w |= u3o_trace;
        u3_Host.tra_u.nid_w = 0;
        u3_Host.tra_u.fil_u = NULL;
        u3_Host.tra_u.con_w = 0;
      }
    }

    /*  Initialize OpenSSL for client and server
    */
    SSL_library_init();
    SSL_load_error_strings();

    {
      c3_i rad;
      c3_y buf[4096];

      // RAND_status, at least on OS X, never returns true.
      // 4096 bytes should be enough entropy for anyone, right?
      rad = open("/dev/urandom", O_RDONLY);
      if ( 4096 != read(rad, &buf, 4096) ) {
        perror("rand-seed");
        exit(1);
      }
      RAND_seed(buf, 4096);
      close(rad);
    }

    u3_king_commence();
  }
  return 0;
}
