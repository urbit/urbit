/* vere/main.c
**
*/
#define U3_GLOBAL
#define C3_GLOBAL
#include "all.h"
#include "vere/vere.h"
#if !defined(U3_OS_mingw)
#include <sigsegv.h>
#endif
#include <openssl/conf.h>
#include <openssl/engine.h>
#include <openssl/err.h>
#include <openssl/ssl.h>
#include <h2o.h>
#include <curl/curl.h>
#include <vere/db/lmdb.h>

#include "ca-bundle.h"

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
  c3_c* new_c = c3_malloc(2 + strlen(txt_c));

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
  u3_Host.ops_u.dem = c3n;
  u3_Host.ops_u.dry = c3n;
  u3_Host.ops_u.gab = c3n;
  u3_Host.ops_u.git = c3n;

  //  always disable hashboard
  //  XX temporary, remove once hashes are added
  //
  u3_Host.ops_u.has = c3y;

  u3_Host.ops_u.net = c3y;
  u3_Host.ops_u.lit = c3n;
  u3_Host.ops_u.nuu = c3n;
  u3_Host.ops_u.pro = c3n;
  u3_Host.ops_u.qui = c3n;
  u3_Host.ops_u.rep = c3n;
  u3_Host.ops_u.tem = c3n;
  u3_Host.ops_u.tex = c3n;
  u3_Host.ops_u.tra = c3n;
  u3_Host.ops_u.veb = c3n;
  u3_Host.ops_u.puf_c = "jam";
  u3_Host.ops_u.hap_w = 50000;
  u3_Host.ops_u.kno_w = DefaultKernel;

  while ( -1 != (ch_i=getopt(argc, argv,
                 "X:Y:G:J:B:b:K:A:H:I:C:w:u:e:F:k:n:p:r:i:Z:LljacdgqstvxPDRS")) )
  {
    switch ( ch_i ) {
      case 'X': {
        u3_Host.ops_u.pek_c = strdup(optarg);
        break;
      }
      case 'Y': {
        u3_Host.ops_u.puk_c = strdup(optarg);
        break;
      }
      case 'Z': {
        u3_Host.ops_u.puf_c = strdup(optarg);
        break;
      }
      case 'J': {
        u3_Host.ops_u.lit_c = strdup(optarg);
        break;
      }
      case 'B': {
        u3_Host.ops_u.pil_c = strdup(optarg);
        break;
      }
      case 'b': {
        u3_Host.ops_u.bin_c = strdup(optarg);
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
      case 'I': {
        u3_Host.ops_u.jin_c = strdup(optarg);
        break;
      }
      case 'C': {
        if ( c3n == _main_readw(optarg, 1000000000, &u3_Host.ops_u.hap_w) ) {
          return c3n;
        }
        break;
      }
      case 'e': {
        u3_Host.ops_u.eth_c = strdup(optarg);
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
      case 'n': {
        u3_Host.ops_u.til_c = strdup(optarg);
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
      case 'r': {
        u3_Host.ops_u.roc_c = strdup(optarg);
        break;
      }
      case 'i': {
        u3_Host.ops_u.imp_c = strdup(optarg);
        break;
      }
      case 'L': { u3_Host.ops_u.net = c3n; break; }
      case 'l': { u3_Host.ops_u.lit = c3y; break; }
      case 'j': { u3_Host.ops_u.tra = c3y; break; }
      case 'a': { u3_Host.ops_u.abo = c3y; break; }
      case 'c': { u3_Host.ops_u.nuu = c3y; break; }
      case 'd': { u3_Host.ops_u.dem = c3y; break; }
      case 'g': { u3_Host.ops_u.gab = c3y; break; }
      case 'P': { u3_Host.ops_u.pro = c3y; break; }
      case 'D': { u3_Host.ops_u.dry = c3y; break; }
      case 'q': { u3_Host.ops_u.qui = c3y; break; }
      case 'v': { u3_Host.ops_u.veb = c3y; break; }
      case 's': { u3_Host.ops_u.git = c3y; break; }
      case 'S': { u3_Host.ops_u.has = c3y; break; }
      case 't': { u3_Host.ops_u.tem = c3y; break; }
      case '?': default: {
        return c3n;
      }
    }
  }

#if !defined(U3_OS_PROF)
  if (u3_Host.ops_u.pro == c3y) {
    fprintf(stderr, "profiling isn't yet supported on your OS\r\n");
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

  if ( argc != (optind + 1) ) {
    if ( u3_Host.ops_u.who_c != 0 ) {
      u3_Host.dir_c = strdup(1 + u3_Host.ops_u.who_c);
    }
    else {
      //  XX not sure how this might be reachable
      return c3n;
    }
  }
  else {
    {
      c3_c* ash_c;

      if ( (ash_c = strrchr(argv[optind], '/')) && (ash_c[1] == 0) ) {
        *ash_c = 0;
      }
    }

    u3_Host.dir_c = strdup(argv[optind]);
  }

  //  daemon mode (-d) implies disabling terminal assumptions (-t)
  //
  if ( c3y == u3_Host.ops_u.dem ) {
    u3_Host.ops_u.tem = c3y;
  }

  //  make -c optional, catch invalid boot of existing pier
  //
  {
    struct stat s;
    if ( 0 != stat(u3_Host.dir_c, &s) ) {
      if ( c3n == u3_Host.ops_u.nuu ) {
        u3_Host.ops_u.nuu = c3y;
      }
    }
    else if ( c3y == u3_Host.ops_u.nuu ) {
      fprintf(stderr, "tried to create, but %s already exists\n", u3_Host.dir_c);
      fprintf(stderr, "normal usage: %s %s\n", argv[0], u3_Host.dir_c);
      exit(1);
    }
    else if ( 0 != access(u3_Host.dir_c, W_OK) ) {
      fprintf(stderr, "urbit: write permissions are required for %s\n", u3_Host.dir_c);
      exit(1);
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

  struct sockaddr_in t;
  if ( u3_Host.ops_u.bin_c != 0 && inet_pton(AF_INET, u3_Host.ops_u.bin_c, &t.sin_addr) == 0 ) {
    fprintf(stderr, "-b invalid IP address\n");
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

  if ( u3_Host.ops_u.eth_c == 0 && imp_t ) {
    u3_Host.ops_u.eth_c = "http://eth-mainnet.urbit.org:8545";
  }

  if ( u3_Host.ops_u.url_c != 0 && u3_Host.ops_u.pil_c != 0 ) {
    fprintf(stderr, "-B and -u cannot be used together\n");
    return c3n;
  }
  else if ( u3_Host.ops_u.nuu == c3y
           && u3_Host.ops_u.url_c == 0
           && u3_Host.ops_u.git == c3n ) {
    u3_Host.ops_u.url_c =
      "https://bootstrap.urbit.org/urbit-v" URBIT_VERSION ".pill";
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

/* _cert_store: decoded CA certificates
 */
static STACK_OF(X509_INFO)* _cert_store;

/* _setup_cert_store(): decodes embedded CA certificates
 */
static void
_setup_cert_store()
{
  BIO* cbio = BIO_new_mem_buf(include_ca_bundle_crt, include_ca_bundle_crt_len);
  if ( !cbio || !(_cert_store = PEM_X509_INFO_read_bio(cbio, NULL, NULL, NULL)) ) {
    u3l_log("boot: failed to decode embedded CA certificates\r\n");
    exit(1);
  }

  BIO_free(cbio);
}

/* _setup_ssl_x509(): adds embedded CA certificates to a X509_STORE
 */
static void
_setup_ssl_x509(void* arg)
{
  X509_STORE* cts = arg;
  int i;
  for ( i = 0; i < sk_X509_INFO_num(_cert_store); i++ ) {
    X509_INFO *itmp = sk_X509_INFO_value(_cert_store, i);
    if(itmp->x509) {
      X509_STORE_add_cert(cts, itmp->x509);
    }
    if(itmp->crl) {
      X509_STORE_add_crl(cts, itmp->crl);
    }
  }
}

/* _curl_ssl_ctx_cb(): curl SSL context callback
 */
static CURLcode
_curl_ssl_ctx_cb(CURL* curl, SSL_CTX* sslctx, void* param)
{
  X509_STORE* cts = SSL_CTX_get_cert_store(sslctx);
  if (!cts || !_cert_store)
    return CURLE_ABORTED_BY_CALLBACK;

  _setup_ssl_x509(cts);
  return CURLE_OK;
}

/* _setup_ssl_curl(): adds embedded CA certificates to a curl context
 */
static void
_setup_ssl_curl(void* arg)
{
  CURL* curl = arg;
  curl_easy_setopt(curl, CURLOPT_CAINFO, NULL);
  curl_easy_setopt(curl, CURLOPT_CAPATH, NULL);
  curl_easy_setopt(curl, CURLOPT_SSL_CTX_FUNCTION, _curl_ssl_ctx_cb);
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
    "-A dir        Use dir for initial clay sync\n",
    "-B pill       Bootstrap from this pill\n",
    "-b ip         Bind HTTP server to this IP address\n",
    "-C limit      Set memo cache max size; 0 means uncapped\n",
    "-c pier       Create a new urbit in pier/\n",
    "-D            Recompute from events\n",
    "-d            Daemon mode; implies -t\n",
    "-e url        Ethereum gateway\n",
    "-F ship       Fake keys; also disables networking\n",
    "-G string     Private key string (see also -k)\n",
    "-g            Set GC flag\n",
    "-i jam_file   import pier state\n",
    "-j            Create json trace file in .urb/put/trace\n",
    "-K stage      Start at Hoon kernel version stage\n",
    "-k file-path  Private key file (see also -G)\n",
    "-L            local networking only\n",
    "-P            Profiling\n",
    "-p ames_port  Set the ames port to bind to\n",
    "-q            Quiet\n",
    "-R            Report urbit build info\n",
    "-S            Disable battery hashing\n",
    // XX find a way to re-enable
    // "-s            Pill URL from arvo git hash\n",
    "-t            Disable terminal/tty assumptions\n",
    "-u url        URL from which to download pill\n",
    "-v            Verbose\n",
    "-w name       Boot as ~name\n",
    "-X path       Scry, write to file, then exit\n"
    "-x            Exit immediately\n",
    "-Y file       Optional name of file (for -X and -o)\n"
    "-Z format     Optional file format ('jam', or aura, for -X)\n"
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
    "   %s <my-planet or my-comet> to restart an existing urbit\n",
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

static void
report(void)
{
  printf("urbit %s\n", URBIT_VERSION);
  printf("gmp: %s\n", gmp_version);
#if !defined(U3_OS_mingw)
  printf("sigsegv: %d.%d\n",
         (libsigsegv_version >> 8) & 0xff,
         libsigsegv_version & 0xff);
#endif
  printf("openssl: %s\n", SSLeay_version(SSLEAY_VERSION));
  printf("libuv: %s\n", uv_version_string());
  printf("libh2o: %d.%d.%d\n",
         H2O_LIBRARY_VERSION_MAJOR,
         H2O_LIBRARY_VERSION_MINOR,
         H2O_LIBRARY_VERSION_PATCH);
  printf("lmdb: %d.%d.%d\n",
         MDB_VERSION_MAJOR,
         MDB_VERSION_MINOR,
         MDB_VERSION_PATCH);
  printf("curl: %d.%d.%d\n",
         LIBCURL_VERSION_MAJOR,
         LIBCURL_VERSION_MINOR,
         LIBCURL_VERSION_PATCH);
}

/* _stop_exit(): exit immediately.
*/
static void
_stop_exit(c3_i int_i)
{
  //  explicit fprintf to avoid allocation in u3l_log
  //
  fprintf(stderr, "\r\n[received keyboard stop signal, exiting]\r\n");
  u3_king_bail();
}

/* _stop_on_boot_completed_cb(): exit gracefully after boot is complete
*/
static void
_stop_on_boot_completed_cb()
{
  u3_king_exit();
}

static c3_i
_debug_db_stats(const c3_c* dir_c)
{
#if defined(U3_CPU_aarch64) && defined(U3_OS_linux)
  const size_t siz_i = 64424509440;
#else
  const size_t siz_i = 1099511627776;
#endif

  c3_c* log_c = c3_malloc(10 + strlen(dir_c));

  strcpy(log_c, dir_c);
  strcat(log_c, "/.urb/log");

  MDB_env* mdb_u = u3_lmdb_init(log_c, siz_i);

  if ( mdb_u ) {
    u3_lmdb_stat(mdb_u, stdout);
    u3_lmdb_exit(mdb_u);
    return 0;
  }
  else {
    return 1;
  }
}

c3_i
main(c3_i   argc,
     c3_c** argv)
{
  //  Parse options.
  //
  if ( c3n == _main_getopt(argc, argv) ) {
    if (  (3 == argc)
       && (0 == strcmp("db-info", argv[1])) )
    {
      return _debug_db_stats(argv[2]);
    }

    u3_ve_usage(argc, argv);
    return 1;
  }

  //  Set `u3_Host.wrk_c` to the worker executable path.
  c3_i urbit_exe_len = strlen(argv[0]);
  c3_i worker_exe_len = 1 + urbit_exe_len + strlen("-worker");
  u3_Host.wrk_c = c3_malloc(worker_exe_len);
  #if defined(U3_OS_mingw)
  if ( urbit_exe_len >= 4 && !strcmp(argv[0] + urbit_exe_len - 4, ".exe")) {
    snprintf(u3_Host.wrk_c, worker_exe_len, "%.*s-worker.exe", urbit_exe_len - 4, argv[0]);
  } else {
    snprintf(u3_Host.wrk_c, worker_exe_len, "%s-worker", argv[0]);
  }
  #else
  snprintf(u3_Host.wrk_c, worker_exe_len, "%s-worker", argv[0]);
  #endif

  if ( c3y == u3_Host.ops_u.dem ) {
    //  In daemon mode, run the urbit as a background process, but don't
    //  exit from the parent process until the ship is finished booting.
    //
    u3_daemon_init();
  }

  if ( c3y == u3_Host.ops_u.rep ) {
    report();
    return 0;
  }

  if ( c3y == u3_Host.ops_u.tex ) {
    u3_Host.bot_f = _stop_on_boot_completed_cb;
  }

#if 0
  if ( 0 == getuid() ) {
    chroot(u3_Host.dir_c);
    u3_Host.dir_c = "/";
  }
#endif
  u3_ve_sysopt();

  //  Block profiling signal, which should be delivered to exactly one thread.
  //
  //    XX review, may be unnecessary due to similar in u3m_init()
  //
#if defined(U3_OS_PROF)
  if ( _(u3_Host.ops_u.pro) ) {
    sigset_t set;

    sigemptyset(&set);
    sigaddset(&set, SIGPROF);
    if ( 0 != pthread_sigmask(SIG_BLOCK, &set, NULL) ) {
      u3l_log("boot: thread mask SIGPROF: %s\r\n", strerror(errno));
      exit(1);
    }
  }
#endif

  #if !defined(U3_OS_mingw)
  //  Handle SIGTSTP as if it was SIGTERM.
  //
  //    Configured here using signal() so as to be immediately available.
  //
  signal(SIGTSTP, _stop_exit);
  #endif

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
      c3_free(abs_c);
      mprint_i *= 2;
      abs_c = c3_malloc(mprint_i);
    }
    printf("boot: home is %s/%s\n", abs_c, u3_Host.dir_c);
    c3_free(abs_c);
  } else {
    printf("boot: home is %s\n", abs_c);
    c3_free(abs_c);
  }
  // printf("vere: hostname is %s\n", u3_Host.ops_u.nam_c);

  if ( c3y == u3_Host.ops_u.dem ) {
    printf("boot: running as daemon\n");
  }

  //  Instantiate process globals.
  {
    /*  Boot the image and checkpoint.  Set flags.
    */
    {
      /*  Set pier directory.
      */
      u3C.dir_c = u3_Host.dir_c;

      /*  Logging that doesn't interfere with console output.
      */
      u3C.stderr_log_f = u3_term_io_log;

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
      **
      **    XX also exit immediately?
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
        u3_Host.tra_u.fun_w = 0;
      }
    }

    #if defined(U3_OS_mingw)
    //  Initialize event used to transmit Ctrl-C to worker process
    //
    {
      SECURITY_ATTRIBUTES sa = {sizeof(sa), NULL, TRUE};
      if ( NULL == (u3_Host.cev_u = CreateEvent(&sa, FALSE, FALSE, NULL)) ) {
        u3l_log("boot: failed to create Ctrl-C event: %d\r\n", GetLastError());
        exit(1);
      }
    }
    #endif

    //  starting u3m configures OpenSSL memory functions, so we must do it
    //  before any OpenSSL allocations
    // 
    u3m_boot_lite();

    //  Initialize OpenSSL for client and server
    //
    {
      SSL_library_init();
      SSL_load_error_strings();
    }

    //  initialize curl
    //
    if ( 0 != curl_global_init(CURL_GLOBAL_DEFAULT) ) {
      u3l_log("boot: curl initialization failed\r\n");
      exit(1);
    }

    _setup_cert_store();
    u3K.ssl_curl_f = _setup_ssl_curl;
    u3K.ssl_x509_f = _setup_ssl_x509;

    u3_king_commence();

    //  uninitialize curl
    //
    curl_global_cleanup();

    //  uninitialize OpenSSL
    //
    //    see https://wiki.openssl.org/index.php/Library_Initialization
    //
    {
      ENGINE_cleanup();
      CONF_modules_unload(1);
      EVP_cleanup();
      CRYPTO_cleanup_all_ex_data();
      SSL_COMP_free_compression_methods();
      ERR_free_strings();
    }
  }

  return 0;
}
