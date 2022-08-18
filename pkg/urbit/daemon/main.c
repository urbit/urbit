/* vere/main.c
**
*/
#define U3_GLOBAL
#define C3_GLOBAL
#include "all.h"
#include "vere/ivory.h"
#include "ur/ur.h"
#include "rsignal.h"
#include <vere/serf.h>
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
#include <getopt.h>
#include <libgen.h>

#include "ca-bundle.h"
#include "whereami.h"

//  serf module state
//
static u3_serf        u3V;             //  one serf per process
static u3_moat      inn_u;             //  input stream
static u3_mojo      out_u;             //  output stream
static u3_cue_xeno* sil_u;             //  cue handle

#undef SERF_TRACE_JAM
#undef SERF_TRACE_CUE

/* Require unsigned char
 */
STATIC_ASSERT(( 0 == CHAR_MIN && UCHAR_MAX == CHAR_MAX ),
              "unsigned char required");

/* _main_self_path(): get binary self-path.
*/
static void
_main_self_path(void)
{
  c3_c* pat_c;
  c3_i  len_i, pat_i;

  if ( 0 < (len_i = wai_getExecutablePath(NULL, 0, &pat_i)) ) {
    pat_c = c3_malloc( 1 + len_i );
    wai_getExecutablePath(pat_c, len_i, &pat_i);
    pat_c[len_i] = 0;

    u3_Host.dem_c = pat_c;
  }
  else {
    fprintf(stderr, "unable to get binary self path\r\n");
    exit(1);

    //  XX continue?
    //
    // u3_Host.dem_c = strdup(bin_c);
  }
}

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

/* _main_repath(): canonicalize path, using dirname if needed.
*/
c3_c*
_main_repath(c3_c* pax_c)
{
  c3_c* rel_c;
  c3_c* fas_c;
  c3_c* dir_c;
  c3_w  len_w;
  c3_i  wit_i;

  c3_assert(pax_c);
  if ( 0 != (rel_c = realpath(pax_c, 0)) ) {
    return rel_c;
  }
  fas_c = strrchr(pax_c, '/');
  if ( !fas_c ) {
    c3_c rec_c[2048];

    wit_i = snprintf(rec_c, sizeof(rec_c), "./%s", pax_c);
    c3_assert(sizeof(rec_c) > wit_i);
    return _main_repath(rec_c);
  }
  c3_assert(u3_unix_cane(fas_c + 1));
  *fas_c = 0;
  dir_c = realpath(pax_c, 0);
  *fas_c = '/';
  if ( 0 == dir_c ) {
    return 0;
  }
  len_w = strlen(dir_c) + strlen(fas_c) + 1;
  rel_c = c3_malloc(len_w);
  wit_i = snprintf(rel_c, len_w, "%s%s", dir_c, fas_c);
  c3_assert(len_w == wit_i + 1);
  c3_free(dir_c);
  return rel_c;
}

/* _main_init(): initialize globals
*/
static void
_main_init(void)
{
  u3_Host.nex_o = c3n;
  u3_Host.pep_o = c3n;

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
}

/* _main_pier_run(): get pier from binary path (argv[0]), if appropriate
*/
static c3_c*
_main_pier_run(c3_c* bin_c)
{
  c3_c* dir_c = 0;
  c3_w  bin_w = strlen(bin_c);
  c3_w  len_w = strlen(U3_BIN_ALIAS);

  //  no args, argv[0] == $pier/.run
  //
  if (  (len_w <= bin_w)
     && (0 == strcmp(bin_c + (bin_w - len_w), U3_BIN_ALIAS)) )
  {
    bin_c = strdup(bin_c); // dirname can modify
    dir_c = _main_repath(dirname(bin_c));
    c3_free(bin_c);
  }

  return dir_c;
}

/* _main_getopt(): extract option map from command line.
*/
static u3_noun
_main_getopt(c3_i argc, c3_c** argv)
{
  c3_i ch_i, lid_i;
  c3_w arg_w;

  static struct option lop_u[] = {
    { "arvo",                required_argument, NULL, 'A' },
    { "abort",               no_argument,       NULL, 'a' },
    { "bootstrap",           required_argument, NULL, 'B' },
    { "http-ip",             required_argument, NULL, 'b' },
    { "memo-cache-limit",    required_argument, NULL, 'C' },
    { "pier",                required_argument, NULL, 'c' },
    { "replay",              no_argument,       NULL, 'D' },
    { "daemon",              no_argument,       NULL, 'd' },
    { "ethereum",            required_argument, NULL, 'e' },
    { "fake",                required_argument, NULL, 'F' },
    { "key-string",          required_argument, NULL, 'G' },
    { "gc",                  no_argument,       NULL, 'g' },
    { "dns-root",            required_argument, NULL, 'H' },
    { "inject",              required_argument, NULL, 'I' },
    { "import",              required_argument, NULL, 'i' },
    { "ivory-pill",          required_argument, NULL, 'J' },
    { "json-trace",          no_argument,       NULL, 'j' },
    { "kernel-stage",        required_argument, NULL, 'K' },
    { "key-file",            required_argument, NULL, 'k' },
    { "local",               no_argument,       NULL, 'L' },
    { "lite-boot",           no_argument,       NULL, 'l' },
    { "replay-to",           required_argument, NULL, 'n' },
    { "profile",             no_argument,       NULL, 'P' },
    { "ames-port",           required_argument, NULL, 'p' },
    { "http-port",           required_argument, NULL, c3__http },
    { "https-port",          required_argument, NULL, c3__htls },
    { "no-conn",             no_argument,       NULL, c3__noco },
    { "no-dock",             no_argument,       NULL, c3__nodo },
    { "quiet",               no_argument,       NULL, 'q' },
    { "versions",            no_argument,       NULL, 'R' },
    { "replay-from",         required_argument, NULL, 'r' },
    { "skip-battery-hashes", no_argument,       NULL, 'S' },
    { "autoselect-pill",     no_argument,       NULL, 's' },
    { "no-tty",              no_argument,       NULL, 't' },
    { "bootstrap-url",       required_argument, NULL, 'u' },
    { "verbose",             no_argument,       NULL, 'v' },
    { "name",                required_argument, NULL, 'w' },
    { "scry",                required_argument, NULL, 'X' },
    { "exit",                no_argument,       NULL, 'x' },
    { "scry-into",           required_argument, NULL, 'Y' },
    { "scry-format",         required_argument, NULL, 'Z' },
    { NULL, 0, NULL, 0 },
  };

  while ( -1 != (ch_i=getopt_long(argc, argv,
                 "A:B:C:DF:G:H:I:J:K:LPRSX:Y:Z:ab:cde:gi:jk:ln:p:qr:stu:vw:x",
                 lop_u, &lid_i)) )
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
        u3_Host.ops_u.lit_c = _main_repath(optarg);
        break;
      }
      case 'B': {
        u3_Host.ops_u.pil_c = _main_repath(optarg);
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
        u3_Host.ops_u.arv_c = _main_repath(optarg);
        break;
      }
      case 'H': {
        u3_Host.ops_u.dns_c = strdup(optarg);
        break;
      }
      case 'I': {
        u3_Host.ops_u.jin_c = _main_repath(optarg);
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
        u3_Host.ops_u.key_c = _main_repath(optarg);
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
      case c3__http: {
        if ( c3n == _main_readw(optarg, 65536, &arg_w) ) {
          return c3n;
        } else u3_Host.ops_u.per_s = arg_w;
        break;
      }
      case c3__htls: {
        if ( c3n == _main_readw(optarg, 65536, &arg_w) ) {
          return c3n;
        } else u3_Host.ops_u.pes_s = arg_w;
        break;
      }
      case c3__noco: {
        u3_Host.ops_u.con = c3n;
        break;
      }
      case c3__nodo: {
        u3_Host.ops_u.doc = c3n;
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
        u3_Host.ops_u.imp_c = _main_repath(optarg);
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
    //  no trailing positional arg, argv[0] != $pier/.run, invalid command
    //
    else  if ( !(u3_Host.dir_c = _main_pier_run(argv[0])) ) {
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

    u3_Host.dir_c = _main_repath(argv[optind]);
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

/* _cw_usage(): print utility usage.
*/
static void
_cw_usage(c3_c* bin_c)
{
  c3_c *use_c[] = {
    "utilities:\n",
    "  %s cram %.*s              jam state:\n",
    "  %s dock %.*s              copy binary:\n",
    "  %s grab %.*s              measure memory usage:\n",
    "  %s info %.*s              print pier info:\n",
    "  %s meld %.*s              deduplicate snapshot:\n",
    "  %s pack %.*s              defragment snapshot:\n",
    "  %s prep %.*s              prepare for upgrade:\n",
    "  %s next %.*s              request upgrade:\n",
    "  %s queu %.*s<at-event>    cue state:\n",
    "  %s vere ARGS <output dir>    download binary:\n",
    "\n  run as a 'serf':\n",
    "    %s serf <pier> <key> <flags> <cache-size> <at-event>"
#ifdef U3_OS_mingw
    " <ctrlc-handle>"
#endif
    "\n",
    0
  };

  c3_c* d = _main_pier_run(bin_c);
  c3_i  i;

  for ( i=0; use_c[i]; i++ ) {
    fprintf(stderr, use_c[i], bin_c, d ? 0 : 7, "<pier> ");
  }

  c3_free(d);
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
    "-A, --arvo DIR                Use dir for initial clay sync\n",
    "-a, --abort                   Abort aggressively\n",
    "-B, --bootstrap PILL          Bootstrap from this pill\n",
    "-b, --http-ip IP              Bind HTTP server to this IP address\n",
    "-C, --memo-cache-limit LIMIT  Set memo cache max size; 0 means uncapped\n",
    "-c, --pier PIER               Create a new urbit in pier/\n",
    "-D, --replay                  Recompute from events\n",
    "-d, --daemon                  Daemon mode; implies -t\n",
    "-e, --ethereum URL            Ethereum gateway\n",
    "-F, --fake SHIP               Fake keys; also disables networking\n",
    "-G, --key-string STRING       Private key string (@uw, see also -k)\n"
    "-g, --gc                      Set GC flag\n",
    "-I, --inject FILE             Inject event from jamfile\n",
    "-i, --import FILE             Import pier state from jamfile\n",
    "-J, --ivory-pill PILL         Use custom ivory pill\n",
    "-j, --json-trace              Create json trace file in .urb/put/trace\n",
    "-K, --kernel-stage STAGE      Start at Hoon kernel version stage\n",
    "-k, --key-file KEYS           Private key file (see also -G)\n",
    "-L, --local                   Local networking only\n",
    "-l, --lite-boot               Most-minimal startup\n",
    "-n, --replay-to NUMBER        Replay up to event\n",
    "-P, --profile                 Profiling\n",
    "-p, --ames-port PORT          Set the ames port to bind to\n",
    "    --http-port PORT          Set the http port to bind to\n",
    "    --https-port PORT         Set the https port to bind to\n",
    "-q, --quiet                   Quiet\n",
    "-R, --versions                Report urbit build info\n",
    "-r, --replay-from NUMBER      Load snapshot from event\n",
    "-S, --skip-battery-hashes     Disable battery hashing\n",
    // XX find a way to re-enable
    // "-s, --autoselect-pill      Pill URL from arvo git hash\n",
    "-t, --no-tty                  Disable terminal/tty assumptions\n",
    "-u, --bootstrap-url URL       URL from which to download pill\n",
    "-v, --verbose                 Verbose\n",
    "-w, --name NAME               Boot as ~name\n",
    "-X, --scry PATH               Scry, write to file, then exit\n",
    "-x, --exit                    Exit immediately\n",
    "-Y, --scry-into FILE          Optional name of file (for -X)\n",
    "-Z, --scry-format FORMAT      Optional file format ('jam', or aura, for -X)\n",
    "    --no-conn                 Do not run control plane\n",
    "    --no-dock                 Skip binary \"docking\" on boot\n",
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
  _cw_usage(argv[0]);
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

/* _cw_serf_fail(): failure stub.
*/
static void
_cw_serf_fail(void* ptr_v, ssize_t err_i, const c3_c* err_c)
{
  if ( UV_EOF == err_i ) {
    fprintf(stderr, "serf: pier unexpectedly shut down\r\n");
  }
  else {
    fprintf(stderr, "serf: pier error: %s\r\n", err_c);
  }

  exit(1);
}

/* _cw_serf_send(): send plea back to daemon.
*/
static void
_cw_serf_send(u3_noun pel)
{
  c3_d  len_d;
  c3_y* byt_y;

#ifdef SERF_TRACE_JAM
  u3t_event_trace("serf ipc jam", 'B');
#endif

  u3s_jam_xeno(pel, &len_d, &byt_y);

#ifdef SERF_TRACE_JAM
  u3t_event_trace("serf ipc jam", 'E');
#endif

  u3_newt_send(&out_u, len_d, byt_y);
  u3z(pel);
}

/* _cw_serf_send_slog(): send hint output (hod is [priority tank]).
*/
static void
_cw_serf_send_slog(u3_noun hod)
{
  _cw_serf_send(u3nc(c3__slog, hod));
}

/* _cw_serf_send_stdr(): send stderr output (%flog)
*/
static void
_cw_serf_send_stdr(c3_c* str_c)
{
  _cw_serf_send(u3nc(c3__flog, u3i_string(str_c)));
}


/* _cw_serf_step_trace(): initialize or rotate trace file.
*/
static void
_cw_serf_step_trace(void)
{
  if ( u3C.wag_w & u3o_trace ) {
    if ( u3_Host.tra_u.con_w == 0  && u3_Host.tra_u.fun_w == 0 ) {
      u3t_trace_open(u3V.dir_c);
    }
    else if ( u3_Host.tra_u.con_w >= 100000 ) {
      u3t_trace_close();
      u3t_trace_open(u3V.dir_c);
    }
  }
}

/* _cw_serf_writ(): process a command from the king.
*/
static void
_cw_serf_writ(void* vod_p, c3_d len_d, c3_y* byt_y)
{
  u3_weak jar;
  u3_noun ret;

  _cw_serf_step_trace();

#ifdef SERF_TRACE_CUE
  u3t_event_trace("serf ipc cue", 'B');
#endif

  jar = u3s_cue_xeno_with(sil_u, len_d, byt_y);

#ifdef SERF_TRACE_CUE
  u3t_event_trace("serf ipc cue", 'E');
#endif

  if (  (u3_none == jar)
     || (c3n == u3_serf_writ(&u3V, jar, &ret)) )
  {
    _cw_serf_fail(0, -1, "bad jar");
  }
  else {
    _cw_serf_send(ret);

    //  all references must now be counted, and all roots recorded
    //
    u3_serf_post(&u3V);
  }
}

/* _cw_serf_stdio(): fix up std io handles
*/
static void
_cw_serf_stdio(c3_i* inn_i, c3_i* out_i)
{
  //  the serf is spawned with [FD 0] = events and [FD 1] = effects
  //  we dup [FD 0 & 1] so we don't accidently use them for something else
  //  we replace [FD 0] (stdin) with a fd pointing to /dev/null
  //  we replace [FD 1] (stdout) with a dup of [FD 2] (stderr)
  //
  c3_i nul_i = c3_open(c3_dev_null, O_RDWR, 0);

  *inn_i = dup(0);
  *out_i = dup(1);

  dup2(nul_i, 0);
  dup2(2, 1);

  close(nul_i);

  //  set stream I/O to unbuffered because it's now a pipe not a console
  //
  setvbuf(stdout, NULL, _IONBF, 0);
  setvbuf(stderr, NULL, _IONBF, 0);
}

/* _cw_serf_stdio(): cleanup on serf exit.
*/
static void
_cw_serf_exit(void)
{
  u3s_cue_xeno_done(sil_u);
  u3t_trace_close();
}

/* _cw_init_io(): initialize i/o streams.
*/
static void
_cw_init_io(uv_loop_t* lup_u)
{
  //  mars is spawned with [FD 0] = events and [FD 1] = effects
  //  we dup [FD 0 & 1] so we don't accidently use them for something else
  //  we replace [FD 0] (stdin) with a fd pointing to /dev/null
  //  we replace [FD 1] (stdout) with a dup of [FD 2] (stderr)
  //
  c3_i nul_i = c3_open(c3_dev_null, O_RDWR, 0);
  c3_i inn_i = dup(0);
  c3_i out_i = dup(1);

  dup2(nul_i, 0);
  dup2(2, 1);

  close(nul_i);

  //  set stream I/O to unbuffered because it's now a pipe not a console
  //
  setvbuf(stdout, NULL, _IONBF, 0);
  setvbuf(stderr, NULL, _IONBF, 0);

  //  Ignore SIGPIPE signals.
  //
#ifndef U3_OS_mingw
  {
    struct sigaction sig_s = {{0}};
    sigemptyset(&(sig_s.sa_mask));
    sig_s.sa_handler = SIG_IGN;
    sigaction(SIGPIPE, &sig_s, 0);
  }
#endif

  //  configure pipe to daemon process
  //
  {
    c3_i err_i;
    err_i = uv_timer_init(lup_u, &inn_u.tim_u);
    c3_assert(!err_i);
    err_i = uv_pipe_init(lup_u, &inn_u.pyp_u, 0);
    c3_assert(!err_i);
    uv_pipe_open(&inn_u.pyp_u, inn_i);
    err_i = uv_pipe_init(lup_u, &out_u.pyp_u, 0);
    c3_assert(!err_i);
    uv_pipe_open(&out_u.pyp_u, out_i);

    uv_stream_set_blocking((uv_stream_t*)&out_u.pyp_u, 1);
  }
}

#ifdef U3_OS_mingw
/* _cw_intr_win_cb(): invoked when urth signals ctrl-c.
*/
static void
_cw_intr_win_cb(PVOID param, BOOLEAN timedOut)
{
  rsignal_raise(SIGINT);
}

/* _cw_intr_win(): initialize ctrl-c handling.
*/
static void
_cw_intr_win(c3_c* han_c)
{
  HANDLE h;
  if ( 1 != sscanf(han_c, "%" PRIu64, &h) ) {
    fprintf(stderr, "mars: ctrl-c event: bad handle %s: %s\r\n",
            han_c, strerror(errno));
  }
  else {
    if ( !RegisterWaitForSingleObject(&h, h, _cw_intr_win_cb,
                                      NULL, INFINITE, 0) )
    {
      fprintf(stderr,
        "mars: ctrl-c event: RegisterWaitForSingleObject(%u) failed (%d)\r\n",
        h, GetLastError());
    }
  }
}
#endif

/* _cw_eval_commence(): initialize and run the hoon evaluator
*/
static void
_cw_eval_commence(c3_i argc, c3_c* argv[])
{
  //Read from stdin until an EOF is recieved
  c3_c* line = NULL;
  size_t len = 0;
  ssize_t nread;
  c3_c* evl_c = "";

  while((nread = getline(&line, &len, stdin)) != -1)
  {
     len = asprintf(&evl_c, "%s%s", evl_c, line);
  }

  free(line);

  //printf("%s", evl_c);

  //Initialize the Loom and load the Ivory Pill
  c3_d          len_d = u3_Ivory_pill_len;
  c3_y*         byt_y = u3_Ivory_pill;
  u3_cue_xeno*  sil_u;
  u3_weak       pil;

  u3C.wag_w |= u3o_hashless;
  u3m_boot_lite();
  sil_u = u3s_cue_xeno_init_with(ur_fib27, ur_fib28);
  if ( u3_none == (pil = u3s_cue_xeno_with(sil_u, len_d, byt_y)) ) {
    printf("*** fail _setup 1\n");
    exit(1);
  }
  u3s_cue_xeno_done(sil_u);
  if ( c3n == u3v_boot_lite(pil) ) {
    printf("*** fail _setup 2\n");
    exit(1);
  }

  printf("Executing hoon:\n");

  //Run the input through a virtualization (u3v_wish_n) and get the ouput
  //u3_noun res = u3m_soft(0, u3v_wish_n, u3i_string(evl_c));
  u3_noun gat = u3v_wish("|=(a=@t (sell (slap !>(+>.$) (rain /eval a))))");
  u3_noun res;
  {
    u3_noun sam = u3i_string(evl_c);
    u3_noun cor = u3nc(u3k(u3h(gat)), u3nc(sam, u3k(u3t(u3t(gat)))));
    res = u3m_soft(0, u3n_kick_on, cor); // Transfer cor and sam
  }



  
  if(0 == u3h(res)){//Succuessful execution print the output
     u3_pier_tank(0,0,u3k(u3t(res)));
  }else{
     u3_pier_punt_goof("Hoon Error", u3k(res)); //print stack trace error
  }
  u3z(res);
  u3z(gat);

  //printf("finishing\n");
}



/* _cw_serf_commence(): initialize and run serf
*/
static void
_cw_serf_commence(c3_i argc, c3_c* argv[])
{
#ifdef U3_OS_mingw
  if ( 8 > argc ) {
#else
  if ( 7 > argc ) {
#endif
    fprintf(stderr, "serf: missing args\n");
    exit(1);
  }

  c3_d       eve_d = 0;
  uv_loop_t* lup_u = u3_Host.lup_u = uv_default_loop();
  c3_c*      dir_c = argv[2];
  c3_c*      key_c = argv[3]; // XX use passkey
  c3_c*      wag_c = argv[4];
  c3_c*      hap_c = argv[5];
  c3_c*      eve_c = argv[6];
#ifdef U3_OS_mingw
  c3_c*      han_c = argv[7];
  _cw_intr_win(han_c);
#endif

  _cw_init_io(lup_u);

  memset(&u3V, 0, sizeof(u3V));
  memset(&u3_Host.tra_u, 0, sizeof(u3_Host.tra_u));

  //  load passkey
  //
  //    XX and then ... use passkey
  //
  {
    sscanf(key_c, "%" PRIx64 ":%" PRIx64 ":%" PRIx64 ":%" PRIx64,
                  &u3V.key_d[0],
                  &u3V.key_d[1],
                  &u3V.key_d[2],
                  &u3V.key_d[3]);
  }

  //  load runtime config
  //
  {
    sscanf(wag_c, "%" SCNu32, &u3C.wag_w);
    sscanf(hap_c, "%" SCNu32, &u3_Host.ops_u.hap_w);

    if ( 1 != sscanf(eve_c, "%" PRIu64, &eve_d) ) {
      fprintf(stderr, "serf: rock: invalid number '%s'\r\n", argv[4]);
    }
  }

  sil_u = u3s_cue_xeno_init();

  //  set up writing
  //
  out_u.ptr_v = &u3V;
  out_u.bal_f = _cw_serf_fail;

  //  set up reading
  //
  inn_u.ptr_v = &u3V;
  inn_u.pok_f = _cw_serf_writ;
  inn_u.bal_f = _cw_serf_fail;

  //  setup loom
  //
  {
    u3V.dir_c = strdup(dir_c);
    u3V.sen_d = u3V.dun_d = u3m_boot(dir_c);

    if ( eve_d ) {
      //  XX need not be fatal, need a u3m_reboot equivalent
      //  XX can spuriously fail do to corrupt memory-image checkpoint,
      //  need a u3m_half_boot equivalent
      //  workaround is to delete/move the checkpoint in case of corruption
      //
      if ( c3n == u3u_uncram(u3V.dir_c, eve_d) ) {
        fprintf(stderr, "serf (%" PRIu64 "): rock load failed\r\n", eve_d);
        exit(1);
      }
    }
  }

  //  set up logging
  //
  //    XX must be after u3m_boot due to u3l_log
  //
  {
    u3C.stderr_log_f = _cw_serf_send_stdr;
    u3C.slog_f = _cw_serf_send_slog;
  }

  u3V.xit_f = _cw_serf_exit;

#if defined(SERF_TRACE_JAM) || defined(SERF_TRACE_CUE)
  u3t_trace_open(u3V.dir_c);
#endif

  //  start serf
  //
  {
    _cw_serf_send(u3_serf_init(&u3V));
  }

  //  start reading
  //
  u3_newt_read_sync(&inn_u);

  //  enter loop
  //
  uv_run(lup_u, UV_RUN_DEFAULT);
  u3m_stop();
}

/* _cw_disk_init(): open event log
*/
static u3_disk*
_cw_disk_init(c3_c* dir_c)
{
  u3_disk_cb cb_u = {0};
  u3_disk*  log_u = u3_disk_init(dir_c, cb_u);

  if ( !log_u ) {
    fprintf(stderr, "unable to open event log\n");
    exit(1);
  }

  return log_u;
}

/* _cw_dock(): copy binary into pier
*/
static void
_cw_dock(c3_i argc, c3_c* argv[])
{
  switch ( argc ) {
    case 2: {
      if ( !(u3_Host.dir_c = _main_pier_run(argv[0])) ) {
        fprintf(stderr, "unable to find pier\r\n");
        exit (1);
      }
    } break;

    case 3: {
      u3_Host.dir_c = argv[2];
    } break;

    default: {
      fprintf(stderr, "invalid command\r\n");
      exit(1);
    } break;
  }

  _main_self_path();

  u3_king_dock(U3_VERE_PACE);
}

/* _cw_info(): print pier info
*/
static void
_cw_info(c3_i argc, c3_c* argv[])
{
  switch ( argc ) {
    case 2: {
      if ( !(u3_Host.dir_c = _main_pier_run(argv[0])) ) {
        fprintf(stderr, "unable to find pier\r\n");
        exit (1);
      }
    } break;

    case 3: {
      u3_Host.dir_c = argv[2];
    } break;

    default: {
      fprintf(stderr, "invalid command\r\n");
      exit(1);
    } break;
  }

  c3_d     eve_d = u3m_boot(u3_Host.dir_c);
  u3_disk* log_u = _cw_disk_init(u3_Host.dir_c);

  fprintf(stderr, "\r\nurbit: %s at event %" PRIu64 "\r\n",
                  u3_Host.dir_c, eve_d);

  u3_disk_slog(log_u);
  printf("\n");
  u3_lmdb_stat(log_u->mdb_u, stdout);
  u3_disk_exit(log_u);

  u3m_stop();
}

/* _cw_grab(): gc pier.
*/
static void
_cw_grab(c3_i argc, c3_c* argv[])
{
  switch ( argc ) {
    case 2: {
      if ( !(u3_Host.dir_c = _main_pier_run(argv[0])) ) {
        fprintf(stderr, "unable to find pier\r\n");
        exit (1);
      }
    } break;

    case 3: {
      u3_Host.dir_c = argv[2];
    } break;

    default: {
      fprintf(stderr, "invalid command\r\n");
      exit(1);
    } break;
  }

  u3m_boot(u3_Host.dir_c);
  u3C.wag_w |= u3o_hashless;
  u3_serf_grab();
  u3m_stop();
}

/* _cw_cram(): jam persistent state (rock), and exit.
*/
static void
_cw_cram(c3_i argc, c3_c* argv[])
{
  switch ( argc ) {
    case 2: {
      if ( !(u3_Host.dir_c = _main_pier_run(argv[0])) ) {
        fprintf(stderr, "unable to find pier\r\n");
        exit (1);
      }
    } break;

    case 3: {
      u3_Host.dir_c = argv[2];
    } break;

    default: {
      fprintf(stderr, "invalid command\r\n");
      exit(1);
    } break;
  }

  c3_d     eve_d = u3m_boot(u3_Host.dir_c);
  u3_disk* log_u = _cw_disk_init(u3_Host.dir_c); // XX s/b try_aquire lock
  c3_o  ret_o;

  fprintf(stderr, "urbit: cram: preparing\r\n");

  if ( c3n == (ret_o = u3u_cram(u3_Host.dir_c, eve_d)) ) {
    fprintf(stderr, "urbit: cram: unable to jam state\r\n");
  }
  else {
    fprintf(stderr, "urbit: cram: rock saved at event %" PRIu64 "\r\n", eve_d);
  }

  //  save even on failure, as we just did all the work of deduplication
  //
  u3e_save();
  u3_disk_exit(log_u);

  if ( c3n == ret_o ) {
    exit(1);
  }

  u3m_stop();
}

/* _cw_queu(): cue rock, save, and exit.
*/
static void
_cw_queu(c3_i argc, c3_c* argv[])
{
  c3_c* eve_c;
  c3_d  eve_d;

  switch ( argc ) {
    case 3: {
      if ( !(u3_Host.dir_c = _main_pier_run(argv[0])) ) {
        fprintf(stderr, "unable to find pier\r\n");
        exit (1);
      }
      eve_c = argv[2];
    } break;

    case 4: {
      u3_Host.dir_c = argv[2];
      eve_c         = argv[3];
    } break;

    default: {
      fprintf(stderr, "invalid command\r\n");
      exit(1);
    } break;
  }

  if ( 1 != sscanf(eve_c, "%" PRIu64 "", &eve_d) ) {
    fprintf(stderr, "urbit: queu: invalid number '%s'\r\n", eve_c);
    exit(1);
  }
  else {
    u3_disk* log_u = _cw_disk_init(u3_Host.dir_c); // XX s/b try_aquire lock

    fprintf(stderr, "urbit: queu: preparing\r\n");

    u3m_boot(u3_Host.dir_c);

    //  XX can spuriously fail do to corrupt memory-image checkpoint,
    //  need a u3m_half_boot equivalent
    //  workaround is to delete/move the checkpoint in case of corruption
    //
    if ( c3n == u3u_uncram(u3_Host.dir_c, eve_d) ) {
      fprintf(stderr, "urbit: queu: failed\r\n");
      exit(1);
    }

    u3e_save();
    u3_disk_exit(log_u);

    fprintf(stderr, "urbit: queu: rock loaded at event %" PRIu64 "\r\n", eve_d);
    u3m_stop();
  }
}

/* _cw_uniq(): deduplicate persistent nouns
*/
static void
_cw_meld(c3_i argc, c3_c* argv[])
{
  switch ( argc ) {
    case 2: {
      if ( !(u3_Host.dir_c = _main_pier_run(argv[0])) ) {
        fprintf(stderr, "unable to find pier\r\n");
        exit (1);
      }
    } break;

    case 3: {
      u3_Host.dir_c = argv[2];
    } break;

    default: {
      fprintf(stderr, "invalid command\r\n");
      exit(1);
    } break;
  }

  u3_disk* log_u = _cw_disk_init(u3_Host.dir_c); // XX s/b try_aquire lock
  c3_w     pre_w;

  u3C.wag_w |= u3o_hashless;
  u3m_boot(u3_Host.dir_c);

  pre_w = u3a_open(u3R);
  u3u_meld();
  u3a_print_memory(stderr, "urbit: meld: gained", (u3a_open(u3R) - pre_w));

  u3e_save();
  u3_disk_exit(log_u);
  u3m_stop();
}

/* _cw_next(): request upgrade
*/
static void
_cw_next(c3_i argc, c3_c* argv[])
{
  c3_i ch_i, lid_i;
  c3_w arg_w;

  static struct option lop_u[] = {
    { "arch",                required_argument, NULL, 'a' },
    { NULL, 0, NULL, 0 }
  };

  u3_Host.dir_c = _main_pier_run(argv[0]);

  while ( -1 != (ch_i=getopt_long(argc, argv, "a:", lop_u, &lid_i)) ) {
    switch ( ch_i ) {
      case 'a': {
        u3_Host.arc_c = strdup(optarg);
      } break;

      case '?': {
        exit(1);
      } break;
    }
  }

  //  argv[optind] is always "next"
  //

  if ( !u3_Host.dir_c ) {
    if ( optind + 1 < argc ) {
      u3_Host.dir_c = argv[optind + 1];
    }
    else {
      fprintf(stderr, "invalid command, pier required\r\n");
      exit(1);
    }

    optind++;
  }

  if ( optind + 1 != argc ) {
    fprintf(stderr, "invalid command\r\n");
    exit(1);
  }

  u3_Host.pep_o = c3y;
  u3_Host.nex_o = c3y;
}

/* _cw_pack(): compact memory, save, and exit.
*/
static void
_cw_pack(c3_i argc, c3_c* argv[])
{
  switch ( argc ) {
    case 2: {
      if ( !(u3_Host.dir_c = _main_pier_run(argv[0])) ) {
        fprintf(stderr, "unable to find pier\r\n");
        exit (1);
      }
    } break;

    case 3: {
      u3_Host.dir_c = argv[2];
    } break;

    default: {
      fprintf(stderr, "invalid command\r\n");
      exit(1);
    } break;
  }

  u3_disk* log_u = _cw_disk_init(u3_Host.dir_c); // XX s/b try_aquire lock

  u3m_boot(u3_Host.dir_c);
  u3a_print_memory(stderr, "urbit: pack: gained", u3m_pack());

  u3e_save();
  u3_disk_exit(log_u);
  u3m_stop();
}

/* _cw_prep(): prepare for upgrade
*/
static void
_cw_prep(c3_i argc, c3_c* argv[])
{
  switch ( argc ) {
    case 2: {
      if ( !(u3_Host.dir_c = _main_pier_run(argv[0])) ) {
        fprintf(stderr, "unable to find pier\r\n");
        exit (1);
      }
    } break;

    case 3: {
      u3_Host.dir_c = argv[2];
    } break;

    default: {
      fprintf(stderr, "invalid command\r\n");
      exit(1);
    } break;
  }

  u3_Host.pep_o = c3y;
}

/* _cw_vere(): download vere
*/
static void
_cw_vere(c3_i argc, c3_c* argv[])
{
  c3_c* pac_c = "live";
  c3_c* arc_c = 0;
  c3_c* ver_c = 0;
  c3_c* dir_c;

  c3_i ch_i, lid_i;
  c3_w arg_w;

  static struct option lop_u[] = {
    { "arch",                required_argument, NULL, 'a' },
    { "pace",             required_argument, NULL, 'p' },
    { "version",             required_argument, NULL, 'v' },
    { NULL, 0, NULL, 0 }
  };

  while ( -1 != (ch_i=getopt_long(argc, argv, "a:p:v:", lop_u, &lid_i)) ) {
    switch ( ch_i ) {
      case 'a': {
        arc_c = strdup(optarg);
      } break;

      case 'p': {
        pac_c = strdup(optarg);
      } break;

      case 'v': {
        ver_c = strdup(optarg);
      } break;

      case '?': {
        exit(1);
      } break;
    }
  }

  //  argv[optind] is always "vere"/"fetch-vere"
  //

  if ( optind + 1 < argc ) {
    dir_c = argv[optind + 1];
    optind++;
  }
  else {
    fprintf(stderr, "invalid command, output directory required\r\n");
    exit(1);
  }

  if ( optind + 1 != argc ) {
    fprintf(stderr, "invalid command\r\n");
    exit(1);
  }

  if ( !arc_c ) {
#ifdef U3_OS_ARCH
    arc_c = U3_OS_ARCH;
#else
    fprintf(stderr, "unknown architecture, --arch required\r\n");
    exit(1);
#endif
  }

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

  if ( !ver_c ) {
    switch ( u3_king_next(pac_c, &ver_c) ) {
      case -2: {
        fprintf(stderr, "vere: unable to check for next version\n");
        exit(1);
      } break;

      case -1: {
        fprintf(stderr, "you're already running it!\n");
        exit(0);
      } break;

      case 0: {
        fprintf(stderr, "vere: next (%%%s): %s\n", pac_c, ver_c);
      } break;

      default: c3_assert(0);
    }
  }


  if ( u3_king_vere(pac_c, ver_c, arc_c, dir_c, 0) ) {
    u3l_log("vere: download failed\r\n");
    exit(1);
  }

  u3l_log("vere: download succeeded\r\n");
}

/* _cw_utils(): "worker" utilities and "serf" entrypoint
*/
static c3_i
_cw_utils(c3_i argc, c3_c* argv[])
{
  //  utility commands and positional arguments, by analogy
  //
  //    $@  ~                                             ::  usage
  //    $%  [%cram dir=@t]                                ::  jam state
  //        [%dock dir=@t]                                ::  copy binary
  //        [?(%grab %mass) dir=@t]                       ::  gc
  //        [%info dir=@t]                                ::  print
  //        [%meld dir=@t]                                ::  deduplicate
  //        [?(%next %upgrade) dir=@t]                    ::  upgrade
  //        [%pack dir=@t]                                ::  defragment
  //        [%prep dir=@t]                                ::  prep upgrade
  //        [%queu dir=@t eve=@ud]                        ::  cue state
  //        [?(%vere %fetch-vere) dir=@t]                 ::  download vere
  //    ::                                                ::    ipc:
  //        [%serf dir=@t key=@t wag=@t hap=@ud eve=@ud]  ::  compute
  //    ==
  //
  //    NB: don't print to anything other than stderr;
  //    other streams may be used for ipc.
  //
  c3_m mot_m = 0;

  if ( 2 <= argc ) {
    if ( 4 == strlen(argv[1]) ) {
      c3_c* s = argv[1];
      mot_m = c3_s4(s[0], s[1], s[2], s[3]);
    }
    else if ( 0 == strcmp(argv[1], "upgrade") ) {
      mot_m = c3__next;
    }
    else if ( 0 == strcmp(argv[1], "fetch-vere") ) {
      mot_m = c3__vere;
    }
  }

  switch ( mot_m ) {
    case c3__cram: _cw_cram(argc, argv); return 1;
    case c3__dock: _cw_dock(argc, argv); return 1;

    case c3__mass:
    case c3__grab: _cw_grab(argc, argv); return 1;

    case c3__info: _cw_info(argc, argv); return 1;
    case c3__meld: _cw_meld(argc, argv); return 1;
    case c3__next: _cw_next(argc, argv); return 2; // continue on
    case c3__pack: _cw_pack(argc, argv); return 1;
    case c3__prep: _cw_prep(argc, argv); return 2; // continue on
    case c3__queu: _cw_queu(argc, argv); return 1;
    case c3__vere: _cw_vere(argc, argv); return 1;

    case c3__serf: _cw_serf_commence(argc, argv); return 1;
    case c3__eval: _cw_eval_commence(argc, argv); return 1;
  }

  return 0;
}

c3_i
main(c3_i   argc,
     c3_c** argv)
{
  if ( argc <= 0 ) {
    fprintf(stderr, "nice try, fbi\r\n");
    exit(1);
  }

  _main_init();

  c3_c* bin_c = strdup(argv[0]);

  //  parse for subcommands
  //
  switch ( _cw_utils(argc, argv) ) {
    default: c3_assert(0);

    //  no matching subcommand, parse arguments
    //
    case 0: {
      if ( c3n == _main_getopt(argc, argv) ) {
        u3_ve_usage(argc, argv);
        return 1;
      }
    } break;

    //  ran subcommand
    case 1: {
      return 0;
    }

    //  found subcommand, continue
    //
    case 2: break;
  }

  _main_self_path();

  //  XX add argument
  //
  if ( !u3_Host.wrk_c ) {
    u3_Host.wrk_c = bin_c;
  }
  else {
    c3_free(bin_c);
  }

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
  printf("boot: home is %s\n", u3_Host.dir_c);
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

#ifdef U3_OS_mingw
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
