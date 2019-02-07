/* include/v/vere.h
**
** This file is in the public domain.
*/

#include "h2o.h"

  /** Quasi-tunable parameters.
  **/
    /* First kernel this executable can boot.
    */
#     define FirstKernel   164
#     define DefaultKernel 164

#define FDB_API_VERSION 520
#include <fdb_c.h>
#include <sqlite3.h>
#include "c.h" /* rocks db */
#include "lmdb.h" /* lmdb */

#define RECK

  /** Data types.
  **/

    /* u3_hhed: http header.
    */
      typedef struct _u3_hhed {
        struct _u3_hhed* nex_u;
        c3_w             nam_w;
        c3_c*            nam_c;
        c3_w             val_w;
        c3_c*            val_c;
      } u3_hhed;

    /* u3_hbod: http body block.  Also used for responses.
    */
      typedef struct _u3_hbod {
        struct _u3_hbod* nex_u;
        c3_w             len_w;
        c3_y             hun_y[0];
      } u3_hbod;

    /* u3_rsat: http request state.
    */
      typedef enum {
        u3_rsat_init = 0,                   //  initialized
        u3_rsat_plan = 1,                   //  planned
        u3_rsat_ripe = 2                    //  responded
      } u3_rsat;

    /* u3_hreq: incoming http request.
    */
      typedef struct _u3_hreq {
        h2o_req_t*       rec_u;             //  h2o request
        c3_w             seq_l;             //  sequence within connection
        u3_rsat          sat_e;             //  request state
        uv_timer_t*      tim_u;             //  timeout
        struct _u3_hcon* hon_u;             //  connection backlink
        struct _u3_hreq* nex_u;             //  next in connection's list
        struct _u3_hreq* pre_u;             //  next in connection's list
      } u3_hreq;

    /* u3_hcon: incoming http connection.
    */
      typedef struct _u3_hcon {
        uv_tcp_t         wax_u;             //  client stream handler
        h2o_conn_t*      con_u;             //  h2o connection
        h2o_socket_t*    sok_u;             //  h2o connection socket
        c3_w             ipf_w;             //  client ipv4
        c3_w             coq_l;             //  connection number
        c3_w             seq_l;             //  next request number
        struct _u3_http* htp_u;             //  server backlink
        struct _u3_hreq* req_u;             //  request list
        struct _u3_hcon* nex_u;             //  next in server's list
        struct _u3_hcon* pre_u;             //  next in server's list
      } u3_hcon;

    /* u3_http: http server.
    */
      typedef struct _u3_http {
        uv_tcp_t         wax_u;             //  server stream handler
        void*            h2o_u;             //  libh2o configuration
        struct _u3_prox* rox_u;             //  maybe proxied
        c3_w             sev_l;             //  server number
        c3_w             coq_l;             //  next connection number
        c3_s             por_s;             //  running port
        c3_o             sec;               //  logically secure
        c3_o             lop;               //  loopback-only
        c3_o             liv;               //  c3n == shutdown
        struct _u3_hcon* hon_u;             //  connection list
        struct _u3_http* nex_u;             //  next in list
      } u3_http;

    /* u3_form: http config from %eyre
    */
      typedef struct _u3_form {
        c3_o             pro;               //  proxy
        c3_o             log;               //  keep access log
        c3_o             red;               //  redirect to HTTPS
        uv_buf_t         key_u;             //  PEM RSA private key
        uv_buf_t         cer_u;             //  PEM certificate chain
      } u3_form;

    /* u3_hfig: general http configuration
    */
      typedef struct _u3_hfig {
        u3_form*         for_u;             //  config from %eyre
        struct _u3_warc* cli_u;             //  rev proxy clients
        struct _u3_pcon* con_u;             //  cli_u connections
      } u3_hfig;

    /* u3_proxy_type: proxy connection downstream type
    */
      typedef enum {
        u3_ptyp_prox = 0,                   //  connected to us
        u3_ptyp_ward = 1                    //  we connected back to
      } u3_proxy_type;

    /* u3_pcon: established proxy connection
    */
      typedef struct _u3_pcon {
        uv_tcp_t         don_u;             //  downstream handle
        uv_tcp_t*        upt_u;             //  upstream handle
        uv_buf_t         buf_u;             //  pending buffer
        c3_o             sec;               //  yes == https
        u3_proxy_type    typ_e;             //  tagged
        union {                             //  union
          struct _u3_warc* cli_u;           //  typ_e == ward
          struct _u3_prox* lis_u;           //  typ_e == prox
        } src_u;                            //  connection source
        struct _u3_pcon* nex_u;             //  next in list
        struct _u3_pcon* pre_u;             //  previous in list
      } u3_pcon;

    /* u3_warc: server connecting back to u3_ward as client
    */
      typedef struct _u3_warc {
        c3_w             ipf_w;             //  ward ip
        c3_s             por_s;             //  ward port
        c3_o             sec;               //  secure connection
        c3_d             who_d[2];          //  ward ship
        c3_c*            hot_c;             //  ward hostname
        uv_buf_t         non_u;             //  nonce
        struct _u3_http* htp_u;             //  local server backlink
        struct _u3_warc* nex_u;             //  next in list
        struct _u3_warc* pre_u;             //  previous in list
      } u3_warc;

    /* u3_wcon: candidate u3_ward upstream connection
    */
      typedef struct _u3_wcon {
        uv_tcp_t         upt_u;             //  connection handle
        struct _u3_ward* rev_u;             //  connecting to ward
        struct _u3_wcon* nex_u;             //  next in list
      } u3_wcon;

    /* u3_ward: reverse, reverse TCP proxy (ship-specific listener)
    */
      typedef struct _u3_ward {
        uv_tcp_t         tcp_u;             //  listener handle
        uv_timer_t       tim_u;             //  expiration timer
        c3_d             who_d[2];          //  reverse proxy for ship
        c3_s             por_s;             //  listening on port
        uv_buf_t         non_u;             //  nonce
        struct _u3_wcon* won_u;             //  candidate upstream connections
        struct _u3_pcon* con_u;             //  initiating connection
        struct _u3_ward* nex_u;             //  next in list
        struct _u3_ward* pre_u;             //  previous in list
      } u3_ward;

    /* u3_prox: reverse TCP proxy server
    */
      typedef struct _u3_prox {
        uv_tcp_t         sev_u;             //  server handle
        c3_s             por_s;             //  listening on port
        c3_o             sec;               //  yes == https
        struct _u3_http* htp_u;             //  local server backlink
        struct _u3_pcon* con_u;             //  active connection list
        struct _u3_ward* rev_u;             //  active reverse listeners
      } u3_prox;

    /* u3_csat: client connection state.
    */
      typedef enum {
        u3_csat_init = 0,                   //  initialized
        u3_csat_addr = 1,                   //  address resolution begun
        u3_csat_quit = 2,                   //  cancellation requested
        u3_csat_ripe = 3                    //  passed to libh2o
      } u3_csat;

    /* u3_cres: response to http client.
    */
      typedef struct _u3_cres {
        c3_w             sas_w;             //  status code
        u3_noun          hed;               //  headers
        u3_hbod*         bod_u;             //  exit of body queue
        u3_hbod*         dob_u;             //  entry of body queue
      } u3_cres;

    /* u3_creq: outgoing http request.
    */
      typedef struct _u3_creq {             //  client request
        c3_l             num_l;             //  request number
        h2o_http1client_t* cli_u;           //  h2o client
        u3_csat          sat_e;             //  connection state
        c3_o             sec;               //  yes == https
        c3_w             ipf_w;             //  IP
        c3_c*            ipf_c;             //  IP (string)
        c3_c*            hot_c;             //  host
        c3_s             por_s;             //  port
        c3_c*            por_c;             //  port (string)
        c3_m             met_m;             //  method
        c3_c*            url_c;             //  url
        u3_hhed*         hed_u;             //  headers
        u3_hbod*         bod_u;             //  body
        u3_hbod*         rub_u;             //  exit of send queue
        u3_hbod*         bur_u;             //  entry of send queue
        h2o_iovec_t*     vec_u;             //  send-buffer array
        u3_cres*         res_u;             //  nascent response
        struct _u3_creq* nex_u;             //  next in list
        struct _u3_creq* pre_u;             //  previous in list
      } u3_creq;

    /* u3_chot: foreign host (not yet used).
    */
      typedef struct _u3_chot {
        c3_w             ipf_w;             //  ip address (or 0)
        c3_c*            hot_c;             //  hostname (no port) (or 0)
        void*            ins_u;             //  insecure connection (or 0)
        void*            sec_u;             //  secure connection (or 0)
      } u3_chot;

    /* u3_cttp: http client.
    */
      typedef struct _u3_cttp {
        u3_creq*         ceq_u;             //  request list
        h2o_http1client_ctx_t*              //
                         ctx_u;             //  h2o client ctx
        void*            tls_u;             //  client SSL_CTX*
      } u3_cttp;

    /* u3_pact: ames packet, coming or going.
    */
      typedef struct _u3_pact {
        uv_udp_send_t    snd_u;             //  udp send request
        c3_w             pip_w;             //  target IPv4 address
        c3_s             por_s;             //  target port
        c3_w             len_w;             //  length in bytes
        c3_y*            hun_y;             //  packet buffer
        c3_y             imp_y;             //  galaxy number (optional)
        c3_c*            dns_c;             //  galaxy fqdn (optional)
      } u3_pact;

    /* u3_poke: poke callback function.
    */
      typedef void (*u3_poke)(void*, u3_noun);

    /* u3_bail: bailout callback function.
    */
      typedef void (*u3_bail)(void*, const c3_c* err_c);

    /* u3_done: completion function.
    */
      typedef void (*u3_done)(void *);

    /* u3_mess: blob message in process.
    */
      typedef struct _u3_mess {
        c3_d             len_d;             //  blob length in bytes
        c3_d             has_d;             //  currently held             
        struct _u3_meat* meq_u;             //  exit of message queue
        struct _u3_meat* qem_u;             //  entry of message queue
      } u3_mess;

    /* u3_meat: blob message block.
    */
      typedef struct _u3_meat {
        struct _u3_meat* nex_u;
        c3_d             len_d;
        c3_y             hun_y[0];
      } u3_meat;

    /* u3_moat: inbound message stream.
    */
      typedef struct _u3_moat {
        uv_pipe_t        pyp_u;             //  input stream
        u3_bail          bal_f;             //  error response function
        void*            vod_p;             //  callback pointer
        u3_poke          pok_f;             //  action function
        struct _u3_mess* mes_u;             //  message in progress
        c3_d             len_d;             //  length of stray bytes
        c3_y*            rag_y;             //  stray bytes
      } u3_moat;

    /* u3_mojo: outbound message stream.
    */
      typedef struct _u3_mojo {
        uv_pipe_t pyp_u;                    //  output stream
        u3_bail   bal_f;                    //  error response function
      } u3_mojo;

    /* u3_moor: two-way message stream, linked list */
      typedef struct _u3_moor {
        uv_pipe_t        pyp_u;
        u3_bail          bal_f;
        void*            vod_p;
        u3_poke          pok_f;
        struct _u3_mess* mes_u;
        c3_d             len_d;
        c3_y*            rag_y;
        struct _u3_moor* nex_u;
      } u3_moor;

    /* u3_foil: abstract chub-addressed file.
    */
      typedef struct _u3_foil {
        uv_file          fil_u;             //  libuv file handle
        struct _u3_dire* dir_u;             //  parent directory
        c3_c*            nam_c;             //  name within parent
        c3_d             end_d;             //  end of file
      } u3_foil;

    /* u3_dent: directory entry.
    */
      typedef struct _u3_dent {
        c3_c*            nam_c;
        struct _u3_dent* nex_u;
      } u3_dent;

    /* u3_dire: simple directory state.
    */
      typedef struct _u3_dire {
        c3_c*    pax_c;                     //  path of directory
        uv_file  fil_u;                     //  file, opened read-only to fsync
        u3_dent* all_u;                     //  file list 
      } u3_dire;

    /* u3_ames: ames networking.
    */
      typedef struct _u3_ames {             //  packet network state
        union {
          uv_udp_t    wax_u;
          uv_handle_t had_u;
        };
        c3_o          liv;                  //  listener on
        c3_o          alm;                  //  alarm on
        c3_w          law_w;                //  last wakeup, unix time
        c3_s          por_s;                //  public IPv4 port
        c3_c*         dns_c;                //  domain XX multiple/fallback
        c3_w          imp_w[256];           //  imperial IPs
        time_t        imp_t[256];           //  imperial IP timestamps
      } u3_ames;

    /* u3_save: checkpoint control.
    */
      typedef struct _u3_save {
        uv_timer_t  tim_u;                  //  checkpoint timer
        uv_signal_t sil_u;                  //  child signal
        c3_d        ent_d;                  //  event number
        c3_w        pid_w;                  //  pid of checkpoint process
      } u3_save;

    /* u3_ubuf: unix tty i/o buffer.
    */
      typedef struct _u3_ubuf {
        struct _u3_ubuf* nex_u;
        c3_w             len_w;
        c3_y             hun_y[0];          //  bytes to send
      } u3_ubuf;

    /* u3_utat: unix terminal state.
    */
      typedef struct {
        struct {
          c3_l  col_l;                      //  columns
          c3_l  row_l;                      //  rows
        } siz;

        struct {
          c3_w* lin_w;                      //  current line (utf32)
          c3_w  len_w;                      //  length of current line
          c3_w  cus_w;                      //  cursor position
        } mir;

        struct {                            //  escape code control
          c3_o    ape;                      //  escape received
          c3_o    bra;                      //  bracket or O received
        } esc;

        struct {
          c3_y syb_y[5];                    //  utf8 code buffer
          c3_w len_w;                       //  present length
          c3_w wid_w;                       //  total width
        } fut;

        struct {
          uv_thread_t* sit_u;               //  spinner thread
          c3_o         diz_o;               //  spinner activated
          c3_d         eve_d;               //  spinner start tick (unix μs)
          c3_d         end_d;               //  spinner end tick (unix μs)
          c3_c*        why_c;               //  spinner event wire (root only)
        } sun;

        uv_mutex_t     mex_u;               //  mutex for non-daemon term state
      } u3_utat;

      struct _u3_umon;
      struct _u3_udir;
      struct _u3_ufil;

    /* u3_unod: file or directory.
    */
      typedef struct _u3_unod {
        c3_o              dir;              //  c3y if dir, c3n if file
        c3_o              dry;              //  ie, unmodified
        c3_c*             pax_c;            //  absolute path
        struct _u3_udir*  par_u;            //  parent
        struct _u3_unod*  nex_u;            //  internal list
      } u3_unod;
      
    /* u3_ufil: synchronized file.
    */
      typedef struct _u3_ufil {
        c3_o              dir;              //  c3y if dir, c3n if file
        c3_o              dry;              //  ie, unmodified
        c3_c*             pax_c;            //  absolute path
        struct _u3_udir*  par_u;            //  parent
        struct _u3_unod*  nex_u;            //  internal list
        c3_w              mug_w;            //  mug of last %into
        c3_w              gum_w;            //  mug of last %ergo
      } u3_ufil;
      
    /* u3_ufil: synchronized directory.
    */
      typedef struct _u3_udir {
        c3_o              dir;              //  c3y if dir, c3n if file
        c3_o              dry;              //  ie, unmodified
        c3_c*             pax_c;            //  absolute path
        struct _u3_udir*  par_u;            //  parent
        struct _u3_unod*  nex_u;            //  internal list
        u3_unod*          kid_u;            //  subnodes
      } u3_udir;

    /* u3_ufil: synchronized mount point.
    */
      typedef struct _u3_umon {
        u3_udir          dir_u;             //  root directory, must be first
        c3_c*            nam_c;             //  mount point name
        struct _u3_umon* nex_u;             //  internal list
      } u3_umon;

    /* u3_usig: receive signals.
    */
      typedef struct _u3_usig {
        uv_signal_t      sil_u;
        c3_i             num_i;
        struct _u3_usig* nex_u;
      } u3_usig;

    /* u3_unix: clay support system, also
    */
      typedef struct _u3_unix {
        u3_umon*    mon_u;                  //  mount points
        c3_o        alm;                    //  timer set
        c3_o        dyr;                    //  ready to update
#ifdef SYNCLOG
        c3_w         lot_w;                 //  sync-slot
        struct _u3_sylo {
          c3_o     unx;                     //  from unix
          c3_m     wer_m;                   //  mote saying where
          c3_m     wot_m;                   //  mote saying what
          c3_c*    pax_c;                   //  path
        } sylo[1024];
#endif
      } u3_unix;

    /* u3_behn: just a timer for ever
    */
      typedef struct _u3_behn {
        uv_timer_t tim_u;                   //  behn timer
        c3_o       alm;                     //  alarm
      } u3_behn;

    /* u2_utfo: unix terminfo strings.
    */
      typedef struct {
        struct {
          const c3_y* kcuu1_y;              //  key_up
          const c3_y* kcud1_y;              //  key_down
          const c3_y* kcub1_y;              //  key_back
          const c3_y* kcuf1_y;              //  key_forward
          c3_w        max_w;                //  maximum input sequence length
        } inn;
        struct {
          const c3_y* clear_y;              //  clear_screen
          const c3_y* el_y;                 //  clr_bol clear to beginning
          // const c3_y* el1_y;                //  clr_eol clear to end
          const c3_y* ed_y;                 //  clear to end of screen
          const c3_y* bel_y;                //  bel sound bell
          const c3_y* cub1_y;               //  parm_left
          const c3_y* cuf1_y;               //  parm_right
          const c3_y* cuu1_y;               //  parm_up
          const c3_y* cud1_y;               //  parm_down
          // const c3_y* cub_y;                //  parm_left_cursor #num
          // const c3_y* cuf_y;                //  parm_right_cursor #num
        } out;
      } u3_utfo;

#if 0
    /* u3_uwen: unix alarm.
    */
      typedef struct _u3_uwen {
        c3_y* pax_y;                        //  printed path
        c3_d  wen_d[2];                     //  timer expire
      };

    /* u3_utim: unix timer control.
    */
      typedef struct _u3_utim {
        uv_timer_t wat_u;                   //  timer control
        u3_uwen*   wen_u;                   //  timers in ascending order
      };
#endif

    /* u3_utty: unix tty.
    */
      typedef struct _u3_utty {
        union {
          uv_pipe_t      pop_u;
          uv_tcp_t       wax_u;
        };
        struct _u3_utty* nex_u;             //  next in host list
        c3_i             fid_i;             //  file descriptor
        c3_w             tid_l;             //  terminal identity number
        u3_utfo          ufo_u;             //  terminfo strings
        c3_i             cug_i;             //  blocking fcntl flags
        c3_i             nob_i;             //  nonblocking fcntl flags
        u3_utat          tat_u;             //  control state
        struct termios   bak_u;             //  cooked terminal state
        struct termios   raw_u;             //  raw terminal state
      } u3_utty;

    /* u3_trac: tracing information.
    */
      typedef struct _u3_trac {
        c3_w   nid_w;                       //  nock pid
        FILE*  fil_u;                       //  trace file (json)
        c3_w   con_w;                       //  trace counter
      } u3_trac;

    /* u3_opts: command line configuration.
    */
      typedef struct _u3_opts {
        // XX find a way to re-enable -A (fastboot)
        // c3_c*   arv_c;                      //  -A, initial sync from
        c3_c*   pin_c;                      //  -i, persistant storage in
        c3_c*   pot_c;                      //  -o, persistant storage out
        c3_o    abo;                        //  -a, abort aggressively
        c3_c*   pil_c;                      //  -B, bootstrap from
        c3_o    bat;                        //  -b, batch create
        c3_o    can;                        //  -C, chain-only, no eth snapshot
        c3_o    nuu;                        //  -c, new pier
        c3_o    dry;                        //  -D, dry compute, no checkpoint
        c3_o    dem;                        //  -d, daemon
        c3_c*   ets_c;                      //  -E, eth snapshot
        c3_c*   eth_c;                      //  -e, ethereum node url
        c3_c*   fak_c;                      //  -F, fake ship
        c3_w    fuz_w;                      //  -f, fuzz testing
        c3_c*   gen_c;                      //  -G, czar generator
        c3_o    gab;                        //  -g, test garbage collection
        c3_c*   dns_c;                      //  -H, ames bootstrap domain
        c3_c*   lit_c;                      //  -J, ivory (fastboot) kernel
        c3_o    tra;                        //  -j, json trace
        c3_w    kno_w;                      //  -K, kernel version
        c3_c*   key_c;                      //  -k, private key file
        c3_o    net;                        //  -L, local-only networking
        c3_c*   sap_c;                      //  -m, eth snapshot url
        c3_o    pro;                        //  -P, profile
        c3_s    por_s;                      //  -p, ames port
        c3_o    qui;                        //  -q, quiet
        c3_o    rep;                        //  -R, report build info
        c3_o    has;                        //  -S, Skip battery hashes
        // XX find a way to re-enable -s (auto-pill)
        // c3_o    git;                        //  -s, pill url from arvo git hash
        c3_o    etn;                        //  -t, trust snapshot for pre-boot
        c3_c*   url_c;                      //  -u, pill url
        c3_o    vno;                        //  -V, replay without reboots
        c3_o    veb;                        //  -v, verbose (inverse of -q)
        c3_c*   who_c;                      //  -w, begin with ticket
        c3_o    tex;                        //  -x, exit after loading
      } u3_opts;

    /* u3_host: entire host.
    */
      typedef struct _u3_host {
        c3_w       kno_w;                   //  current executing stage
        c3_c*      dir_c;                   //  pier path (no trailing /)
        c3_d       now_d;                   //  event tick
        uv_loop_t* lup_u;                   //  libuv event loop
        u3_usig*   sig_u;                   //  signal list
        u3_hfig    fig_u;                   //  http configuration
        u3_http*   htp_u;                   //  http servers
        u3_cttp    ctp_u;                   //  http clients
        u3_utty*   uty_u;                   //  linked terminal list
        u3_opts    ops_u;                   //  commandline options
        c3_o       liv;                     //  if u3_no, shut down
        c3_i       xit_i;                   //  exit code for shutdown
        void*      tls_u;                   //  server SSL_CTX*
        u3_trac    tra_u;                   //  tracing information
      } u3_host;                            //  host == computer == process

    /**  New pier system.
    **/
      /* u3_writ: inbound event.
      */
        typedef struct _u3_writ {
          struct _u3_pier* pir_u;               //  backpointer to pier
          u3_noun          job;                 //  (pair date ovum)
          c3_d             evt_d;               //  event number
          u3_noun          now;                 //  event time
          c3_l             msc_l;               //  ms to timeout
          c3_l             mug_l;               //  hash before executing
          u3_atom          mat;                 //  jammed $work, or 0
          u3_noun          act;                 //  action list
          void*            mwh_u;               //  multi-write handle (for fond.c writes)
          c3_o             pes_o;               //  write submitted to initiated to persistant store
          c3_o             ped_o;               //  write done
          c3_o             ces_o;               //  compute started
          c3_o             ced_o;               //  compute done
          struct _u3_writ* nex_u;               //  next in queue, or 0
        } u3_writ;

      /* u3_lord: working process controller.
      */
        typedef struct _u3_lord {
          uv_process_t         cub_u;           //  process handle
          uv_process_options_t ops_u;           //  process configuration
          uv_stdio_container_t cod_u[3];        //  process options
          time_t               wen_t;           //  process creation time
          u3_mojo              inn_u;           //  client's stdin
          u3_moat              out_u;           //  client's stdout
          c3_d                 sen_d;           //  last event dispatched
          c3_d                 dun_d;           //  last event completed
          c3_d                 rel_d;           //  last event released
          c3_l                 mug_l;           //  mug after last completion
          struct _u3_pier*     pir_u;           //  pier backpointer
        } u3_lord;

      /* u3_queue: manage events (incoming, processing, persisted in disk / database)
      **
      **    any event once discovered should be in one of these sets.
      **    at present, all sets are ordered and can be defined by a
      **    simple counter.  any events <= the counter is in the set.
      */
        typedef struct _u3_queue {
      /* persistance (disk / SQLite / FoundationDB) state machine */
          c3_d             moc_d;               //  ID of max event sent to persistant store
          c3_d             com_d;               //  ID of max event confirmed by persistant store
          struct _u3_pier* pir_u;               //  pier backpointer
        } u3_queue;

        /* persistence backends: legacy disk, SQLite, FoundationDB, RocksDB */
        typedef struct _u3_disk {
          u3_dire*         dir_u;               //  main pier directory
          u3_dire*         urb_u;               //  urbit system data
          u3_dire*         com_u;               //  log directory
          u3_foil*         fol_u;               //  logfile
          c3_d             end_d;               //  byte end of file
          struct _u3_pier* pir_u;               //  pier backpointer
        } u3_disk;

        typedef struct _u3_sqlt {
          sqlite3 *        sql_u;               //
          struct _u3_pier* pir_u;               //  pier backpointer
        } u3_sqlt;

        typedef struct _u3_fond {
          FDBDatabase *    dab_u;               //
          struct _u3_pier* pir_u;               //  pier backpointer
        } u3_fond;

        typedef struct _u3_rock {
          rocksdb_t *      rok_u;               // db
          rocksdb_options_t * rop_u;            // db options
          rocksdb_writeoptions_t * wri_u;       // write options
          rocksdb_readoptions_t * red_u;        // read options
          struct _u3_pier* pir_u;               // pier backpointer
        } u3_rock;

        typedef struct _u3_lmdb {
          MDB_env * env_u;
          MDB_txn * txn_u;
          MDB_dbi dbi_u;
          c3_y * path_c;
          pthread_mutex_t  mut_u; /* don't trust lmdb locking */
        } u3_lmdb;

      /* u3_pers: persistance handled
      */
        typedef struct _u3_pers {
          u3_sqlt *        sqlt_u;              //  option 1: sqlite
          u3_fond *        fond_u;              //  option 2: foundationdb
          u3_rock *        rock_u;              //  option 3: rocksdb
          u3_lmdb *        lmdb_u;              //  option 4: lmdb
          c3_d             pos_d;               //  read position
        } u3_pers;

      /* u3_pier: ship controller.
      */
        typedef struct _u3_pier {
          c3_c*            pax_c;               //  pier directory
          c3_c*            sys_c;               //  pill file
          c3_w             wag_w;               //  config flags
          c3_d             gen_d;               //  last event discovered  (our counter for events; new events based off this)
          c3_d             but_d;               //  boot barrier
          c3_d             tic_d[1];            //  ticket (unstretched)
          c3_d             sec_d[1];            //  generator (unstretched)
          c3_d             key_d[4];            //  secret (stretched)
          c3_d             who_d[2];            //  identity
          c3_c*            who_c;               //  identity as C string
          c3_s             por_s;               //  UDP port
          c3_o             fak_o;               //  yes iff fake security
          u3_noun          bot;                 //  boot event XX review
          u3_lord*         god_u;               //  computer
          u3_ames*         sam_u;               //  packet interface
          u3_behn*         teh_u;               //  behn timer
          u3_unix*         unx_u;               //  sync and clay
          u3_save*         sav_u;               //  autosave
          u3_writ*         ent_u;               //  entry of queue
          u3_writ*         ext_u;               //  exit of queue
          uv_prepare_t     pep_u;               //  preloop registration
          u3_queue *       que_u;               //  event queue
          u3_pers *        pin_u;               //  persistance (read in)
          u3_pers *        pot_u;               //  persistance (write out)
        } u3_pier;

      /* u3_king: all executing piers.
      */
        typedef struct _u3_king {
          c3_c*     soc_c;                      //  socket name
          c3_w      len_w;                      //  number of lords used
          c3_w      all_w;                      //  number of lords allocated
          u3_pier** tab_u;                      //  lord table
          uv_pipe_t cmd_u;                      //  command socket
          u3_moor*  cli_u;                      //  connected clients
        } u3_king;

#     define u3L  u3_Host.lup_u             //  global event loop
#     define u3Z  (&(u3_Raft))
#     define u3K  u3_King

  /** Global variables.
  **/
    c3_global  u3_host  u3_Host;
    c3_global  c3_c*    u3_Local;
    c3_global  u3_king  u3_King;

  /** Functions.
  **/
    /*  Urbit time: 128 bits, leap-free.
    **
    **  High 64 bits: 0x8000.000c.cea3.5380 + Unix time at leap 25 (Jul 2012)
    **  Low 64 bits: 1/2^64 of a second.
    **
    **  Seconds per Gregorian 400-block: 12.622.780.800
    **  400-blocks from 0 to 0AD: 730.692.561
    **  Years from 0 to 0AD: 292.277.024.400
    **  Seconds from 0 to 0AD: 9.223.372.029.693.628.800
    **  Seconds between 0A and Unix epoch: 62.167.219.200
    **  Seconds before Unix epoch: 9.223.372.091.860.848.000
    **  The same, in C hex notation: 0x8000000cce9e0d80ULL
    **
    **  XX: needs to be adjusted to implement Google leap-smear time.
    */
      /* u3_time_sec_in(): urbit seconds from unix time.
      **
      ** Adjust (externally) for future leap secs!
      */
        c3_d
        u3_time_sec_in(c3_w unx_w);

      /* u3_time_sec_out(): unix time from urbit seconds.
      **
      ** Adjust (externally) for future leap secs!
      */
        c3_w
        u3_time_sec_out(c3_d urs_d);

      /* u3_time_fsc_in(): urbit fracto-seconds from unix microseconds.
      */
        c3_d
        u3_time_fsc_in(c3_w usc_w);

      /* u3_time_fsc_out: unix microseconds from urbit fracto-seconds.
      */
        c3_w
        u3_time_fsc_out(c3_d ufc_d);

      /* u3_time_in_tv(): urbit time from struct timeval.
      */
        u3_atom
        u3_time_in_tv(struct timeval* tim_tv);

      /* u3_time_out_tv(): struct timeval from urbit time.
      */
        void
        u3_time_out_tv(struct timeval* tim_tv, u3_noun now);

      /* u3_time_in_ts(): urbit time from struct timespec.
      */
        u3_atom
        u3_time_in_ts(struct timespec* tim_ts);
#if defined(U3_OS_linux)
      /* u3_time_t_in_ts(): urbit time from time_t.
       */
         u3_atom
         u3_time_t_in_ts(time_t tim);
#endif

      /* u3_time_out_ts(): struct timespec from urbit time.
      */
        void
        u3_time_out_ts(struct timespec* tim_ts, u3_noun now);

      /* u3_time_gap_ms(): (wen - now) in ms.
      */
        c3_d
        u3_time_gap_ms(u3_noun now, u3_noun wen);

    /**  Filesystem (new api).
    **/
      /* u3_walk_load(): load file or bail.
      */
        u3_noun
        u3_walk_load(c3_c* pas_c);

      /* u3_walk_safe(): load file or 0.
      */
        u3_noun
        u3_walk_safe(c3_c* pas_c);

      /* u3_walk_save(): save file or bail.
      */
        void
        u3_walk_save(c3_c* pas_c, u3_noun tim, u3_atom pad, c3_c* bas_c, u3_noun pax);

      /* u3_sync_reck(): traverse filesystem for changes -> lamb
      */
        u3_noun
        u3_sync_reck(void);

      /* u3_walk(): traverse `dir_c` to produce an arch, updating `old`.
      */
        u3_noun
        u3_walk(const c3_c* dir_c, u3_noun old);

      /* u3_path(): C unix path in computer for file or directory.
      */
        c3_c*
        u3_path(c3_o    fyl, u3_noun pax);

    /**  Filesystem (old api).
    **/
      /* u3_ve_file(): load internal file as atom from local or system.
      */
        u3_weak
        u3_ve_file(c3_c* ext_c, u3_noun tah);

      /* u3_ve_frep(): load [.~ %rep myp {now} tah].
      **
      **   File is either ~ or [nbytes mdate atom].
      */
        u3_noun
        u3_ve_frep(u3_noun myp, u3_noun tah);

      /* u3_ve_date(): date internal file.
      */
        c3_d
        u3_ve_date(c3_c* ext_c, u3_noun tah);

      /* u3_ve_save(): save internal file as atom.
      */
        c3_o   
        u3_ve_save(c3_c* ext_c, u3_noun tah, u3_noun dat);

      /* u3_ve_zeus(): prayer to internal file path.  Return unit.
      */
        u3_noun
        u3_ve_zeus(u3_noun hap);

    /**  Filesystem (async)
    **/
      /* u3_foil_folder(): load directory, blockingly.  create if nonexistent.
      */
        u3_dire*
        u3_foil_folder(const c3_c* pax_c);         //  directory object, or 0

      /* u3_foil_create(): create a new, empty file, not syncing.
      */
        void
        u3_foil_create(void      (*fun_f)(void*,    //  context pointer
                                          u3_foil*),//  file object
                       void*       vod_p,           //  context pointer
                       u3_dire*    dir_u,           //  directory
                       const c3_c* nam_c);          //  name of new file

      /* u3_foil_absorb(): absorb logfile, truncating to last good frame; block.
      */
        u3_foil*
        u3_foil_absorb(u3_dire* dir_u,              //  directory
                       c3_c*    nam_c);             //  filename

      /* u3_foil_delete(): delete a file; free descriptor.
      */
        void
        u3_foil_delete(void   (*fun_f)(void*),      //  context pointer
                       void*    vod_p,              //  context pointer
                       u3_foil* fol_u);             //  file to delete

      /* u3_foil_append(): write a frame at the end of a file, freeing buffer.
      */
        void
        u3_foil_append(void   (*fun_f)(void*),      //  context pointer
                       void*    vod_p,              //  context pointer
                       u3_foil* fol_u,              //  file
                       c3_d*    buf_d,              //  buffer to write from
                       c3_d     len_d);             //  length in chubs

      /* u3_foil_reveal(): read the frame before a position, blocking.
      */
        c3_d*
        u3_foil_reveal(u3_foil* fol_u,              //  file from
                       c3_d*    pos_d,              //  end position/prev end
                       c3_d*    len_d);             //  length return

      /* u3_foil_commit(): reveal from one file, append to another.
      */
        void
        u3_foil_commit(void   (*fun_f)(void*,       //  context pointer
                                       u3_foil*,    //  file from
                                       c3_d,        //  previous from
                                       u3_foil*,    //  file to
                                       c3_d),       //  end of to
                       void*    vod_p,              //  context pointer
                       u3_foil* del_u,              //  file from
                       c3_d     del_d,              //  end of from frame
                       u3_foil* unt_u,              //  file to
                       c3_d     unt_d);             //  end of to frame

      /* u3_foil_invent(): make new file with one frame; free buffer, sync.
      */
        void
        u3_foil_invent(void   (*fun_f)(void*,       //  context pointer
                                       u3_foil*),   //  new file
                       void*    vod_p,              //  context pointer
                       u3_dire* dir_u,              //  directory
                       c3_c*    nam_c,              //  filename
                       c3_d*    buf_d,              //  buffer (to free)
                       c3_d     len_d);             //  length

    /**  Output.
    **/
      /* u3_reck_kick(): handle effect.
      */
        void
        u3_reck_kick(u3_pier* pir_u, u3_noun ovo);

    /**  Terminal, new style.
    **/
      /* u3_term_get_blew(): return window size [columns rows].
      */
        u3_noun
        u3_term_get_blew(c3_l tid_l);

      /* u3_term_ef_boil(): initial effects for restored server.
      */
        void
        u3_term_ef_boil();

      /* u3_term_ef_verb(): initial effects for verbose events.
      */
        void
        u3_term_ef_verb(void);

      /* u3_term_ef_winc(): window change.
      */
        void
        u3_term_ef_winc(void);

      /* u3_term_ef_ctlc(): send ^C.
      */
        void
        u3_term_ef_ctlc(void);

      /* u3_term_ef_bake(): initial effects for new server.
      */
        void
        u3_term_ef_bake(void);

      /* u3_term_ef_blit(): send %blit effect to terminal.
      */
        void
        u3_term_ef_blit(c3_l    tid_l,
                        u3_noun blt);

      /* u3_term_io_init(): initialize terminal I/O.
      */
        void
        u3_term_io_init(void);

      /* u3_term_io_talk(): start terminal listener.
      */
        void
        u3_term_io_talk(void);

      /* u3_term_io_exit(): terminate terminal I/O.
      */
        void
        u3_term_io_exit(void);

      /* u3_term_io_hija(): hijack console for cooked print.
      */
        FILE*
        u3_term_io_hija(void);

      /* u3_term_io_loja(): release console from cooked print.
      */
        void
        u3_term_io_loja(int x);

      /* uL, uH: wrap hijack/lojack around fprintf.
      **
      **  uL(fprintf(uH, ...));
      */
#       define uH    u3_term_io_hija()
#       define uL(x) u3_term_io_loja(x)


    /**  Ames, packet networking.
    **/
      /* u3_ames_ef_bake(): create ames duct.
      */
        void
        u3_ames_ef_bake(u3_pier* pir_u);

      /* u3_ames_ef_send(): send packet to network.
      */
        void
        u3_ames_ef_send(u3_pier* pir_u,
                        u3_noun lan,
                        u3_noun pac);

      /* u3_ames_ef_turf(): initialize ames I/O on domain(s).
      */
        void
        u3_ames_ef_turf(u3_pier* pir_u,
                        u3_noun tuf);

      /* u3_ames_io_init(): initialize ames I/O.
      */
        void
        u3_ames_io_init(u3_pier* pir_u);

      /* u3_ames_io_talk(): bring up listener.
      */
        void
        u3_ames_io_talk(u3_pier* pir_u);

      /* u3_ames_ef_bake(): send initial events.
      */
        void 
        u3_ames_io_bake(u3_pier* pir_u);

      /* u3_ames_io_exit(): terminate ames I/O.
      */
        void
        u3_ames_io_exit(u3_pier* pir_u);


    /**  Autosave.
    **/
      /* u3_save_ef_chld(): report SIGCHLD.
      */
        void
        u3_save_ef_chld(u3_pier *pir_u);

      /* u3_save_io_init(): initialize autosave.
      */
        void
        u3_save_io_init(u3_pier *pir_u);

      /* u3_save_io_exit(): terminate autosave.
      */
        void
        u3_save_io_exit(u3_pier *pir_u);


    /**  Storage, new school.
    **/
      /* u3_unix_ef_hold():
      */
        void
        u3_unix_ef_hold(void);

      /* u3_unix_ef_boot(): boot actions 
      */
        void
        u3_unix_ef_boot(u3_pier *pir_u);

      /* u3_unix_ef_bake(): initial effects for new process.
      */
        void
        u3_unix_ef_bake(u3_pier *pir_u);

      /* u3_unix_ef_move():
      */
        void
        u3_unix_ef_move(void);

      /* u3_unix_initial_into(): intialize filesystem from urb/zod
      */
        void
        u3_unix_ef_initial_into(u3_pier *pir_u);

      /* u3_unix_ef_look(): update filesystem from unix
      */
        void
        u3_unix_ef_look(u3_pier *pir_u, u3_noun all);

      /* u3_unix_ef_ergo(): update filesystem from urbit
      */
        void
        u3_unix_ef_ergo(u3_pier *pir_u, u3_noun mon, u3_noun can);

      /* u3_unix_ef_dirk(): mark mount dirty
      */
        void
        u3_unix_ef_dirk(u3_pier *pir_u, u3_noun mon);

      /* u3_unix_ef_ogre(): delete mount point
      */
        void
        u3_unix_ef_ogre(u3_pier *pir_u, u3_noun mon);

      /* u3_unix_ef_hill(): enumerate mount points
      */
        void
        u3_unix_ef_hill(u3_pier *pir_u, u3_noun hil);

      /* u3_unix_io_init(): initialize storage.
      */
        void
        u3_unix_io_init(u3_pier *pir_u);

      /* u3_unix_io_talk(): start listening for fs events.
      */
        void
        u3_unix_io_talk(u3_pier *pir_u);

      /* u3_unix_io_exit(): terminate storage.
      */
        void
        u3_unix_io_exit(u3_pier *pir_u);


    /**  behn, just a timer.
    **/
      /* u3_behn_io_init(): initialize behn timer.
      */
        void
        u3_behn_io_init(u3_pier *pir_u);

      /* u3_behn_io_exit(): terminate timer.
      */
        void
        u3_behn_io_exit(u3_pier *pir_u);

      /* u3_behn_ef_bake(): notify %behn that we're live
      */
        void
        u3_behn_ef_bake(u3_pier *pir_u);

      /* u3_behn_ef_doze(): set or cancel timer
      */
        void
        u3_behn_ef_doze(u3_pier *pir_u, u3_noun wen);


    /**  HTTP server.
    **/
      /* u3_http_ef_form: send %from effect to http.
      */
        void
        u3_http_ef_form(u3_noun fig);

      /* u3_http_ef_that: send %that effect to http.
      */
        void
        u3_http_ef_that(u3_noun tat);

      /* u3_http_ef_thou(): send %thou effect to http.
      */
        void
        u3_http_ef_thou(c3_l     sev_l,
                        c3_l     coq_l,
                        c3_l     seq_l,
                        u3_noun  rep);

      /* u3_cttp_ef_thus(): send %thus effect to cttp.
      */
        void
        u3_cttp_ef_thus(c3_l    num_l,
                        u3_noun req);

      /* u3_http_ef_bake(): create new http server.
      */
        void
        u3_http_ef_bake(void);

      /* u3_http_io_init(): initialize http I/O.
      */
        void
        u3_http_io_init(void);

      /* u3_http_io_talk(): start http listener.
      */
        void
        u3_http_io_talk(void);

      /* u3_http_io_exit(): terminate http I/O.
      */
        void
        u3_http_io_exit(void);


    /**  HTTP client.
    **/
      /* u3_cttp_ef_thus(): send %thus effect to cttp.
      */
        void
        u3_cttp_ef_thus(c3_l    num_l,
                        u3_noun req);

      /* u3_cttp_io_init(): initialize cttp I/O.
      */
        void
        u3_cttp_io_init(void);

      /* u3_cttp_io_exit(): terminate cttp I/O.
      */
        void
        u3_cttp_io_exit(void);


    /**  Stream messages.
    **/
      /* u3_newt_write(): write atom to stream; free atom.
      */
        void
        u3_newt_write(u3_mojo* moj_u,
                      u3_atom  mat,
                      void*    vod_p);

      /* u3_newt_read(): activate reading on input stream.
      */
        void
        u3_newt_read(u3_moat* mot_u);

    /** Pier control.
    **/


      /* u3_pier_create(): create a pier, loading existing.
      */
        u3_pier*
        u3_pier_create(c3_w wag_w, c3_c* pax_c, c3_c* sys_c);

      /* u3_pier_interrupt(): interrupt running process.
      */
        void
        u3_pier_interrupt(u3_pier* pir_u);

      /* u3_pier_discover(): insert task into process controller.
      */
        void
        u3_pier_discover(u3_pier* pir_u,
                         c3_l     msc_l,
                         u3_noun  job);

      /* u3_pier_exit(): trigger a gentle shutdown.
      */
        void
        u3_pier_exit(void);

      /* u3_pier_work(): send event; real pier pointer.
      */
        void
        u3_pier_work(u3_pier* pir_u, u3_noun pax, u3_noun fav);

      /* u3_pier_stub(): get the One Pier for unreconstructed code.
      */
        u3_pier*
        u3_pier_stub(void);

      /* u3_pier_plan(): submit event; fake pier 
      */
        void
        u3_pier_plan(u3_noun pax, u3_noun fav);

      /* u3_pier_boot(): start the new pier system.
      */
        void
        u3_pier_boot(c3_w    wag_w,                 //  config flags
                     u3_noun who,                   //  identity
                     u3_noun ven,                   //  boot event
                     u3_noun pil,                   //  type-of/path-to pill
                     u3_noun pax);                  //  path to pier

      /* u3_pier_stay(): restart the new pier system.
      */
        void
        u3_pier_stay(c3_w wag_w, u3_noun pax);

      /* u3_pier_tank(): dump single tank.
      */
        void
        u3_pier_tank(c3_l tab_l, u3_noun tac);

      /* u3_pier_punt(): dump tank list.
      */
        void
        u3_pier_punt(c3_l tab_l, u3_noun tac);

      /* u3_pier_sway(): print trace.
      */
        void
        u3_pier_sway(c3_l tab_l, u3_noun tax);

      /* u3_dawn_come(): mine a comet
      */
        u3_noun
        u3_dawn_come(void);

      /* u3_dawn_vent(): validated boot event
      */
        u3_noun
        u3_dawn_vent(u3_noun seed);

      /* u3_king_commence(): start the daemon
      */
        void
        u3_king_commence();


        void
        u3_pier_apply(u3_pier*);

        void
        _pier_init_read(u3_pier* pir_u, c3_c * pin_c);

        void
        _pier_init_writ(u3_pier* pir_u, c3_c * pot_c);

     /* read */

     typedef struct _mult_read_hand {
       void      * han_u;   /* if the read was a single-fragment read, this will be set and needs to be cleaned up */
       c3_y      * dat_y;   /* if the read was a multi-fragment read, this will be set and needs to be free()-ed */
     } mult_read_hand;




     typedef c3_o
     (* _frag_read)(u3_pers * pers_u,    /* IN: FoundationDB handle */
                    c3_d pos_d,            /* IN: row id */
                    c3_w frg_w,            /* IN: fragment id */
                    c3_y ** dat_y,        /* OUT: set pointer to data */
                    c3_w * len_w,        /* OUT: set len of data */
                    void ** opaq_u);

     typedef void
     (* _frag_done)(void * opaq_u);


     c3_o u3_frag_read(_frag_read read_u,
                    _frag_done done_u,
                    c3_w max_w,
                    u3_pers* pers_u,
                    c3_y **  dat_y,
                    c3_w * len_w,
                    mult_read_hand ** hand_u);

     c3_o u3_frag_read_done(mult_read_hand * mrh_u, _frag_done done_u);

     /* write */

    typedef void (*writ_test_cb)(void*);  /* a callback used in testing */


      typedef struct _u3_pers_frag {
        c3_o *           don_o;     /* array of flags, one per fragment - used by frag layer */
        pthread_mutex_t  mut_u;
      } u3_pers_frag;

      typedef struct _u3_pers_writ_calb {

        u3_writ *        wit_u;        /* the writ from which this fragment comes */

        c3_w             cnt_w;        /* total number of fragments (OPTIONAL) - used by frag layer */
        c3_w             frg_w;        /* index of this fragment    (OPTIONAL) - used by frag layer */
        u3_pers_frag *   frg_u;     /* fragmentation data        (OPTIONAL) - used by frag layer */ 
        
        writ_test_cb     cbf_u;       /* meta callback function - used for speed tests */
        void *           ext_u;     /* extension data - each persistence engine can use as it sees fit */

      } u3_pers_writ_calb;



     typedef void
     (* _writ_frag)(u3_writ* wit_u,      /* IN: writ */
                    c3_d pos_d,          /* IN: row id */
                    c3_w frg_w,          /* IN: frag id */
                    c3_w cnt_w,          /* IN: frag id */
                    c3_y* byt_y,         /* IN: frag data (with header) */
                    c3_w  len_w,         /* IN: frag len */
                    u3_pers_frag * mwh_u, /* IN: multi-frag handle  */
                    writ_test_cb test_cb
                    );

     void frag_writ(c3_w max_w,          /* IN: max fragment size */
                    u3_writ* wit_u,      /* IN: writ */
                    c3_d pos_d,          /* IN: row id */
                    c3_y* buf_y,         /* IN: frag data (with space for header) */
                    c3_y* byt_y,         /* IN: frag data (with header) */
                    c3_w  len_w,         /* IN: frag len */
                    _writ_frag,          /* IN: persistence driver function that actually writes the frag */          
                    writ_test_cb test_cb);

void u3_frag_write_check(u3_pers_writ_calb  * cbd_u);

c3_o u3_frag_write_done(c3_w frg_w,
                        c3_w cnt_w,
                        u3_writ * wit_u,
                        u3_pers_frag * mwh_u);


     c3_w u3_frag_head_size(c3_w len_w, /* size of total write */
                            c3_w frg_w, /* this fragment index */
                            c3_w max_w); /* max fragment size */


     /*  plugable storage backend functions 
     */

      /* when an async write to a db completes, a callback fires. That
         callback needs data passed to it so that the CB knows what
         writ and fragment it's working on, and can do appropriate
         signalling / cleanup. */



     /* read */
        c3_o
        u3_disk_read_init(u3_pier* pir_u, c3_c * sto_c);
        c3_o
        u3_sqlt_read_init(u3_pier* pir_u, c3_c * sto_c);
        c3_o
        u3_fond_read_init(u3_pier* pir_u, c3_c * sto_c);
        c3_o
        u3_rock_read_init(u3_pier* pir_u, c3_c * sto_c);
        c3_o
        u3_lmdb_read_init(u3_pier* pir_u, c3_c * sto_c);

        c3_o
        u3_disk_read_read(u3_pier* pir_u,  c3_y ** dat_y, c3_w * len_w, void ** hand_u);
        c3_o
        u3_sqlt_read_read(u3_pier* pir_u,  c3_y ** dat_y, c3_w * len_w, void ** hand_u);
        c3_o
        u3_fond_read_read(u3_pier* pir_u,  c3_y ** dat_y, c3_w * len_w, void ** hand_u);
        c3_o
        u3_rock_read_read(u3_pier* pir_u,  c3_y ** dat_y, c3_w * len_w, void ** hand_u);
        c3_o
        u3_lmdb_read_read(u3_pier* pir_u,  c3_y ** dat_y, c3_w * len_w, void ** hand_u);

        void
        u3_disk_read_done(void * hand_u);
        void
        u3_sqlt_read_done(void * hand_u);
        void
        u3_fond_read_done(void * hand_u);
        void
        u3_rock_read_done(void * hand_u);
        void
        u3_lmdb_read_done(void * hand_u);

        void
        u3_disk_read_shut(u3_pier* pir_u);
        void
        u3_sqlt_read_shut(u3_pier* pir_u);
        void
        u3_fond_read_shut(u3_pier* pir_u);
        void
        u3_rock_read_shut(u3_pier* pir_u);
        void
        u3_lmdb_read_shut(u3_pier* pir_u);

        /* write */




        c3_o
        u3_disk_write_init(u3_pier* pir_u, c3_c * sto_c);
        c3_o
        u3_sqlt_write_init(u3_pier* pir_u, c3_c * sto_c);
        c3_o
        u3_fond_write_init(u3_pier* pir_u, c3_c * sto_c);
        c3_o
        u3_rock_write_init(u3_pier* pir_u, c3_c * sto_c);
        c3_o
        u3_lmdb_write_init(u3_pier* pir_u, c3_c * sto_c);

        c3_w
        u3_disk_frag_size();
        c3_w
        u3_sqlt_frag_size();
        c3_w
        u3_fond_frag_size();
        c3_w
        u3_rock_frag_size();
        c3_w
        u3_lmdb_frag_size();


        void
        u3_pier_writ_done(u3_noun);   /* callback from pluggable persistance noting noun is committed */

        typedef void (*writ_test_cb)(void*);  /* a callback used in testing */

        /*  conforming write_write() functions must do three things:
              1) write to persistent store     [ action ]
              2) on success,set
                     wit_u->ped_o = c3y        [ ack     ]
              3) free(buf_y)                   [ cleanup ]
        */
        void
        u3_disk_write_write(u3_writ* wit_u, c3_d pos_d, c3_y* buf_y, c3_y* byt_y, c3_w  len_w, writ_test_cb test_cb);
        void
        u3_sqlt_write_write(u3_writ* wit_u, c3_d pos_d, c3_y* buf_y, c3_y* byt_y, c3_w  len_w, writ_test_cb test_cb);
        void
         u3_fond_write_write(u3_writ* wit_u, c3_d pos_d, c3_y* buf_y, c3_y* byt_y, c3_w  len_w, writ_test_cb test_cb);
        void
         u3_rock_write_write(u3_writ* wit_u, c3_d pos_d, c3_y* buf_y, c3_y* byt_y, c3_w  len_w, writ_test_cb test_cb);
        void
         u3_lmdb_write_write(u3_writ* wit_u, c3_d pos_d, c3_y* buf_y, c3_y* byt_y, c3_w  len_w, writ_test_cb test_cb);

        void
        u3_disk_write_shut(u3_pier* pir_u);
        void
        u3_sqlt_write_shut(u3_pier* pir_u);
        void
        u3_fond_write_shut(u3_pier* pir_u);
        void
        u3_rock_write_shut(u3_pier* pir_u);
        void
        u3_lmdb_write_shut(u3_pier* pir_u);

/* TESTING ENTRY POINTS */

c3_o  rein(u3_pier* pir_u, c3_c * pot_c);
c3_o  rere(u3_pier* pir_u, c3_y ** dat_y, c3_w* len_w, void ** opaq_u) ;
void  rede(void * opaq_u) ;
void  resh(u3_pier* pir_u) ;

c3_o wrin(u3_pier* pir_u, c3_c * pot_c) ;
c3_w wrze();
void wric(u3_writ* wit_u, c3_d pos_d, c3_y* buf_y,  c3_y* byt_y, c3_w  len_w, writ_test_cb test_cb);
void wris(u3_pier* pir_u) ;

// debugging for TJIC
extern FILE * ulog;
