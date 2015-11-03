/* include/v/vere.h
**
** This file is in the public domain.
*/
  /** Quasi-tunable parameters.
  **/
    /* First kernel this executable can boot.
    */
#     define FirstKernel   164
#     define DefaultKernel 164

#define RECK

  /** Data types.
  **/
    struct _u3_http;

    /* u3_hhed: http header.
    */
      typedef struct _u3_hhed {
        struct _u3_hhed* nex_u;
        c3_c*            nam_c;
        c3_c*            val_c;
      } u3_hhed;

    /* u3_hbod: http body block.  Also used for responses.
    */
      typedef struct _u3_hbod {
        struct _u3_hbod* nex_u;
        c3_w             len_w;
        c3_y             hun_y[0];
      } u3_hbod;

    /* u3_hrat: http parser state.
    */
      typedef enum {
        u3_hreq_non,
        u3_hreq_nam,
        u3_hreq_val
      } u3_hrat;

    /* u3_csat: client connection state.
    */
      typedef enum {
        u3_csat_dead = 0,                   //  connection dead
        u3_csat_addr = 1,                   //  connection addressed
        u3_csat_clyr = 2,                   //  connection open in cleartext
        u3_csat_crop = 3,                   //  connection open, ssl needs hs
        u3_csat_sing = 4,                   //  connection handshaking ssl
        u3_csat_cryp = 5,                   //  connection open, ssl open
      } u3_csat;

    /* u3_hmet: http method.  Matches jhttp encoding.
    */
      typedef enum {
        u3_hmet_delete,
        u3_hmet_get,
        u3_hmet_head,
        u3_hmet_post,
        u3_hmet_put,
        u3_hmet_nop,                        //  virtual method
        u3_hmet_other                       //  ie, unsupported
      } u3_hmet;

    /* u3_hreq: incoming http request.
    */
      typedef struct _u3_hreq {
        struct _u3_hcon* hon_u;             //  connection
        c3_w             seq_l;             //  sequence within connection
        u3_hmet          met_e;             //  method
        u3_hrat          rat_e;             //  parser state
        void*            par_u;             //  struct http_parser *
        c3_c*            url_c;             //  url
        c3_w             ipf_w;             //  ipv4
        c3_o             liv;               //  keepalive
        c3_o             end;               //  all responses added
        u3_hhed*         hed_u;             //  headers
        u3_hbod*         bod_u;             //  body parts (exit)
        u3_hbod*         dob_u;             //  body parts (entry)
        struct _u3_hreq* nex_u;             //  next in request queue
        u3_hbod*         rub_u;             //  exit of write queue
        u3_hbod*         bur_u;             //  entry of write queue
      } u3_hreq;

    /* u3_hrep: outgoing http response.
    */
      typedef struct _u3_hrep {
        c3_w             sev_l;             //  server number
        c3_w             coq_l;             //  connection number
        c3_w             seq_l;             //  request number
        c3_w             sas_w;             //  status
        u3_hhed*         hed_u;             //  headers
        u3_hbod*         bod_u;             //  body (one part)
      } u3_hrep;

    /* u3_hcon: incoming http connection.
    */
      typedef struct _u3_hcon {
        uv_tcp_t         wax_u;             //  event handler state
        c3_w             coq_l;             //  connection number
        c3_w             seq_l;             //  next request number
        struct _u3_http* htp_u;             //  backlink to server
        struct _u3_hcon* nex_u;             //  next in server's list
        struct _u3_hreq* ruc_u;             //  request under construction
        struct _u3_hreq* req_u;             //  exit of request queue
        struct _u3_hreq* qer_u;             //  entry of request queue
      } u3_hcon;

    /* u3_http: http server.
    */
      typedef struct _u3_http {
        uv_tcp_t         wax_u;             //  event handler state
        c3_w             sev_l;             //  server number
        c3_w             coq_l;             //  next connection number
        c3_w             por_w;             //  running port
        c3_o             sec;               //  logically secure
        struct _u3_hcon* hon_u;             //  connection list
        struct _u3_http* nex_u;             //  next in list
      } u3_http;

    /* u3_cres: response to http client.
    */
      typedef struct _u3_cres {
        u3_hrat          rat_e;             //  parser state
        void*            par_u;             //  struct http_parser *
        c3_w             sas_w;             //  status code
        u3_hhed*         hed_u;             //  headers
        u3_hbod*         bod_u;             //  exit of body queue
        u3_hbod*         dob_u;             //  entry of body queue
      } u3_cres;

    /* u3_creq: outgoing http request.
    */
      typedef struct _u3_creq {             //  client request
        c3_l             num_l;             //  request number
        c3_c*            hot_c;             //  host
        c3_s             por_s;             //  port
        c3_c*            url_c;             //  url
        c3_o             sec;               //  yes == https
        u3_hmet          met_e;             //  method
        u3_hhed*         hed_u;             //  headers
        u3_hbod*         bod_u;             //  body
        u3_cres*         res_u;             //  nascent response
        struct _u3_ccon* coc_u;             //  parent connection
        struct _u3_creq* nex_u;             //  next in queue
      } u3_creq;

    /* u3_sslx: per-connection ssl context.
     */
      typedef struct _u3_sslx {
        void*           ssl_u;              //  struct SSL*
        void*           rio_u;              //  struct BIO* for read
        void*           wio_u;              //  struct BIO* for write
      } u3_sslx;

    /* u3_ccon: outgoing http connection.
    */
      typedef struct _u3_ccon {             //  client connection
        uv_tcp_t         wax_u;             //  i/o handler state
        uv_connect_t     cot_u;             //  connection handler state
        uv_getaddrinfo_t adr_u;             //  resolver state
        u3_sslx          ssl;               //  ssl state
        u3_csat          sat_e;             //  connection state
        c3_c*            hot_c;             //  hostname
        c3_s             por_s;             //  port
        c3_w             ipf_w;             //  IP
        c3_o             sec;               //  yes == https
        u3_hbod*         rub_u;             //  exit of send queue
        u3_hbod*         bur_u;             //  entry of send queue
        u3_creq*         ceq_u;             //  exit of request queue
        u3_creq*         qec_u;             //  entry of request queue
        struct _u3_ccon* pre_u;             //  previous in list
        struct _u3_ccon* nex_u;             //  next in list
      } u3_ccon;

    /* u3_chot: foreign host (not yet used).
    */
      typedef struct _u3_chot {
        c3_w             ipf_w;             //  ip address (or 0)
        c3_c*            hot_c;             //  hostname (no port) (or 0)
        struct _u3_ccon* ins_u;             //  insecure connection (or 0)
        struct _u3_ccon* sec_u;             //  secure connection (or 0)
      } u3_chot;

    /* u3_cttp: http client.
    */
      typedef struct _u3_cttp {
        struct _u3_ccon* coc_u;             //  connection list
      } u3_cttp;

    /* u3_apac: ames packet, coming or going.
    */
      typedef struct _u3_apac {
        struct _u3_apac* nex_u;             //  next in queue
        c3_w             pip_w;             //  IPv4 address, to
        c3_s             por_s;             //  IPv4 port, to
        c3_w             len_w;             //  length in bytes
        c3_y             hun_y[0];          //  data
      } u3_apac;

    /* u3_ames: ames networking.
    */
      typedef struct _u3_ames {             //  packet network state
        union {
          uv_udp_t    wax_u;
          uv_handle_t had_u;
        };
        uv_timer_t    tim_u;                //  network timer
        c3_o          alm;                  //  alarm on
        c3_w          law_w;                //  last wakeup, unix time
        c3_s          por_s;                //  public IPv4 port
        c3_w          imp_w[256];           //  imperial IPs
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
      } u3_utat;

    /* u3_uled: event log header.
    */
      typedef struct {
        c3_l mag_l;                         //  mug of log format, 'a', 'b'...
        c3_w kno_w;                         //  kernel number validated with
        c3_l sal_l;                         //  salt for passcode
        c3_l key_l;                         //  mug of crypto key, or 0
        c3_l sev_l;                         //  host process identity
        c3_l tno_l;                         //  terminal count in host
      } u3_uled;

    /* u3_olar: event log trailer, old version.
    */
      typedef struct {
        c3_w syn_w;                         //  must equal mug of address
        c3_w ent_w;                         //  event sequence number
        c3_w len_w;                         //  word length of this event
        c3_w mug_w;                         //  mug of entry
      } u3_olar;

    /* u3_ular: event log trailer.
    */
      typedef struct {
        c3_w syn_w;                         //  must equal mug of address
        c3_d ent_d;                         //  event sequence number
        c3_w len_w;                         //  word length of this event
        c3_w mug_w;                         //  mug of entry
        c3_w tem_w;                         //  raft term of event
        c3_w typ_w;                         //  type of event, %ra|%ov
      } u3_ular;

    /* u3_ulog: unix event log.
    */
      typedef struct {
        c3_i fid_i;                         //  file descriptor
        c3_d len_d;                         //  length in words
      } u3_ulog;

      struct _u3_umon;
      struct _u3_udir;
      struct _u3_ufil;

    /* u3_unod: file or directory.
    */
      typedef struct _u3_unod {
        uv_fs_event_t     was_u;            //  stat watcher
        c3_o              dir;              //  c3y if dir, c3n if file
        c3_o              dry;              //  ie, unmodified
        c3_c*             pax_c;            //  absolute path
        struct _u3_udir*  par_u;            //  parent
        struct _u3_unod*  nex_u;            //  internal list
      } u3_unod;
      
    /* u3_ufil: synchronized file.
    */
      typedef struct _u3_ufil {
        uv_fs_event_t     was_u;            //  stat watcher
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
        uv_fs_event_t     was_u;            //  stat watcher
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
        uv_check_t  syn_u;                  //  fs sync check
        uv_timer_t  tim_u;                  //  timer
        u3_umon*    mon_u;                  //  mount points
        u3_usig*    sig_u;                  //  signal list
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
        c3_w       run_w;                   //  run of consecutive alarms
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

    /* u3_utel: unix telnet listener.
    */
      typedef struct _u3_utel {
        struct _u3_utty uty_t;             //  pseudo-tty
        c3_s            por_s;             //  file descriptor
        void*           tel_u;             //  telnet context
      } u3_utel;

    /* u3_raty: raft server type.
    */
      typedef enum {
        u3_raty_none,
        u3_raty_foll,
        u3_raty_cand,
        u3_raty_lead
      } u3_raty;

    /* u3_raft: raft state.
    */
      typedef struct {
        uv_tcp_t         wax_u;             //  TCP listener
        uv_timer_t       tim_u;             //  election/heartbeat timer
        u3_ulog          lug_u;             //  event log
        c3_d             ent_d;             //  last log index
        c3_w             lat_w;             //  last log term
        u3_raty          typ_e;             //  server type
        struct _u3_rnam* nam_u;             //  list of peers
        struct _u3_rcon* run_u;             //  unknown connections
        c3_w             pop_w;             //  population count
        c3_w             vot_w;             //  votes in this election
        c3_c*            str_c;             //  our name
        //  persistent state
        c3_w             tem_w;             //  current term
        c3_c*            vog_c;             //  who we voted for this term
        //  end persistent state
      } u3_raft;

    /* u3_rreq: raft request.
    */
      typedef struct _u3_rreq {
        struct _u3_rmsg* msg_u;
        struct _u3_rreq* nex_u;
        struct _u3_rcon* ron_u;
      } u3_rreq;

    /* u3_rbuf: raft input buffer.
    */
      typedef struct _u3_rbuf {
        c3_w                len_w;
        c3_w                cap_w;
        c3_y                buf_y[0];
      } u3_rbuf;

    /* u3_rcon: raft connection.
    */
      typedef struct _u3_rcon {
        uv_tcp_t         wax_u;             //  TCP handle
        struct _u3_rnam* nam_u;             //  peer we're connected to
        u3_rbuf*         red_u;             //  read buffer
        c3_o             red;               //  u3_yes on new data
        u3_rbuf*         wri_u;             //  write buffer
        u3_raft*         raf_u;             //  back-reference to server
        u3_rreq*         out_u;             //  exit of request queue
        u3_rreq*         tou_u;             //  entry of request queue
        struct _u3_rcon* nex_u;             //  pointer to next con
        c3_o             liv;               //  are we live?
      } u3_rcon;

    /* u3_rnam: raft peer name.
    */
      typedef struct _u3_rnam {
        c3_c*            str_c;             //  our name
        c3_c*            nam_c;             //  hostname
        c3_c*            por_c;             //  port
        u3_rcon*         ron_u;             //  connection
        struct _u3_rnam* nex_u;             //  pointer to next peer
        c3_o             vog;               //  did they vote for us?
      } u3_rnam;

    /* u3_opts: command line configuration.
    */
      typedef struct _u3_opts {
        c3_c*   imp_c;                      //  -I, czar name
        c3_c*   nam_c;                      //  -n, unix hostname
        c3_c*   raf_c;                      //  -r, raft flotilla
        c3_c*   who_c;                      //  -T, begin with ticket
        c3_c*   tic_c;                      //  -T, ticket value
        c3_w    kno_w;                      //  -k, kernel version
        c3_w    fuz_w;                      //  -f, fuzz testing
        c3_s    por_s;                      //  -p, ames port
        c3_s    rop_s;                      //  -l, raft port
        c3_o    abo;                        //  -a
        c3_o    bat;                        //  -b, batch create
        c3_o    gab;                        //  -g
        c3_o    dem;                        //  -d, daemon
        c3_o    dry;                        //  -D, dry compute  
        c3_o    tex;                        //  -x, exit after loading
        c3_o    fog;                        //  -X, skip last event
        c3_o    fak;                        //  -F, fake carrier
        c3_o    loh;                        //  -L, local-only networking
        c3_o    pro;                        //  -P, profile
        c3_o    veb;                        //  -v, verbose (inverse of -q)
        c3_o    nuu;                        //  -c, new pier
        c3_o    qui;                        //  -q, quiet
        c3_o    vno;                        //  -V, turn on +verb
        c3_o    mem;                        //  -M, memory madness
      } u3_opts;

    /* u3_host: entire host.
    */
      typedef struct _u3_host {
        c3_w       kno_w;                   //  current executing stage
        c3_c*      dir_c;                   //  pier path (no trailing /)
        c3_d       now_d;                   //  event tick
        uv_loop_t* lup_u;                   //  libuv event loop
        u3_http*   htp_u;                   //  http servers
        u3_cttp    ctp_u;                   //  http clients
        u3_utel    tel_u;                   //  telnet listener
        u3_utty*   uty_u;                   //  linked terminal list
        u3_ames    sam_u;                   //  packet interface
        u3_save    sav_u;                   //  autosave
        u3_opts    ops_u;                   //  commandline options
        u3_unix    unx_u;                   //  sync and clay
        u3_behn    teh_u;                   //  behn timer
        c3_o       liv;                     //  if u3_no, shut down
        c3_i       xit_i;                   //  exit code for shutdown
        void*      ssl_u;                   //  struct SSL_CTX*
      } u3_host;                            //  host == computer == process

#     define u3L  u3_Host.lup_u             //  global event loop
#     define u3Z  (&(u3_Raft))
#     define u3S  u3_Host.ssl_u

  /** Global variables.
  **/
    c3_global  u3_host  u3_Host;
    c3_global  u3_raft  u3_Raft;
    c3_global  c3_c*    u3_Local;
    c3_global  c3_c*    u3_System;

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
    **  New leap seconds after July 2012 (leap second 25) are ignored.  The
    **  platform OS will not ignore them, of course, so they must be detected
    **  and counteracted.  Perhaps this phenomenon will soon find an endpoint.
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
        u3_walk_save(c3_c* pas_c, u3_noun tim, u3_atom pad);

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

    /**  Output.
    **/
      /* u3_ve_tank(): print a tank at `tab`.
      */
        void
        u3_ve_tank(c3_l tab_l, u3_noun tac);

      /* u3_reck_kick(): handle effect.
      */
        void
        u3_reck_kick(u3_noun ovo);


    /**  Main loop, new style.
    **/
      /* u3_lo_loop(): enter main event loop.
      */
        void
        u3_lo_loop(void);

      /* u3_lo_lead(): actions on promotion to leader.
      */
        void
        u3_lo_lead(void);

      /* u3_lo_exit(): shut down io across pier.
      */
        void
        u3_lo_exit(void);

      /* u3_lo_show(): print typeless noun.
      */
        void
        u3_lo_show(c3_c* cap_c, u3_noun nun);
#define   u3ls(cap_c, nun) u3_lo_show(cap_c, nun)

      /* u3_lo_bail(): clean up all event state.
      */
        void
        u3_lo_bail(void);

      /* u3_lo_tank(): dump single tank.
      */
        void
        u3_lo_tank(c3_l tab_l, u3_noun tac);

      /* u3_lo_punt(): dump tank list.
      */
        void
        u3_lo_punt(c3_l tab_l, u3_noun tac);

      /* u3_lo_sway(): print trace.
      */
        void
        u3_lo_sway(c3_l tab_l, u3_noun tax);

      /* u3_lo_grab(): garbage-collect the world, plus roots; end with u3_none
      */
        void
        u3_lo_grab(c3_c* cap_c, u3_noun som, ...);

      /* u3_lo_open(): begin callback processing.
      */
        void
        u3_lo_open(void);

      /* u3_lo_shut(): end callback processing.
      */
        void
        u3_lo_shut(c3_o);


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

      /* u3_term_ef_ticket(): initial effects for new ticket.
      */
        void
        u3_term_ef_ticket(c3_c* who_c, c3_c* tic_c);

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
        u3_term_ef_bake(u3_noun  fav);

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

      /* u3_term_io_poll(): update terminal IO state.
      */
        void
        u3_term_io_poll(void);

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
        u3_ames_ef_bake(void);

      /* u3_ames_ef_send(): send packet to network.
      */
        void
        u3_ames_ef_send(u3_noun lan,
                        u3_noun pac);

      /* u3_ames_io_init(): initialize ames I/O.
      */
        void
        u3_ames_io_init(void);

      /* u3_ames_io_talk(): bring up listener.
      */
        void
        u3_ames_io_talk(void);

      /* u3_ames_io_exit(): terminate ames I/O.
      */
        void
        u3_ames_io_exit(void);

      /* u3_ames_io_poll(): update ames IO state.
      */
        void
        u3_ames_io_poll(void);

    /**  Autosave.
    **/
      /* u3_save_ef_chld(): report SIGCHLD.
      */
        void
        u3_save_ef_chld(void);

      /* u3_save_io_init(): initialize autosave.
      */
        void
        u3_save_io_init(void);

      /* u3_save_io_exit(): terminate autosave.
      */
        void
        u3_save_io_exit(void);

      /* u3_save_io_poll(): update autosave state.
      */
        void
        u3_save_io_poll(void);

    /**  Storage, new school.
    **/
      /* u3_unix_ef_hold():
      */
        void
        u3_unix_ef_hold();

      /* u3_unix_ef_move():
      */
        void
        u3_unix_ef_move();

      /* u3_unix_initial_into(): intialize filesystem from urb/zod
      */
        void
        u3_unix_ef_initial_into();

      /* u3_unix_ef_look(): update filesystem from unix
      */
        void
        u3_unix_ef_look(u3_noun all);

      /* u3_unix_ef_ergo(): update filesystem from urbit
      */
        void
        u3_unix_ef_ergo(u3_noun mon, u3_noun can);

      /* u3_unix_ef_ogre(): delete mount point
      */
        void
        u3_unix_ef_ogre(u3_noun mon);

      /* u3_unix_ef_hill(): enumerate mount points
      */
        void
        u3_unix_ef_hill(u3_noun hil);

      /* u3_unix_io_init(): initialize storage.
      */
        void
        u3_unix_io_init(void);

      /* u3_unix_io_talk(): start listening for fs events.
      */
        void
        u3_unix_io_talk(void);

      /* u3_unix_io_exit(): terminate storage.
      */
        void
        u3_unix_io_exit(void);

      /* u3_unix_io_poll(): update storage state.
      */
        void
        u3_unix_io_poll(void);


    /**  behn, just a timer.
    **/
      /* u2_behn_io_init(): initialize behn timer.
      */
        void
        u2_behn_io_init(void);

      /* u2_behn_io_exit(): terminate timer.
      */
        void
        u2_behn_io_exit(void);

      /* u2_behn_io_poll(): update behn IO state.
      */
        void
        u2_behn_io_poll(void);


    /**  HTTP server.
    **/
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

      /* u3_http_io_poll(): update http IO state.
      */
        void
        u3_http_io_poll(void);

    /** Raft log syncing.
    **/
      /* u3_raft_readopt(): parse command line options.
      */
        u3_rnam*
        u3_raft_readopt(const c3_c* arg_c, c3_c* our_c, c3_s oup_s);

      /* u3_raft_init(): start Raft process.
      */
        void
        u3_raft_init(void);

      /* u3_raft_work(): poke, kick, and push pending events.
      */
        void
        u3_raft_work(void);


    /**  Disk persistence.
    **/
      /* u3_sist_boot(): restore or create pier from disk.
      */
        void
        u3_sist_boot(void);

      /* u3_sist_pack(): write a log entry to disk.
      **
      ** XX Synchronous.
      **
      ** typ_w is a mote describing the entry type: %ov for Arvo
      ** logs, %ra for Raft events.
      **
      ** Returns the entry's sequence number.
      */
        c3_d
        u3_sist_pack(c3_w tem_w,
                     c3_w typ_w,
                     c3_w* bob_w,
                     c3_w len_w);

      /* u3_sist_put(): moronic key-value store put.
      **
      ** u3_sist_put will do its best to associate the passed key with
      ** the passed value in a way that will persist across process
      ** restarts. It will probably do so by writing a file named for
      ** the key with contents identical to the value. To rely on it
      ** for anything heavy-duty would be a mistake.
      **
      ** Why would we even have something like this? Because sometimes
      ** we need to maintain files completely independently of the
      ** noun state.
      */
        void
        u3_sist_put(const c3_c* key_c, const c3_y* val_y, size_t siz_i);

      /* u3_sist_nil(): moronic key-value store rm.
      **
      ** Does its best to expunge all records on the given key. Has
      ** no effect if the key doesn't exist.
      */
        void
        u3_sist_nil(const c3_c* key_c);

      /* u3_sist_has(): moronic key-value store existence check.
      **
      ** Returns the byte length of the value previously stored via
      ** u3_sist_put, or -1 if it couldn't find one.
      */
        ssize_t
        u3_sist_has(const c3_c* key_c);

      /* u3_sist_get(): moronic key-value store get.
      **
      ** u3_sist_get is the mirror of u3_sist_put. It writes to val_y,
      ** which had better be at least as big as the return value from
      ** u3_sist_has, the value that you previously put.
      **
      ** Needless to say, u3_sist_get crashes if it can't find your
      ** value.
      */
        void
        u3_sist_get(const c3_c* key_c, c3_y* val_y);

      /* u3_sist_rand(): fill 8 words (32 bytes) with high-quality entropy.
      */
        void
        u3_sist_rand(c3_w* rad_w);

    /**  New timer system.
    **/
      /* u3_behn_io_init(): initialize time timer.
      */
        void
        u3_behn_io_init(void);

      /* u3_behn_io_exit(): terminate timer.
      */
        void
        u3_behn_io_exit(void);

      /* u3_behn_io_poll(): update behn IO state.
      */
        void
        u3_behn_io_poll(void);


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

      /* u3_cttp_io_poll(): update cttp IO state.
      */
        void
        u3_cttp_io_poll(void);
