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
    struct _u2_http;

    /* u2_hhed: http header.
    */
      typedef struct _u2_hhed {
        struct _u2_hhed* nex_u;
        c3_c*            nam_c;
        c3_c*            val_c;
      } u2_hhed;

    /* u2_hbod: http body block.  Also used for responses.
    */
      typedef struct _u2_hbod {
        struct _u2_hbod* nex_u; 
        c3_w             len_w;
        c3_y             hun_y[0];
      } u2_hbod;

    /* u2_hrat: http parser state.
    */
      typedef enum {
        u2_hreq_non,                    
        u2_hreq_nam,
        u2_hreq_val
      } u2_hrat;

    /* u2_hmet: http method.  Matches jhttp encoding.
    */
      typedef enum {
        u2_hmet_delete,
        u2_hmet_get,
        u2_hmet_head,
        u2_hmet_post,
        u2_hmet_put,
        u2_hmet_other                       //  ie, unsupported
      } u2_hmet;
 
    /* u2_hreq: incoming http request.
    */
      typedef struct _u2_hreq {
        struct _u2_hcon* hon_u;             //  connection
        c3_w             seq_l;             //  sequence within connection
        u2_hmet          met_e;             //  method
        u2_hrat          rat_e;             //  parser state
        void*            par_u;             //  struct http_parser *
        c3_c*            url_c;             //  url
        u2_bean          liv;               //  keepalive
        u2_bean          end;               //  all responses added
        u2_hhed*         hed_u;             //  headers 
        u2_hbod*         bod_u;             //  body parts
        struct _u2_hreq* nex_u;             //  next in request queue
        u2_hbod*         rub_u;             //  exit of write queue
        u2_hbod*         bur_u;             //  entry of write queue
      } u2_hreq;

    /* u2_hrep: outgoing http response.
    */
      typedef struct _u2_hrep {
        c3_w             coq_l;             //  connection number
        c3_w             seq_l;             //  request number
        c3_w             sas_w;             //  status
        u2_hhed*         hed_u;             //  headers
        u2_hbod*         bod_u;             //  body (one part)
      } u2_hrep;

    /* u2_hcon: incoming http connection.
    */
      typedef struct _u2_hcon {
        uv_tcp_t         wax_u;             //  event handler state
        c3_w             coq_l;             //  connection number
        c3_w             seq_l;             //  next request number
        struct _u2_http* htp_u;             //  backlink to server 
        struct _u2_hcon* nex_u;             //  next in server's list
        struct _u2_hreq* ruc_u;             //  request under construction
        struct _u2_hreq* req_u;             //  exit of request queue
        struct _u2_hreq* qer_u;             //  entry of request queue
      } u2_hcon;

    /* u2_http: http server.
    */
      typedef struct _u2_http {
        uv_tcp_t         wax_u;             //  event handler state
        c3_w             sev_l;             //  server number
        c3_w             coq_l;             //  next connection number
        c3_w             por_w;             //  running port
        struct _u2_hcon* hon_u;             //  connection list
        struct _u2_http* nex_u;             //  next in list
      } u2_http;

    /* u2_creq: outgoing http request.
    */
      typedef struct _u2_creq {             //  client request
        c3_l             num_l;             //  connection number
        c3_c*            url_c;             //  url
        u2_hmet          met_e;             //  method
        u2_hhed*         hed_u;             //  headers
        u2_hbod*         bod_u;             //  body
        struct _u2_creq* nex_u;             //  next in queue
      } u2_creq;

    /* u2_cres: response to http client.
    */
      typedef struct _u2_cres {
        u2_hrat          rat_e;             //  parser state
        void*            par_u;             //  struct http_parser *
        c3_c*            url_c;             //  url
        u2_bean          end;               //  all responses added
        u2_hhed*         hed_u;             //  headers 
        u2_hbod*         bod_u;             //  body parts
        struct _u2_cres* nex_u;             //  next in response queue
      } u2_cres;

    /* u2_ccon: outgoing http connection.
    */
      typedef struct _u2_ccon {             //  client connection
        uv_tcp_t         wax_u;             //  i/o handler state
        uv_connect_t     cot_u;             //  connection handler state 
        c3_w             las_w;             //  last active (Unix time)
        c3_w             coq_l;             //  connection number
        c3_c*            hos_c;             //  hostname
        c3_s             por_s;             //  port
        u2_bean          sec;               //  yes == https
        u2_hbod*         rub_u;             //  exit of send queue
        u2_hbod*         bur_u;             //  entry of send queue
        u2_creq*         ceq_u;             //  exit of request queue
        u2_creq*         qec_u;             //  entry of request queue
        u2_cres*         res_u;             //  current response
        struct _u2_ccon* pre_u;             //  previous in list
        struct _u2_ccon* nex_u;             //  next in list
      } u2_ccon;

    /* u2_cttp: http client.
    */
      typedef struct _u2_cttp {
        struct _u2_ccon* coc_u;             //  connection list
      } u2_cttp;

    /* u2_apac: ames packet, coming or going.
    */
      typedef struct _u2_apac {
        struct _u2_apac* nex_u;             //  next in queue
        c3_w             pip_w;             //  IPv4 address, to
        c3_s             por_s;             //  IPv4 port, to
        c3_w             len_w;             //  length in bytes
        c3_y             hun_y[0];          //  data
      } u2_apac;

    /* u2_ames: ames networking.
    */
      typedef struct _u2_ames {             //  packet network state
        uv_udp_t   wax_u;                   //  socket state
        uv_timer_t tim_u;                   //  network timer
        u2_bean    alm;                     //  alarm on
        c3_s       por_s;                   //  public IPv4 port
        c3_w       imp_w[256];              //  imperial IPs
      } u2_ames;

    /* u2_save: checkpoint control.
    */
      typedef struct _u2_save {
        uv_timer_t  tim_u;                  //  checkpoint timer
        uv_signal_t sil_u;                  //  child signal
        c3_w        ent_w;                  //  event number, XX 64
        c3_w        pid_w;                  //  pid of checkpoint process
      } u2_save;

    /* u2_ubuf: unix tty i/o buffer.
    */
      typedef struct _u2_ubuf {
        struct _u2_ubuf* nex_u; 
        c3_w             len_w;
        c3_y             hun_y[0];          //  bytes to send
      } u2_ubuf;

    /* u2_utat: unix terminal state.
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
          u2_bean ape;                      //  escape received 
          u2_bean bra;                      //  bracket or O received
        } esc;

        struct {
          c3_y syb_y[5];                    //  utf8 code buffer
          c3_w len_w;                       //  present length
          c3_w wid_w;                       //  total width
        } fut;
      } u2_utat;

    /* u2_uled: event log header.
    */
      typedef struct {
        c3_l mag_l;                         //  mug of log format, 'a', 'b'...
        c3_w kno_w;                         //  kernel number validated with
        c3_l sal_l;                         //  salt for passcode
        c3_l key_l;                         //  mug of crypto key, or 0
        c3_l sev_l;                         //  host process identity
        c3_l tno_l;                         //  terminal count in host
      } u2_uled;

    /* u2_ular: event log trailer.
    */
      typedef struct {
        c3_w syn_w;                         //  must equal mug of address
        c3_w ent_w;                         //  event sequence number
        c3_w len_w;                         //  word length of this event
        c3_w mug_w;                         //  mug of noun
      } u2_ular;

    /* u2_ulog: unix event log.
    */
      typedef struct {
        c3_i fid_i;                         //  file descriptor
        c3_d len_d;                         //  length in words
      } u2_ulog;

      struct _u2_uhot;
      struct _u2_udir;
      struct _u2_ufil;

    /* u2_unod: file or directory.
    */
      typedef struct _u2_unod {
        uv_fs_event_t    was_u;             //  stat watcher
        u2_bean          dir;               //  always 
        u2_bean          dry;               //  ie, unmodified
        c3_c*            pax_c;             //  absolute path
        struct _u2_udir* par_u;             //  in directory
      } u2_unod;

    /* u2_ufil: synchronized file.
    */
      typedef struct _u2_ufil {
        uv_fs_event_t    was_u;             //  stat watcher
        u2_bean          non;               //  always u2_no
        u2_bean          dry;               //  ie, unmodified
        c3_c*            pax_c;             //  absolute path
        struct _u2_udir* par_u;             //  in directory
        c3_c*            dot_c;             //  extension point or 0
        mpz_t            mod_mp;            //  mtime as @da
        struct _u2_ufil* nex_u;             //  internal list
      } u2_ufil;

    /* u2_udir: synchronized directory.
    */
      typedef struct _u2_udir {
        uv_fs_event_t    was_u;             //  stat watcher
        u2_bean          yes;               //  always u2_yes
        u2_bean          dry;               //  ie, unmodified
        c3_c*            pax_c;             //  absolute path
        struct _u2_udir* par_u;             //  parent directory
        struct _u2_udir* dis_u;             //  subdirectories
        struct _u2_ufil* fil_u;             //  files
        struct _u2_udir* nex_u;             //  internal list
      } u2_udir;

    /* u2_uhot: synchronized host.
    */
      typedef struct _u2_uhot {
        u2_udir          dir_u;             //  root directory
        mpz_t            who_mp;            //  owner as GMP
        struct _u2_uhot* nex_u;             //  internal list
      } u2_uhot;

    /* u2_usig: receive signals.
    */
      typedef struct _u2_usig {
        uv_signal_t      sil_u;
        c3_i             num_i;
        struct _u2_usig* nex_u;
      } u2_usig;

    /* u2_unix: clay support system, also 
    */
      typedef struct _u2_unix {
        uv_timer_t tim_u;                   //  clay timer
        u2_bean    alm;                     //  alarm
        u2_uhot*   hot_u;                   //  host state
        u2_usig*   sig_u;                   //  signal list
      } u2_unix;

    /* u2_batz: just a timer for now
    */
      typedef struct _u2_batz {
        uv_timer_t tim_u;                   //  clay timer
        c3_w       run_w;                   //  run of consecutive alarms
        u2_bean    alm;                     //  alarm
      } u2_batz;

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
      } u2_utfo;

#if 0
    /* u2_uwen: unix alarm.
    */
      typedef struct _u2_uwen {
        c3_y* pax_y;                        //  printed path
        c3_d  wen_d[2];                     //  timer expire
      };

    /* u2_utim: unix timer control.
    */
      typedef struct _u2_utim {
        uv_timer_t wat_u;              //  libev timer control
        u2_uwen*        wen_u;              //  timers in ascending order
      };
#endif

    /* u2_utty: unix tty.
    */
      typedef struct _u2_utty {
#if 0
        uv_tty_t         wax_u;             //  event handler state
#else
        union {
          uv_poll_t        wax_u;
          uv_pipe_t        pop_u;
        };
#endif
        struct termios   bak_u;             //  cooked terminal state
        struct termios   raw_u;             //  raw terminal state
        c3_i             fid_i;             //  file descriptor
#     if 1
        c3_i             cug_i;             //  blocking fcntl flags
        c3_i             nob_i;             //  nonblocking fcntl flags
#     endif
        c3_w             tid_l;             //  terminal identity number
        u2_utfo          ufo_u;             //  terminfo strings
#     if  1
        u2_ubuf*         tou_u;             //  entry of output queue
        u2_ubuf*         out_u;             //  exit of output queue
#     endif
        u2_utat          tat_u;             //  control state
        struct _u2_utty* nex_u;             //  next in host list
      } u2_utty;

    /* u2_raft: raft state.
    */
      typedef struct {
        uv_tcp_t   wax_u;
        uv_timer_t tim_u;
        u2_ulog    lug_u;                   //  event log
      } u2_raft;

    /* u2_rnam: raft peer name.
    */
      typedef struct _u2_rnam {
        c3_c* str_c;
        c3_c* nam_c;
        c3_s  por_s;
        struct _u2_rnam* nex_u;
      } u2_rnam;

    /* u2_ropt: raft options.
    */
      typedef struct {
        u2_rnam* nam_u;
        c3_s     por_s;
      } u2_ropt;

    /* u2_opts:
    */
      typedef struct _u2_opts {
        c3_c*   cpu_c;
        c3_c*   imp_c;
        c3_c*   hom_c; 
        c3_c*   nam_c;
        c3_w    kno_w;
        c3_w    fuz_w;
        c3_s    por_s;
        u2_ropt rop_u;
        u2_bean abo;
        u2_bean bat;
        u2_bean gab;
        u2_bean dem;
        u2_bean loh;
        u2_bean pro;
        u2_bean veb;
        u2_bean pas;
        u2_bean rez;
        u2_bean sow;
        u2_bean nuu;
        u2_bean vno;
      } u2_opts;

    /* u2_host: entire host.
    */
      typedef struct _u2_host {
        u2_wire    wir_r;                   //  noun system, 1 per thread
        c3_w       kno_w;                   //  current executing stage
        c3_c*      cpu_c;                   //  computer path

        c3_d       now_d;                   //  event tick
        uv_loop_t* lup_u;                   //  libuv event loop
        u2_http*   htp_u;                   //  http servers
        u2_cttp*   ctp_u;                   //  http connections
        u2_utty*   uty_u;                   //  all terminals 
        u2_utty*   tem_u;                   //  main terminal (1)
        u2_ames    sam_u;                   //  packet interface
        u2_save    sav_u;                   //  autosave
        u2_opts    ops_u;                   //  commandline options
        u2_unix    unx_u;                   //  sync and clay
        u2_batz    beh_u;                   //  batz timer
        u2_bean    liv;                     //  if u2_no, shut down

        u2_reck*   arv_u;                   //  runtime
      } u2_host;                            //  host == computer == process

#     define u2L  u2_Host.lup_u             //  global event loop
#     define u2R  (&(u2_Raft))

    /* u2_funk: standard system function.
    */
      typedef u2_noun (*u2_funk)(u2_reck* rec_u, u2_noun);

  /** Global variables.
  **/
    c3_global  u2_host  u2_Host;
    c3_global  u2_wire  u2_Wire;
    c3_global  u2_raft  u2_Raft;
    c3_global  c3_c*    u2_Local;
    c3_global  c3_c*    u2_System;

    c3_global  u2_bean  u2_Flag_Abort;
    c3_global  u2_bean  u2_Flag_Garbage;
    c3_global  u2_bean  u2_Flag_Profile;
    c3_global  u2_bean  u2_Flag_Verbose;

#   define u2_ve_at() ( &u2_Host.ver_e[u2_Host.kno_w] )


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
      /* u2_time_sec_in(): urbit seconds from unix time.  
      **
      ** Adjust (externally) for future leap secs!
      */
        c3_d
        u2_time_sec_in(c3_w unx_w);

      /* u2_time_sec_out(): unix time from urbit seconds.  
      **
      ** Adjust (externally) for future leap secs!
      */
        c3_w
        u2_time_sec_out(c3_d urs_d);

      /* u2_time_fsc_in(): urbit fracto-seconds from unix microseconds.
      */
        c3_d
        u2_time_fsc_in(c3_w usc_w);

      /* u2_time_fsc_out: unix microseconds from urbit fracto-seconds.
      */
        c3_w
        u2_time_fsc_out(c3_d ufc_d);

      /* u2_time_in_tv(): urbit time from struct timeval.
      */
        u2_atom
        u2_time_in_tv(struct timeval* tim_tv);

      /* u2_time_out_tv(): struct timeval from urbit time.
      */
        void
        u2_time_out_tv(struct timeval* tim_tv, u2_noun now);

      /* u2_time_in_ts(): urbit time from struct timespec.
      */
        u2_atom
        u2_time_in_ts(struct timespec* tim_ts);
#if defined(U2_OS_linux)
      /* u2_time_t_in_ts(): urbit time from time_t.
       */
         u2_atom
         u2_time_t_in_ts(time_t tim);
#endif

      /* u2_time_out_ts(): struct timespec from urbit time.
      */
        void
        u2_time_out_ts(struct timespec* tim_ts, u2_noun now);

      /* u2_time_gap_ms(): (wen - now) in ms.
      */
        c3_d
        u2_time_gap_ms(u2_noun now, u2_noun wen);

    /**  Filesystem (new api).
    **/
      /* u2_walk_load(): load file or bail.
      */
        u2_noun
        u2_walk_load(c3_c* pas_c);

      /* u2_walk_safe(): load file or 0.
      */
        u2_noun
        u2_walk_safe(c3_c* pas_c);

      /* u2_walk_save(): save file or bail.
      */
        void
        u2_walk_save(c3_c* pas_c, u2_noun tim, u2_atom pad);

      /* u2_sync_reck(): traverse filesystem for changes -> lamb
      */
        u2_noun
        u2_sync_reck(u2_reck* rec_u);

      /* u2_walk(): traverse `dir_c` to produce an arch, updating `old`.
      */
        u2_noun
        u2_walk(u2_reck* rec_u, const c3_c* dir_c, u2_noun old);

      /* u2_path(): C unix path in computer for file or directory. 
      */
        c3_c*
        u2_path(u2_bean fyl, u2_noun pax);

    /**  Filesystem (old api).
    **/
      /* u2_ve_file(): load internal file as atom from local or system.
      */
        u2_weak
        u2_ve_file(c3_c* ext_c, u2_noun tah);

      /* u2_ve_frep(): load [.~ %rep myp {now} tah].
      **
      **   File is either ~ or [nbytes mdate atom].
      */
        u2_noun
        u2_ve_frep(u2_noun myp, u2_noun tah);

      /* u2_ve_date(): date internal file.
      */
        c3_d
        u2_ve_date(c3_c* ext_c, u2_noun tah);

      /* u2_ve_save(): save internal file as atom.
      */
        u2_bean
        u2_ve_save(c3_c* ext_c, u2_noun tah, u2_noun dat);

      /* u2_ve_zeus(): prayer to internal file path.  Return unit.
      */
        u2_noun
        u2_ve_zeus(u2_noun hap);

    /**  Output.
    **/
      /* u2_ve_tank(): print a tank at `tab`.
      */
        void
        u2_ve_tank(c3_l tab_l, u2_noun tac);


    /**  Kernel control.
    **/
      /* u2_reck_do(): use a kernel function.
      */
#       define u2_do(t, a)              u2_reck_do(t, a)
#       define u2_dc(t, a1, a2)         u2_reck_do(t, u2nc(a1, a2))
#       define u2_dt(t, a1, a2, a3)     u2_reck_do(t, u2nt(a1, a2, a3))
#       define u2_dq(t, a1, a2, a3, a4) u2_reck_do(t, u2nq(a1, a2, a3, a4))

        u2_noun
        u2_reck_do(const c3_c* txt_c, u2_noun arg);

      /* u2_reck_line(): apply a reck line (protected).
      */
        void
        u2_reck_line(u2_reck* rec_u, u2_noun lin);

      /* u2_reck_numb(): set the instance number.
      */
        void
        u2_reck_numb(u2_reck* rec_u);

      /* u2_reck_http_request(): hear http request on channel.
      */
        void
        u2_reck_http_request(u2_reck* rec_u, 
                             u2_bean  sec,
                             u2_noun  pox, 
                             u2_noun  req);

      /* u2_reck_http_respond(): apply http response.
      */
        void
        u2_reck_http_respond(u2_reck* rec_u, u2_noun pox, u2_noun rep);

      /* u2_reck_boot(): boot the reck engine (unprotected).
      */
        void
        u2_reck_boot(u2_reck* rec_u);

      /* u2_reck_launch(): launch the reck engine (unprotected).
      */
        u2_bean
        u2_reck_launch(u2_reck* rec_u);

      /* u2_reck_peek(): query the reck namespace (protected).
      */
        u2_noun
        u2_reck_peek(u2_reck* rec_u, u2_noun hap);

      /* u2_reck_keep(): measure timer.
      */
        u2_noun 
        u2_reck_keep(u2_reck* rec_u, u2_noun hap);

      /* u2_reck_poke(): insert and apply an input ovum (protected).
      */
        u2_noun
        u2_reck_poke(u2_reck* rec_u, u2_noun ovo);

      /* u2_reck_prick(): query the reck namespace (unprotected).
      */
        u2_noun
        u2_reck_prick(u2_reck* rec_u, u2_noun hap);

      /* u2_reck_kick(): handle effect.
      */
        void
        u2_reck_kick(u2_reck* rec_u, u2_noun ovo);

      /* u2_reck_sync(): poll and apply sync events (protected).
      */
        void
        u2_reck_sync(u2_reck* rec_u);

      /* u2_reck_time(): set the reck time.
      */
        void
        u2_reck_time(u2_reck* rec_u);
     
      /* u2_reck_wind(): set the reck time artificially.
      */
        void
        u2_reck_wind(u2_reck* rec_u, u2_noun now);

      /* u2_reck_plan(): queue ovum (external).
      */
        void
        u2_reck_plan(u2_reck* rec_u,
                     u2_noun  pax,
                     u2_noun  fav);

      /* u2_reck_plow(): queue ovum list in order (external).
      */
        void
        u2_reck_plow(u2_reck* rec_u, u2_noun ova);

      /* u2_reck_work(): flush ova (unprotected).
      */
        void
        u2_reck_work(u2_reck* rec_u);


    /**  Main loop, new style.
    **/
      /* u2_lo_boot(): restore or create pier.
      */
        void
        u2_lo_boot(void);

      /* u2_lo_loop(): enter main event loop.
      */
        void
        u2_lo_loop(u2_reck* rec_u);

      /* u2_lo_show(): print typeless noun.
      */
        void
        u2_lo_show(c3_c* cap_c, u2_noun nun);
#define   u2ls(cap_c, nun) u2_lo_show(cap_c, nun)

      /* u2_lo_bail(): clean up all event state.
      */
        void
        u2_lo_bail(u2_reck* rec_u);

      /* u2_lo_tank(): dump single tank.
      */
        void
        u2_lo_tank(c3_l tab_l, u2_noun tac);

      /* u2_lo_punt(): dump tank list.
      */
        void
        u2_lo_punt(c3_l tab_l, u2_noun tac);

      /* u2_lo_sway(): print trace.
      */
        void
        u2_lo_sway(c3_l tab_l, u2_noun tax);

      /* u2_lo_grab(): garbage-collect the world, plus roots; end with u2_none
      */
        void
        u2_lo_grab(c3_c* cap_c, u2_noun som, ...);

      /* u2_lo_open(): begin callback processing.
      */
        void
        u2_lo_open(void);

      /* u2_lo_shut(): end callback processing.
      */
        void
        u2_lo_shut(u2_bean);


    /**  Terminal, new style.
    **/
      /* u2_term_get_blew(): return window size [columns rows].
      */
        u2_noun
        u2_term_get_blew(c3_l tid_l);

      /* u2_term_ef_boil(): initial effects for restored server.
      */
        void
        u2_term_ef_boil(c3_l ono_l);

      /* u2_term_ef_winc(): window change.
      */
        void
        u2_term_ef_winc(void);

      /* u2_term_ef_ctlc(): send ^C.
      */
        void
        u2_term_ef_ctlc(void);
 
      /* u2_term_ef_bake(): initial effects for new server.
      */
        void
        u2_term_ef_bake(u2_noun  fav);

      /* u2_term_ef_blit(): send %blit effect to to terminal.
      */
        void
        u2_term_ef_blit(c3_l    tid_l,
                        u2_noun blt);

      /* u2_term_io_init(): initialize terminal I/O.
      */
        void 
        u2_term_io_init(void);

      /* u2_term_io_exit(): terminate terminal I/O.
      */
        void 
        u2_term_io_exit(void);

      /* u2_term_io_poll(): update terminal IO state.
      */
        void
        u2_term_io_poll(void);
        
      /* u2_term_io_hija(): hijack console for cooked print.
      */
        FILE*
        u2_term_io_hija(void);

      /* u2_term_io_loja(): release console from cooked print.
      */
        void
        u2_term_io_loja(int x);

      /* uL, uH: wrap hijack/lojack around fprintf.
      **
      **  uL(fprintf(uH, ...));
      */
#       define uH    u2_term_io_hija()
#       define uL(x) u2_term_io_loja(x)


    /**  Ames, packet networking.
    **/
      /* u2_ames_ef_send(): send packet to network.
      */
        void
        u2_ames_ef_send(u2_noun lan,
                        u2_noun pac);

      /* u2_ames_io_init(): initialize ames I/O.
      */
        void 
        u2_ames_io_init(void);

      /* u2_ames_io_talk(): bring up listener.
      */
        void
        u2_ames_io_talk(void);

      /* u2_ames_io_exit(): terminate ames I/O.
      */
        void 
        u2_ames_io_exit(void);

      /* u2_ames_io_poll(): update ames IO state.
      */
        void
        u2_ames_io_poll(void);

    /**  Autosave.
    **/
      /* u2_save_ef_chld(): report SIGCHLD.
      */
        void
        u2_save_ef_chld(void);

      /* u2_save_io_init(): initialize autosave.
      */
        void 
        u2_save_io_init(void);

      /* u2_save_io_exit(): terminate autosave.
      */
        void 
        u2_save_io_exit(void);

      /* u2_save_io_poll(): update autosave state.
      */
        void
        u2_save_io_poll(void);

    /**  Storage, new school.
    **/
      /* u2_unix_ef_hold(): 
      */
        void
        u2_unix_ef_hold();

      /* u2_unix_ef_move():
      */
        void
        u2_unix_ef_move();

      /* u2_unix_ef_look(): update filesystem, inbound.
      */
        void
        u2_unix_ef_look(void);

      /* u2_unix_ef_init(): update filesystem for new acquisition.
      */
        void
        u2_unix_ef_init(u2_noun who);

      /* u2_unix_ef_ergo(): update filesystem, outbound.
      */
        void
        u2_unix_ef_ergo(u2_noun who,
                        u2_noun syd,
                        u2_noun rel);

      /* u2_unix_io_init(): initialize storage.
      */
        void 
        u2_unix_io_init(void);

      /* u2_unix_io_talk(): start listening for fs events.
      */
        void
        u2_unix_io_talk(void);

      /* u2_unix_io_exit(): terminate storage.
      */
        void 
        u2_unix_io_exit(void);

      /* u2_unix_io_poll(): update storage state.
      */
        void
        u2_unix_io_poll(void);


    /**  Behn, just a timer.
    **/
      /* u2_batz_io_init(): initialize batz timer.
      */
        void 
        u2_batz_io_init(void);

      /* u2_batz_io_exit(): terminate timer.
      */
        void 
        u2_batz_io_exit(void);

      /* u2_batz_io_poll(): update batz IO state.
      */
        void
        u2_batz_io_poll(void);


    /**  HTTP, new style.
    **/
      /* u2_http_ef_thou(): send %thou effect to http. 
      */
        void
        u2_http_ef_thou(c3_l     coq_l,
                        c3_l     seq_l,
                        u2_noun  rep);

      /* u2_http_ef_thus(): send %thus effect to http.
      */
        void
        u2_http_ef_thus(c3_l    num_l,
                        u2_noun req);

      /* u2_http_ef_bake(): create new http server.
      */
        void
        u2_http_ef_bake(void);

      /* u2_http_io_init(): initialize http I/O.
      */
        void 
        u2_http_io_init(void);

      /* u2_http_io_talk(): start http listener.
      */
        void
        u2_http_io_talk(void);

      /* u2_http_io_exit(): terminate http I/O.
      */
        void 
        u2_http_io_exit(void);

      /* u2_http_io_poll(): update http IO state.
      */
        void
        u2_http_io_poll(void);

    /** Raft log syncing.
    **/
      /* u2_raft_readopt(): parse command line options.
      */
        u2_bean
        u2_raft_readopt(u2_ropt* rop_u, const c3_c* arg_c);

      /* u2_raft_io_init(): initialize raft I/O.
      */
        void
        u2_raft_io_init(void);

      /* u2_raft_push(): persist a blob.
      **
      ** Accepts blob pointer and length in 32-bit words.
      ** Returns the sequence number for this blob.
      */
        c3_w
        u2_raft_push(u2_raft* raf_u, c3_w* bob_w, c3_w len_w);
