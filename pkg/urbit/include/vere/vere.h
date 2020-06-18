/* include/vere/vere.h
*/

#include <uv.h>

  /** Quasi-tunable parameters.
  **/
    /* First kernel this executable can boot.
    */
#     define FirstKernel   164
#     define DefaultKernel 164

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

    /* u3_lane: ames lane (IP address and port)
    */
      typedef struct _u3_lane {
        c3_w             pip_w;             //  target IPv4 address
        c3_s             por_s;             //  target port
      } u3_lane;

    /* u3_moor_poke: poke callback function.
    */
      typedef void (*u3_moor_poke)(void*, u3_atom);

    /* u3_moor_bail: bailout callback function.
    */
      typedef void (*u3_moor_bail)(void*, const c3_c* err_c);

    /* u3_meat: blob message block.
    */
      typedef struct _u3_meat {
        struct _u3_meat* nex_u;
        c3_d             len_d;
        c3_y             hun_y[0];
      } u3_meat;

    /* u3_mess_type: in-process message type.
    */
      typedef enum {
        u3_mess_head = 0,                   //  awaiting header
        u3_mess_tail = 1                    //  awaiting body
      } u3_mess_type;

    /* u3_mess: blob message in process.
    */
      typedef struct _u3_mess {
        u3_mess_type     sat_e;             //  msg type
        union {                             //
          struct {                          //  awaiting header:
            c3_y         len_y[8];          //    header bytes
            c3_y         has_y;             //    length
          } hed_u;                          //
          struct {                          //  awaiting body
            u3_meat*     met_u;             //    partial message
            c3_d         has_d;             //    length
          } tal_u;                          //
        };
      } u3_mess;

    /* u3_moat: inbound message stream.
    */
      typedef struct _u3_moat {
        uv_pipe_t        pyp_u;             //  input stream
        u3_moor_bail     bal_f;             //  error response function
        void*            ptr_v;             //  callback pointer
        u3_moor_poke     pok_f;             //  action function
        u3_mess          mes_u;             //  message in progress
        uv_timer_t       tim_u;             //  queue timer
        u3_meat*         ent_u;             //  entry of message queue
        u3_meat*         ext_u;             //  exit of message queue
      } u3_moat;

    /* u3_mojo: outbound message stream.
    */
      typedef struct _u3_mojo {
        uv_pipe_t        pyp_u;             //  output stream
        u3_moor_bail     bal_f;             //  error response function
        void*            ptr_v;             //  callback pointer
      } u3_mojo;

    /* u3_moor: two-way message stream, linked list */
      typedef struct _u3_moor {
        uv_pipe_t        pyp_u;             //  duplex stream
        u3_moor_bail     bal_f;             //  error response function
        void*            ptr_v;             //  callback pointer
        u3_moor_poke     pok_f;             //  action function
        u3_mess          mes_u;             //  message in progress
        uv_timer_t       tim_u;             //  queue timer
        u3_meat*         ent_u;             //  entry of message queue
        u3_meat*         ext_u;             //  exit of message queue
        struct _u3_moor* nex_u;             //  next in list
      } u3_moor;

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

    /* u3_save: checkpoint control.
    */
      typedef struct _u3_save {
        uv_timer_t  tim_u;                  //  checkpoint timer
        uv_signal_t sil_u;                  //  child signal
        c3_d        req_d;                  //  requested at evt_d
        c3_d        dun_d;                  //  completed at evt_d
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
          uv_timer_t tim_u;                 //  spinner timeout
          c3_o       diz_o;                 //  spinner activated
          c3_d       eve_d;                 //  spin count
          c3_d       end_d;                 //  spinner end tick (ms)
          c3_c       why_c[5];              //  spinner label
        } sun_u;
      } u3_utat;

    /* u3_usig: receive signals.
    */
      typedef struct _u3_usig {
        uv_signal_t      sil_u;
        c3_i             num_i;
        struct _u3_usig* nex_u;
      } u3_usig;

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
        struct _u3_auto* car_u;             //  driver hack
      } u3_utty;

    /* u3_trac: tracing information.
    */
      typedef struct _u3_trac {
        c3_w   nid_w;                       //  nock pid
        FILE*  fil_u;                       //  trace file (json)
        c3_w   con_w;                       //  trace counter
        c3_w   fun_w;                       //  file counter
      } u3_trac;

    /* u3_opts: command line configuration.
    */
      typedef struct _u3_opts {
        c3_c*   arv_c;                      //  -A, initial sync from
        c3_o    abo;                        //  -a, abort aggressively
        c3_c*   pil_c;                      //  -B, bootstrap from
        c3_o    nuu;                        //  -c, new pier
        c3_o    dry;                        //  -D, dry compute, no checkpoint
        c3_o    dem;                        //  -d, daemon
        c3_c*   eth_c;                      //  -e, ethereum node url
        c3_c*   fak_c;                      //  -F, fake ship
        c3_c*   gen_c;                      //  -G, czar generator
        c3_o    gab;                        //  -g, test garbage collection
        c3_c*   dns_c;                      //  -H, ames bootstrap domain
        c3_c*   jin_c;                      //  -I, inject raw event
        c3_c*   lit_c;                      //  -J, ivory (fastboot) kernel
        c3_o    tra;                        //  -j, json trace
        c3_w    kno_w;                      //  -K, kernel version
        c3_c*   key_c;                      //  -k, private key file
        c3_o    net;                        //  -L, local-only networking
        c3_o    lit;                        //  -l, lite mode
        c3_c*   til_c;                      //  -n, play till eve_d
        c3_o    pro;                        //  -P, profile
        c3_s    por_s;                      //  -p, ames port
        c3_o    qui;                        //  -q, quiet
        c3_o    rep;                        //  -R, report build info
        c3_c*   roc_c;                      //  -r, load rock by eve_d
        c3_o    has;                        //  -S, Skip battery hashes
        c3_o    tem;                        //  -t, Disable terminal/tty assumptions
        c3_o    git;                        //  -s, pill url from arvo git hash
        c3_c*   url_c;                      //  -u, pill url
        c3_o    veb;                        //  -v, verbose (inverse of -q)
        c3_c*   who_c;                      //  -w, begin with ticket
        c3_o    tex;                        //  -x, exit after loading
      } u3_opts;

    /* u3_host: entire host.
    */
      typedef struct _u3_host {
        c3_w       kno_w;                   //  current executing stage
        c3_c*      dir_c;                   //  pier path (no trailing /)
        c3_c*      wrk_c;                   //  worker executable path
        c3_d       now_d;                   //  event tick
        uv_loop_t* lup_u;                   //  libuv event loop
        u3_usig*   sig_u;                   //  signal list
        u3_utty*   uty_u;                   //  linked terminal list
        u3_opts    ops_u;                   //  commandline options
        c3_i       xit_i;                   //  exit code for shutdown
        u3_trac    tra_u;                   //  tracing information
        void     (*bot_f)();                //  call when chis is up
      } u3_host;                            //  host == computer == process

    /**  New pier system.
    **/
      /* u3_ovum_news: u3_ovum lifecycle events
      */
        typedef enum {
          u3_ovum_drop = 0,                 //  unplanned
          u3_ovum_work = 1,                 //  begun
          u3_ovum_done = 2                  //  complete
        } u3_ovum_news;

      struct _u3_ovum;

      /* u3_ovum_peer: news callback
      */
        typedef void (*u3_ovum_peer)(struct _u3_ovum*, u3_ovum_news);

      /* u3_ovum_bail: failure callback
      */
        typedef void (*u3_ovum_bail)(struct _u3_ovum*, u3_noun);

      /* u3_ovum: potential event
      */
        typedef struct _u3_ovum {
          void*            vod_p;               //  context
          c3_l             msc_l;               //  ms to timeout
          u3_noun            tar;               //  target (in arvo)
          u3_noun            wir;               //  wire
          u3_noun            cad;               //  card
          struct {                              //  spinner:
            u3_atom          lab;               //    label
            c3_o           del_o;               //    delay (c3y)
          } pin_u;                              //
          struct {                              //  optional callbacks:
            u3_ovum_peer  news_f;               //    progress
            u3_ovum_bail  bail_f;               //    failure
          } cb_u;                               //
          struct _u3_ovum* pre_u;               //  previous ovum
          struct _u3_ovum* nex_u;               //  next ovum
          struct _u3_auto* car_u;               //  backpointer to i/o driver
        } u3_ovum;

      /* u3_fact: completed event
      */
        typedef struct _u3_fact {
          c3_d             eve_d;               //  event number
          c3_l             bug_l;               //  kernel mug before XX remove
          c3_l             mug_l;               //  kernel mug after
          u3_noun            job;               //  (pair date ovum)
          struct _u3_fact* nex_u;               //  next in queue
        } u3_fact;

      /* u3_gift: effects
      */
        typedef struct _u3_gift {
          c3_d             eve_d;               //  causal event number
          u3_noun            act;               //  (list ovum)
          struct _u3_gift* nex_u;               //  next in queue
        } u3_gift;

      /* u3_info: ordered, contiguous slice of facts
      */
        typedef struct _u3_info {
          u3_fact*         ent_u;               //  queue entry (highest)
          u3_fact*         ext_u;               //  queue exit (lowest)
        } u3_info;

      /* u3_peek_cb: namespace read response callback.
      */
        typedef void (*u3_peek_cb)(void*, u3_noun);

      /* u3_peek: namespace read request
      */
        typedef struct _u3_peek {
          void*            vod_p;               //  context
          u3_peek_cb       fun_f;               //  callback
          u3_noun            now;               //  XX
          u3_noun            gan;               //  leakset
          u3_noun            ful;               //  /care/beam
        } u3_peek;

      /* u3_writ_type: king->serf ipc message types
      */
        typedef enum {
          u3_writ_work = 0,
          u3_writ_peek = 1,
          u3_writ_play = 2,
          u3_writ_save = 3,
          u3_writ_pack = 4,
          u3_writ_exit = 5
        } u3_writ_type;

      /* u3_writ: ipc message from king to serf
      */
        typedef struct _u3_writ {
          struct timeval   tim_u;               //  time enqueued
          u3_atom            mat;               //  serialized
          struct _u3_writ* nex_u;               //  next in queue
          u3_writ_type     typ_e;               //  type-tagged
          union {                               //
            struct {                            //  work:
              u3_ovum*     egg_u;               //    origin
              u3_noun        job;               //    (pair date ovum)
            } wok_u;                            //
            u3_peek*       pek_u;               //  peek
            u3_info        fon_u;               //  recompute
            c3_d           eve_d;               //  save/pack at
            c3_w           xit_w;               //  exit code
          };
        } u3_writ;

      /* u3_lord_cb: u3_lord callbacks
      */
        typedef struct _u3_lord_cb {
          void* vod_p;
          void (*live_f)(void*);
          void (*slog_f)(void*, c3_w, u3_noun);
          void (*spin_f)(void*, u3_atom, c3_o);
          void (*spun_f)(void*);
          void (*play_done_f)(void*, u3_info, c3_l mug_l);
          void (*play_bail_f)(void*, u3_info, c3_l mug_l, c3_d eve_d, u3_noun dud);
          void (*work_done_f)(void*, u3_ovum*, u3_fact*, u3_gift*);
          void (*work_bail_f)(void*, u3_ovum*, u3_noun lud);
          void (*save_f)(void*);
          void (*pack_f)(void*);
          void (*exit_f)(void*, c3_o);
        } u3_lord_cb;

      /* u3_lord: serf controller.
      */
        typedef struct _u3_lord {
          uv_process_t         cub_u;           //  process handle
          uv_process_options_t ops_u;           //  process configuration
          uv_stdio_container_t cod_u[3];        //  process options
          time_t               wen_t;           //  process creation time
          u3_mojo              inn_u;           //  client's stdin
          u3_moat              out_u;           //  client's stdout
          c3_w                 wag_w;           //  config flags
          c3_c*                bin_c;           //  binary path
          c3_c*                pax_c;           //  directory
          c3_d                 key_d[4];        //  image key
          c3_o                 liv_o;           //  live
          c3_y                 hon_y;           //  hoon kelvin
          c3_y                 noc_y;           //  nock kelvin
          c3_d                 eve_d;           //  last event completed
          c3_l                 mug_l;           //  mug at eve_d
          u3_lord_cb            cb_u;           //  callbacks
          c3_o                 pin_o;           //  spinning
          c3_w                 dep_w;           //  queue depth
          struct _u3_writ*     ent_u;           //  queue entry
          struct _u3_writ*     ext_u;           //  queue exit
        } u3_lord;

      /* u3_read: event log read request
      */
        typedef struct _u3_read {
          union {                               //  read timer/handle
            uv_timer_t  tim_u;                  //
            uv_handle_t had_u;                  //
          };                                    //
          c3_d             eve_d;               //  first event
          c3_d             len_d;               //  read stride
          struct _u3_fact* ent_u;               //  response entry
          struct _u3_fact* ext_u;               //  response exit
          struct _u3_read* nex_u;               //  next read
          struct _u3_read* pre_u;               //  previous read
          struct _u3_disk* log_u;               //  disk backpointer
        } u3_read;

      /* u3_disk_cb: u3_disk callbacks
      */
        typedef struct _u3_disk_cb {
          void* vod_p;
          void (*read_done_f)(void*, u3_info);
          void (*read_bail_f)(void*, c3_d eve_d);
          void (*write_done_f)(void*, c3_d eve_d);
          void (*write_bail_f)(void*, c3_d eve_d);
        } u3_disk_cb;

      /* u3_disk: manage event persistence.
      */
        typedef struct _u3_disk {
          u3_dire*         dir_u;               //  main pier directory
          u3_dire*         urb_u;               //  urbit system data
          u3_dire*         com_u;               //  log directory
          c3_o             liv_o;               //  live
          void*            mdb_u;               //  lmdb environment.
          c3_d             sen_d;               //  commit requested
          c3_d             dun_d;               //  committed
          u3_disk_cb        cb_u;               //  callbacks
          u3_read*         red_u;               //  read requests
          union {                               //  write thread/request
            uv_work_t      ted_u;               //
            uv_req_t       req_u;               //
          };                                    //
          c3_o             ted_o;               //  c3y == active
          u3_info          put_u;               //  write queue
        } u3_disk;

      /* u3_psat: pier state.
      */
        typedef enum {
          u3_psat_init = 0,                   //  initialized
          u3_psat_boot = 1,                   //  bootstrap
          u3_psat_play = 2,                   //  replaying
          u3_psat_work = 3,                   //  working
          u3_psat_done = 4                    //  shutting down
        } u3_psat;

      /* u3_boot: bootstrap event sequence
      */
        typedef struct _u3_boot {
          u3_noun bot;                          //  boot formulas
          u3_noun mod;                          //  module ova
          u3_noun use;                          //  userpace ova
        } u3_boot;

      /* u3_play: replay control.
      */
        typedef struct _u3_play {
          c3_d             eve_d;               //  target
          c3_d             req_d;               //  last read requested
          c3_d             sen_d;               //  last sent
          u3_fact*         ent_u;               //  queue entry
          u3_fact*         ext_u;               //  queue exit
          struct _u3_pier* pir_u;               //  pier backpointer
        } u3_play;

      /* u3_auto_cb: i/o driver callbacks
      */
        typedef struct _u3_auto_cb {
          void (*talk_f)(struct _u3_auto*);
          c3_o (*kick_f)(struct _u3_auto*, u3_noun, u3_noun);
          void (*exit_f)(struct _u3_auto*);  // XX close_cb?
        } u3_auto_cb;

      /* u3_auto: abstract i/o driver
      */
        typedef struct _u3_auto {
          c3_m             nam_m;
          c3_o             liv_o;
          u3_auto_cb          io;  // XX io_u;
          c3_w             dep_w;
          struct _u3_ovum* ent_u;
          struct _u3_ovum* ext_u;
          struct _u3_auto* nex_u;
          struct _u3_pier* pir_u;
        } u3_auto;

      /* u3_wall: pier barrier
      */
        typedef struct _u3_wall {
          void*            vod_p;
          c3_d             eve_d;
          void (*wal_f)(void*, c3_d);
          struct _u3_wall* nex_u;
        } u3_wall;

      /* u3_work: normal operation.
      */
        typedef struct _u3_work {
          u3_auto*         car_u;               //  i/o drivers
          u3_wall*         wal_u;               //  barriers
          struct {                              //  finished event queue:
            c3_d           rel_d;               //    last released
            u3_gift*       ent_u;               //    entry
            u3_gift*       ext_u;               //    exit
          } fec_u;                              //
          uv_prepare_t     pep_u;               //  pre-loop
          uv_check_t       cek_u;               //  post-loop
          uv_idle_t        idl_u;               //  catchall XX uv_async_t?
          struct _u3_pier* pir_u;               //  pier backpointer
        } u3_work;

      /* u3_pier: ship controller.
      */
        typedef struct _u3_pier {
          c3_c*            pax_c;               //  pier directory
          c3_w             lif_w;               //  lifecycle barrier
          c3_d             who_d[2];            //  identity
          c3_c*            who_c;               //  identity as C string
          c3_o             fak_o;               //  yes iff fake security
          c3_o             liv_o;               //  fully live
          u3_disk*         log_u;               //  event log
          u3_lord*         god_u;               //  computer
          u3_psat          sat_e;               //  type-tagged
          union {                               //
            u3_boot*       bot_u;               //    bootstrap
            u3_play*       pay_u;               //    recompute
            u3_work*       wok_u;               //    work
          };
          // XX remove
          c3_s             por_s;               //  UDP port
          u3_save*         sav_u;               //  autosave
        } u3_pier;

      /* u3_king: all executing piers.
      */
        typedef struct _u3_daemon {
          c3_c*     soc_c;                      //  socket name
          c3_c*     certs_c;                    //  ssl certificate dump
          c3_w      len_w;                      //  number used
          c3_w      all_w;                      //  number allocated
          u3_pier** tab_u;                      //  pier table
          uv_pipe_t cmd_u;                      //  command socket
          u3_moor*  cli_u;                      //  connected clients
          uv_timer_t tim_u;                     //  gc timer
        } u3_daemon;

      /* u3_pier_spin(): (re-)activate idle handler
      */
        void
        u3_pier_spin(u3_pier* pir_u);

#     define u3L  u3_Host.lup_u             //  global event loop
#     define u3Z  (&(u3_Raft))
#     define u3K  u3_Daemon

  /** Global variables.
  **/
    c3_global  u3_host   u3_Host;
    c3_global  c3_c*     u3_Local;
    c3_global  u3_daemon u3_Daemon;

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

    /**  ward: common structure lifecycle
    **/
      /* u3_dent_init(): initialize file record.
      */
        u3_dent*
        u3_dent_init(const c3_c* nam_c);

      /* u3_dent_free(): dispose file record.
      */
        void
        u3_dent_free(u3_dent *det_u);

      /* u3_dire_init(): initialize directory record.
      */
        u3_dire*
        u3_dire_init(const c3_c* pax_c);

      /* u3_dire_free(): dispose directory record.
      */
        void
        u3_dire_free(u3_dire *dir_u);

      /* u3_fact_init(): initialize completed event.
      */
        u3_fact*
        u3_fact_init(c3_d eve_d, c3_l mug_l, u3_noun job);

      /* u3_fact_free(): dispose completed event.
      */
        void
        u3_fact_free(u3_fact *tac_u);

      /* u3_gift_init(): initialize effect list.
      */
        u3_gift*
        u3_gift_init(c3_d eve_d, u3_noun act);

      /* u3_gift_free(): dispose effect list.
      */
        void
        u3_gift_free(u3_gift* gif_u);

    /**  New vere
    **/
      /* u3_auto_init(): initialize all drivers.
      */
        u3_auto*
        u3_auto_init(u3_pier* pir_u);

      /* u3_auto_exit(): close all drivers.
      */
        void
        u3_auto_exit(u3_auto* car_u);

      /* u3_auto_talk(): start all drivers.
      */
        void
        u3_auto_talk(u3_auto* car_u);

      /* u3_auto_live(): check if all drivers are live.
      */
        c3_o
        u3_auto_live(u3_auto* car_u);

      /* u3_auto_kick(): route effects to a linked driver. RETAIN
      */
        void
        u3_auto_kick(u3_auto* car_u, u3_noun act);

      /* u3_auto_next(): select an ovum, dequeue and construct.
      */
        u3_ovum*
        u3_auto_next(u3_auto* car_u, u3_noun* ovo);

      /* u3_auto_drop(): dequeue and dispose an ovum.
      */
        void
        u3_auto_drop(u3_auto* car_u, u3_ovum* egg_u);

      /* u3_auto_work(): notify driver of [egg_u] commencement.
      */
        void
        u3_auto_work(u3_ovum* egg_u);

      /* u3_auto_done(): notify driver of [egg_u] completion.
      */
        void
        u3_auto_done(u3_ovum* egg_u);

      /* u3_auto_bail(): notify driver that [egg_u] crashed.
      */
        void
        u3_auto_bail(u3_ovum* egg_u, u3_noun lud);

      /* u3_auto_bail_slog(): print a bail notification.
      */
        void
        u3_auto_bail_slog(u3_ovum* egg_u, u3_noun lud);

      /* u3_auto_plan(): create and enqueue an ovum.
      */
        u3_ovum*
        u3_auto_plan(u3_auto* car_u,
                     c3_l     msc_l,
                     u3_noun    tar,
                     u3_noun    wir,
                     u3_noun    cad);

      /* u3_auto_peer(): subscribe to updates.
      */
        void
        u3_auto_peer(u3_ovum*      egg_u,
                     u3_ovum_peer news_f,
                     u3_ovum_bail bail_f);

      /* u3_disk_init(): load or create pier directories and event log.
      */
        u3_disk*
        u3_disk_init(c3_c* pax_c, u3_disk_cb cb_u);

      /* u3_disk_exit(): close [log_u] and dispose.
      */
        void
        u3_disk_exit(u3_disk* log_u);

      /* u3_disk_read_meta(): read metadata.
      */
        c3_o
        u3_disk_read_meta(u3_disk* log_u,
                          c3_d*    who_d,
                          c3_o*    fak_o,
                          c3_w*    lif_w);

      /* u3_disk_save_meta(): save metadata.
      */
        c3_o
        u3_disk_save_meta(u3_disk* log_u,
                          c3_d     who_d[2],
                          c3_o     fak_o,
                          c3_w     lif_w);

      /* u3_disk_read(): read [len_d] events starting at [eve_d].
      */
        void
        u3_disk_read(u3_disk* log_u, c3_d eve_d, c3_d len_d);

      /* u3_disk_boot_plan(): enqueue boot sequence, without autocommit.
      */
        void
        u3_disk_boot_plan(u3_disk* log_u, u3_noun job);

      /* u3_disk_boot_save(): commit boot sequence.
      */
        void
        u3_disk_boot_save(u3_disk* log_u);

      /* u3_disk_plan(): enqueue completed event for persistence.
      */
        void
        u3_disk_plan(u3_disk* log_u, u3_fact* tac_u);

      /* u3_lord_init(): start serf.
      */
        u3_lord*
        u3_lord_init(c3_c*     pax_c,
                     c3_w      wag_w,
                     c3_d      key_d[4],
                     u3_lord_cb cb_u);

      /* u3_lord_exit(): shutdown gracefully.
      */
        void
        u3_lord_exit(u3_lord* god_u, c3_w cod_w);

      /* u3_lord_save(): save a snapshot.
      */
        c3_o
        u3_lord_save(u3_lord* god_u);

      /* u3_lord_pack(): save portable state.
      */
        c3_o
        u3_lord_pack(u3_lord* god_u);

      /* u3_lord_work(): attempt work.
      */
        void
        u3_lord_work(u3_lord* god_u, u3_ovum* egg_u, u3_noun ovo);

      /* u3_lord_play(): recompute batch.
      */
        void
        u3_lord_play(u3_lord* god_u, u3_info fon_u);

      /* u3_lord_peek(): read namespace.
      */
        void
        u3_lord_peek(u3_lord*   god_u,
                     u3_noun      gan,
                     u3_noun      ful,
                     void*      vod_p,
                     u3_peek_cb fun_f);

      /* u3_lord_peek_mine(): read namespace, injecting ship.
      */
        void
        u3_lord_peek_mine(u3_lord*   god_u,
                          u3_noun      gan,
                          c3_m       car_m,
                          u3_noun      pax,
                          void*      vod_p,
                          u3_peek_cb fun_f);

      /* u3_lord_peek_last(): read namespace, injecting ship and case.
      */
        void
        u3_lord_peek_last(u3_lord*   god_u,
                          u3_noun      gan,
                          c3_m       car_m,
                          u3_atom      des,
                          u3_noun      pax,
                          void*      vod_p,
                          u3_peek_cb fun_f);

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

    /**  Terminal, new style.
    **/
      /* u3_term_start_spinner(): prepare spinner state. RETAIN.
      */
        void
        u3_term_start_spinner(u3_noun say, c3_o del_o);

      /* u3_term_stop_spinner(): reset spinner state and restore input line.
      */
        void
        u3_term_stop_spinner(void);

      /* u3_term_get_blew(): return window size [columns rows].
      */
        u3_noun
        u3_term_get_blew(c3_l tid_l);

      /* u3_term_ef_winc(): window change.
      */
        void
        u3_term_ef_winc(void);

      /* u3_term_ef_ctlc(): send ^C.
      */
        void
        u3_term_ef_ctlc(void);

      /* u3_term_io_init(): initialize terminal I/O.
      */
        u3_auto*
        u3_term_io_init(u3_pier* pir_u);

      /* u3_term_io_hija(): hijack console for cooked print.
      */
        FILE*
        u3_term_io_hija(void);

      /* u3_term_it_log(): writes a log message
      */
        void
        u3_term_io_log(c3_c* line);

      /* u3_term_io_loja(): release console from cooked print.
      */
        void
        u3_term_io_loja(int x);

      /* u3_term_log_init(): initialize terminal for logging
      */
        void
        u3_term_log_init(void);

      /* u3_term_log_exit(): clean up terminal.
      */
        void
        u3_term_log_exit(void);


    /**  Ames, packet networking.
    **/
      /* u3_ames_io_init(): initialize ames I/O.
      */
        u3_auto*
        u3_ames_io_init(u3_pier* pir_u);

      /* u3_ames_decode_lane(): destructure lane from noun
      */
        u3_lane
        u3_ames_decode_lane(u3_noun);

      /* u3_ames_encode_lane(): encode lane as noun
      */
        u3_noun
        u3_ames_encode_lane(u3_lane);

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
      /* u3_unix_initial_into_card(): create initial filesystem sync card.
      */
        u3_noun
        u3_unix_initial_into_card(c3_c* arv_c);

      /* u3_unix_io_init(): initialize storage.
      */
        u3_auto*
        u3_unix_io_init(u3_pier* pir_u);

    /**  behn, just a timer.
    **/
      /* u3_behn_io_init(): initialize behn timer.
      */
        u3_auto*
        u3_behn_io_init(u3_pier* pir_u);

    /**  HTTP server.
    **/
      /* u3_http_io_init(): initialize http I/O.
      */
        u3_auto*
        u3_http_io_init(u3_pier* pir_u);

    /**  HTTP client.
    **/
      /* u3_cttp_io_init(): initialize cttp I/O.
      */
        u3_auto*
        u3_cttp_io_init(u3_pier* pir_u);

    /**  fore, first events
    **/
      /* u3_hind_io_init(): initialize fore
      */
        u3_auto*
        u3_fore_io_init(u3_pier* pir_u);

    /**  hind, defaults
    **/
      /* u3_hind_io_init(): initialize hint
      */
        u3_auto*
        u3_hind_io_init(u3_pier* pir_u);

    /**  Stream messages.
    **/
      /* u3_newt_decode(): decode a (partial) length-prefixed byte buffer
      */
        void
        u3_newt_decode(u3_moat* mot_u, c3_y* buf_y, c3_d len_d);

      /* u3_newt_write(): write atom to stream; free atom.
      */
        void
        u3_newt_write(u3_mojo* moj_u, u3_atom mat);

      /* u3_newt_read_sync(): start reading; multiple msgs synchronous.
      */
        void
        u3_newt_read_sync(u3_moat* mot_u);

      /* u3_newt_read(): start reading; each msg asynchronous.
      */
        void
        u3_newt_read(u3_moat* mot_u);

    /** Pier control.
    **/
      /* u3_pier_db_shutdown(): close the log.
      */
        void
        u3_pier_db_shutdown(u3_pier* pir_u);

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
        u3_pier_exit(u3_pier* pir_u);

      /* u3_pier_bail(): immediately shutdown..
      */
        void
        u3_pier_bail(void);

      /* u3_pier_halt(): emergency release.
      */
        void
        u3_pier_halt(void);

      /* u3_pier_save(): request checkpoint.
      */
        c3_o
        u3_pier_save(u3_pier* pir_u);

      /* u3_pier_pack(): save a portable snapshot.
      */
        c3_o
        u3_pier_pack(u3_pier* pir_u);

      /* u3_pier_stub(): get the One Pier for unreconstructed code.
      */
        u3_pier*
        u3_pier_stub(void);

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
        u3_pier_tank(c3_l tab_l, c3_w pri_w, u3_noun tac);

      /* u3_pier_punt(): dump tank list.
      */
        void
        u3_pier_punt(c3_l tab_l, u3_noun tac);

      /* u3_pier_punt_goof(): dump a [mote tang] crash report.
      */
        void
        u3_pier_punt_goof(const c3_c* cap_c, u3_noun dud);

      /* u3_pier_punt_ovum(): print ovum details.
      */
        void
        u3_pier_punt_ovum(const c3_c* cap_c, u3_noun wir, u3_noun tag);

      /* u3_pier_sway(): print trace.
      */
        void
        u3_pier_sway(c3_l tab_l, u3_noun tax);

      /* u3_pier_mark(): mark all Loom allocations in all u3_pier structs.
      */
        c3_w
        u3_pier_mark(FILE* fil_u);

      /* u3_dawn_come(): mine a comet
      */
        u3_noun
        u3_dawn_come(void);

      /* u3_dawn_vent(): validated boot event
      */
        u3_noun
        u3_dawn_vent(u3_noun seed);

      /* u3_daemon_commence(): start the daemon
      */
        void
        u3_daemon_commence();

      /* u3_daemon_bail(): immediately shutdown.
      */
        void
        u3_daemon_bail(void);

      /* u3_daemon_grab(): gc the daemon
      */
        void
        u3_daemon_grab(void* vod_p);


        c3_w
        u3_readdir_r(DIR *dirp, struct dirent *entry, struct dirent **result);
