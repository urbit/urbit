#ifndef U3_VERE_H
#define U3_VERE_H

#include <uv.h>

#include "vere/disk.h"
#include "vere/newt.h"
#include "vere/ovum.h"
#include "vere/peek.h"

  /** Quasi-tunable parameters.
  **/
    /* First kernel this executable can boot.
    */
#     define FirstKernel   164
#     define DefaultKernel 164

  /** Data types.
  **/

    /* u3_lane: ames lane (IP address and port)
    */
      typedef struct _u3_lane {
        c3_w             pip_w;             //  target IPv4 address
        c3_s             por_s;             //  target port
      } u3_lane;

    /* u3_utat: unix terminal state.
    */
      typedef struct {
        struct {
          c3_l  col_l;                      //  columns
          c3_l  row_l;                      //  rows
        } siz;

        struct {
          c3_y* lin_y;                      //  current line (utf8)
          c3_w  byt_w;                      //  utf8 line-length
          c3_w  wor_w;                      //  utf32 line-length
          c3_w  sap_w;                      //  escape chars in line
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
        //    disabled, currently unused
        //
        // struct {
        //   uv_buf_t kcuu1_u;              //  key_up
        //   uv_buf_t kcud1_u;              //  key_down
        //   uv_buf_t kcub1_u;              //  key_back
        //   uv_buf_t kcuf1_u;              //  key_forward
        // } inn;
        struct {
          uv_buf_t clear_u;              //  clear_screen
          uv_buf_t el_u;                 //  clr_bol clear to beginning
          // uv_buf_t el1_u;             //  clr_eol clear to end
          uv_buf_t ed_u;                 //  clear to end of screen
          uv_buf_t bel_u;                //  bel sound bell
          uv_buf_t cub1_u;               //  parm_left
          uv_buf_t cuf1_u;               //  parm_right
          uv_buf_t cuu1_u;               //  parm_up
          uv_buf_t cud1_u;               //  parm_down
          // uv_buf_t cub_u;             //  parm_left_cursor #num
          // uv_buf_t cuf_u;             //  parm_right_cursor #num
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
      struct _u3_utty;

    /* u3_ustm: uv stream.
    */
      typedef union _u3_ustm {
        uv_pipe_t pip_u;
        uv_tty_t  tty_u;
      } u3_ustm;

    /* u3_ttyf: simple unix tty function.
    */
      typedef c3_o (*u3_ttyf)(struct _u3_utty* uty_u);

    /* u3_utty: unix tty.
    */
      typedef struct _u3_utty {
        u3_ustm          pin_u;             //  input stream
        u3_ustm          pop_u;             //  output stream
        struct _u3_utty* nex_u;             //  next in host list
        u3_ttyf          sta_f;             //  start tty
        u3_ttyf          sto_f;             //  clean up tty
        u3_ttyf          hij_f;             //  hijack tty for cooked print
        u3_ttyf          loj_f;             //  release tty from cooked print
        c3_o           (*wsz_f)
                       (struct _u3_utty* uty_u,
                        c3_l* col_l,
                        c3_l* row_l);       //  return tty window size
        c3_i             fid_i;             //  file descriptor
        c3_w             tid_l;             //  terminal identity number
        u3_utfo          ufo_u;             //  terminfo strings
        u3_utat          tat_u;             //  control state
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
        c3_c*   bin_c;                      //  -b, http server bind ip
        c3_o    nuu;                        //  -c, new pier
        c3_o    dry;                        //  -D, dry compute, no checkpoint
        c3_o    dem;                        //  -d, daemon
        c3_c*   eth_c;                      //  -e, ethereum node url
        c3_c*   fak_c;                      //  -F, fake ship
        c3_c*   gen_c;                      //  -G, czar generator
        c3_o    gab;                        //  -g, test garbage collection
        c3_c*   dns_c;                      //  -H, ames bootstrap domain
        c3_c*   jin_c;                      //  -I, inject raw event
        c3_c*   imp_c;                      //  -i, import pier state
        c3_w    hap_w;                      //  -C, cap memo cache
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
        c3_c*   pek_c;                      //  -X, scry path (/vc/desk/path)
        c3_c*   puk_c;                      //  -Y, scry result filename
        c3_c*   puf_c;                      //  -Z, scry result format
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
        #if defined(U3_OS_mingw)
        HANDLE     cev_u;                   //  Ctrl-C event handle
        #endif
        u3_utty*   uty_u;                   //  linked terminal list
        u3_opts    ops_u;                   //  commandline options
        c3_i       xit_i;                   //  exit code for shutdown
        u3_trac    tra_u;                   //  tracing information
        void     (*bot_f)();                //  call when chis is up
      } u3_host;                            //  host == computer == process

    /**  New pier system.
    **/
      /* u3_writ_type: king->serf ipc message types
      */
        typedef enum {
          u3_writ_poke = 0,
          u3_writ_peek = 1,
          u3_writ_live = 2,
          u3_writ_exit = 3
        } u3_writ_type;

      /* u3_writ: ipc message from king to serf
      */
        typedef struct _u3_writ {
          struct _u3_writ* nex_u;               //  next in queue
          u3_writ_type     typ_e;               //  type-tagged
          union {                               //
            struct {                            //  poke:
              u3_ovum*     egg_u;               //    origin
              u3_noun        job;               //    (pair date ovum)
            } wok_u;                            //
            u3_peek*       pek_u;               //  peek
          };
        } u3_writ;

      /* u3_lord_cb: u3_lord callbacks
      */
        typedef struct _u3_lord_cb {
          void* ptr_v;
          void (*live_f)(void*);
          void (*slog_f)(void*, c3_w, u3_noun);
          void (*spin_f)(void*, u3_atom, c3_o);
          void (*spun_f)(void*);
          void (*work_done_f)(void*, u3_ovum*, u3_noun act);
          void (*work_bail_f)(void*, u3_ovum*, u3_noun lud);
          void (*save_f)(void*);
          void (*cram_f)(void*);
          void (*bail_f)(void*);
          void (*exit_f)(void*);
        } u3_lord_cb;

      /* u3_lord: serf controller.
      */
        typedef struct _u3_lord {
          uv_process_t         cub_u;           //  process handle
          uv_process_options_t ops_u;           //  process configuration
          uv_stdio_container_t cod_u[3];        //  process options
          u3_cue_xeno*         sil_u;           //  cue handle
          time_t               wen_t;           //  process creation time
          u3_mojo              inn_u;           //  client's stdin
          u3_moat              out_u;           //  client's stdout
          uv_pipe_t            err_u;           //  client's stderr
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

      /* u3_psat: pier state.
      */
        typedef enum {
          u3_psat_init = 0,                   //  initialized
          u3_psat_wyrd = 1,                   //  versioning
          u3_psat_work = 2,                   //  working
          u3_psat_done = 3                    //  shutting down
        } u3_psat;

      /* u3_boot_opts: bootstrap parameters.
      */
        typedef struct _u3_boot_opts {
          c3_w           eny_w[16];         //  entropy
          c3_o           veb_o;             //  verbose
          c3_o           lit_o;             //  lite
          c3_o           sev_l;             //  instance number
          struct timeval tim_u;             //  time
          struct {                          //  kelvin
            c3_m         nam_m;             //    label
            c3_w         ver_w;             //    version
          } ver_u;
        } u3_boot_opts;

      /* u3_auto_cb: i/o driver callbacks
      */
        typedef struct _u3_auto_cb {
          void (*talk_f)(struct _u3_auto*);
          void (*info_f)(struct _u3_auto*);
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

      /* u3_work: normal operation.
      */
        typedef struct _u3_work {
          u3_auto*         car_u;               //  i/o drivers
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
          u3_lord*         god_u;               //  computer
          u3_psat          sat_e;               //  type-tagged
          u3_work*         wok_u;               //    work
          struct {
            u3_pico*       ent_u;
            u3_pico*       ext_u;
          } pec_u;
          void*            sop_p;               //  slog stream data
          void           (*sog_f)               //  slog stream callback
                         (void*, c3_w, u3_noun);//
          // XX remove
          c3_s             por_s;               //  UDP port
          struct _u3_pier* nex_u;               //  next in list
        } u3_pier;

      /* u3_king: all executing piers.
      */
        typedef struct _u3_king {
          void           (*ssl_curl_f)(void*);  //  setup ssl CAs in CURL*
          void           (*ssl_x509_f)(void*);  //  setup ssl CAs in X509_STORE*
          u3_pier*         pir_u;               //  pier list
          uv_timer_t       tim_u;               //  gc timer
        } u3_king;

#     define u3L  u3_Host.lup_u             //  global event loop
#     define u3Z  (&(u3_Raft))
#     define u3K  u3_King

  /** Global variables.
  **/
    c3_global  u3_host   u3_Host;
    c3_global  c3_c*     u3_Local;
    c3_global  u3_king   u3_King;

  /** Functions.
  **/
    /**  ward: common structure lifecycle
    **/
      /* u3_pico_init(): initialize a scry request struct
      */
        u3_pico*
        u3_pico_init();

      /* u3_pico_free(): dispose a scry request struct
      */
        void
        u3_pico_free(u3_pico* pic_u);

      /* u3_mcut_char(): measure/cut character.
      */
        c3_w
        u3_mcut_char(c3_c* buf_c, c3_w len_w, c3_c chr_c);

      /* u3_mcut_cord(): measure/cut cord.
      */
        c3_w
        u3_mcut_cord(c3_c* buf_c, c3_w len_w, u3_noun san);

      /* u3_mcut_path(): measure/cut cord list.
      */
        c3_w
        u3_mcut_path(c3_c* buf_c, c3_w len_w, c3_c sep_c, u3_noun pax);

      /* u3_mcut_host(): measure/cut host.
      */
        c3_w
        u3_mcut_host(c3_c* buf_c, c3_w len_w, u3_noun hot);

    /**  New vere
    **/
      /* u3_auto_init(): initialize all drivers.
      */
        u3_auto*
        u3_auto_init(u3_pier* pir_u);

      /* u3_auto_info(): print status info.
      */
        void
        u3_auto_info(u3_auto* car_u);

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

      /* u3_auto_plan(): enqueue an ovum.
      */
        u3_ovum*
        u3_auto_plan(u3_auto* car_u, u3_ovum* egg_u);

      /* u3_auto_redo(): retry an ovum.
      */
        u3_ovum*
        u3_auto_redo(u3_auto* car_u, u3_ovum* egg_u);

      /* u3_auto_peer(): subscribe to updates.
      */
        void
        u3_auto_peer(u3_ovum*      egg_u,
                     void*         ptr_v,
                     u3_ovum_peer news_f,
                     u3_ovum_bail bail_f);

      /* u3_lord_boot(): instantiate child process.
      */
        void
        u3_lord_boot(c3_c* pax_c,
                     c3_w  wag_w,
                     c3_d  key_d[4],
                     u3_noun msg,
                     void* ptr_v,
                     void (*done_f)(void*, c3_o));

      /* u3_lord_init(): start serf.
      */
        u3_lord*
        u3_lord_init(c3_c*     pax_c,
                     c3_w      wag_w,
                     c3_d      key_d[4],
                     u3_lord_cb cb_u);

      /* u3_lord_info(): print status info.
      */
        void
        u3_lord_info(u3_lord* god_u);

      /* u3_lord_exit(): shutdown gracefully.
      */
        void
        u3_lord_exit(u3_lord* god_u);

      /* u3_lord_stall(): send SIGINT
      */
        void
        u3_lord_stall(u3_lord* god_u);

      /* u3_lord_halt(): shutdown immediately
      */
        void
        u3_lord_halt(u3_lord* god_u);

      /* u3_lord_save(): save a snapshot.
      */
        c3_o
        u3_lord_save(u3_lord* god_u);

      /* u3_lord_cram(): save portable state.
      */
        c3_o
        u3_lord_cram(u3_lord* god_u);

      /* u3_lord_meld(): globally deduplicate persistent state.
      */
        void
        u3_lord_meld(u3_lord* god_u);

      /* u3_lord_pack(): defragment persistent state.
      */
        void
        u3_lord_pack(u3_lord* god_u);

      /* u3_lord_work(): attempt work.
      */
        void
        u3_lord_work(u3_lord* god_u, u3_ovum* egg_u, u3_noun job);

      /* u3_lord_peek(): read namespace, injecting what's missing.
      */
        void
        u3_lord_peek(u3_lord* god_u, u3_pico* pic_u);

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

      /* u3_walk(): traverse `dir_c` to produce an arch, updating `old`.
      */
        u3_noun
        u3_walk(const c3_c* dir_c, u3_noun old);

      /* u3_path(): C unix path in computer for file or directory.
      */
        c3_c*
        u3_path(c3_o    fyl, u3_noun pax);

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

      /* u3_ptty_init(): initialize platform-specific tty.
      */
        u3_utty*
        u3_ptty_init(uv_loop_t* lup_u, const c3_c** err_c);


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

    /** Pier scries.
    **/
      /* u3_pier_peek(): read namespace.
      */
        void
        u3_pier_peek(u3_pier*   pir_u,
                     u3_noun      gan,
                     u3_noun      ful,
                     void*      ptr_v,
                     u3_peek_cb fun_f);

      /* u3_pier_peek_last(): read namespace, injecting ship and case.
      */
        void
        u3_pier_peek_last(u3_pier*   pir_u,
                          u3_noun      gan,
                          c3_m       car_m,
                          u3_atom      des,
                          u3_noun      pax,
                          void*      ptr_v,
                          u3_peek_cb fun_f);

    /** Pier control.
    **/
      /* u3_pier_exit(): trigger a gentle shutdown.
      */
        void
        u3_pier_exit(u3_pier* pir_u);

      /* u3_pier_bail(): immediately shutdown..
      */
        void
        u3_pier_bail(u3_pier* pir_u);

      /* u3_pier_save(): request checkpoint.
      */
        c3_o
        u3_pier_save(u3_pier* pir_u);

      /* u3_pier_cram(): save a portable snapshot.
      */
        c3_o
        u3_pier_cram(u3_pier* pir_u);

      /* u3_pier_meld(): globally deduplicate persistent state.
      */
        void
        u3_pier_meld(u3_pier* pir_u);

      /* u3_pier_pack(): defragment persistent state.
      */
        void
        u3_pier_pack(u3_pier* pir_u);

      /* u3_pier_info(): print status info.
      */
        void
        u3_pier_info(u3_pier* pir_u);

      /* u3_pier_boot(): start the new pier system.
      */
        u3_pier*
        u3_pier_boot(c3_w    wag_w,                 //  config flags
                     u3_noun who,                   //  identity
                     u3_noun ven,                   //  boot event
                     u3_noun pil,                   //  type-of/path-to pill
                     u3_noun pax,                   //  path to pier
                     u3_weak fed);                  //  extra private keys

      /* u3_pier_spin(): (re-)activate idle handler
      */
        void
        u3_pier_spin(u3_pier* pir_u);

      /* u3_pier_stay(): restart the new pier system.
      */
        u3_pier*
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
        u3_dawn_vent(u3_noun ship, u3_noun seed);

      /* u3_king_commence(): start the daemon
      */
        void
        u3_king_commence();

      /* u3_king_stub(): get the One Pier for unreconstructed code.
      */
        u3_pier*
        u3_king_stub(void);

      /* u3_king_info(): print status info.
      */
        void
        u3_king_info(void);

      /* u3_king_done(): all piers closed
      */
        void
        u3_king_done(void);

      /* u3_king_exit(): shutdown gracefully
      */
        void
        u3_king_exit(void);

      /* u3_king_bail(): immediately shutdown.
      */
        void
        u3_king_bail(void);

      /* u3_king_grab(): gc the daemon
      */
        void
        u3_king_grab(void* ptr_v);

      /* u3_daemon_init(): platform-specific daemon mode initialization.
      */
        void
        u3_daemon_init();

      /* u3_write_fd(): retry interrupts, continue partial writes, assert errors.
      */
        void
        u3_write_fd(c3_i fid_i, const void* buf_v, size_t len_i);

        c3_w
        u3_readdir_r(DIR *dirp, struct dirent *entry, struct dirent **result);

#endif /* ifndef U3_VERE_H */
