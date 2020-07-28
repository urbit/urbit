/* worker/main.c
**
**  the main loop of a serf process.
*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <sigsegv.h>
#include <stdint.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <uv.h>
#include <errno.h>

#include "all.h"
#include <vere/vere.h>
#include <vere/serf.h>

#include "ur/hashcons.h"

static u3_serf   u3V;             //  one serf per process
static u3_moat inn_u;             //  input stream
static u3_mojo out_u;             //  output stream

/* _cw_serf_fail(): failure stub.
*/
static void
_cw_serf_fail(void* vod_p, const c3_c* wut_c)
{
  fprintf(stderr, "serf: fail: %s\r\n", wut_c);
  exit(1);
}

/* _cw_serf_send(): send plea back to daemon.
*/
static void
_cw_serf_send(u3_noun pel)
{
  u3_newt_write(&out_u, u3ke_jam(pel));
}

/* _cw_serf_send_slog(): send hint output (hod is [priority tank]).
*/
static void
_cw_serf_send_slog(u3_noun hod)
{
  _cw_serf_send(u3nc(c3__slog, hod));
}

/* _cw_serf_send_stdr(): send stderr output
*/
static void
_cw_serf_send_stdr(c3_c* str_c)
{
  _cw_serf_send_slog(u3nc(0, u3i_string(str_c)));
}

/* _cw_serf_writ():
*/
static void
_cw_serf_writ(void* vod_p, u3_noun mat)
{
  u3_noun ret;

  if ( c3n == u3_serf_writ(&u3V, u3ke_cue(mat), &ret) ) {
    _cw_serf_fail(0, "bad jar");
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
  c3_i nul_i = open("/dev/null", O_RDWR, 0);

  *inn_i = dup(0);
  *out_i = dup(1);

  dup2(nul_i, 0);
  dup2(2, 1);

  close(nul_i);
}

/* _cw_serf_commence(); initialize and run serf
*/
static void
_cw_serf_commence(c3_i argc, c3_c* argv[])
{
  c3_i inn_i, out_i;
  _cw_serf_stdio(&inn_i, &out_i);

  c3_assert( 7 == argc );

  uv_loop_t* lup_u = uv_default_loop();
  c3_c*      dir_c = argv[2];
  c3_c*      key_c = argv[3];
  c3_c*      wag_c = argv[4];
  c3_c*      hap_c = argv[5];
  c3_d       eve_d = 0;

  if ( 1 != sscanf(argv[6], "%" PRIu64 "", &eve_d) ) {
    fprintf(stderr, "serf: rock: invalid number '%s'\r\n", argv[4]);
  }

  memset(&u3V, 0, sizeof(u3V));
  memset(&u3_Host.tra_u, 0, sizeof(u3_Host.tra_u));

  //  load passkey
  //
  //    XX and then ... use passkey
  //
  {
    sscanf(key_c, "%" PRIx64 ":%" PRIx64 ":%" PRIx64 ":%" PRIx64 "",
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
  }

  //  Ignore SIGPIPE signals.
  //
  {
    struct sigaction sig_s = {{0}};
    sigemptyset(&(sig_s.sa_mask));
    sig_s.sa_handler = SIG_IGN;
    sigaction(SIGPIPE, &sig_s, 0);
  }

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
      u3_serf_uncram(&u3V, eve_d);
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
}

/* _cw_info(); print pier info
*/
static void
_cw_info(c3_i argc, c3_c* argv[])
{
  c3_assert( 3 <= argc );

  c3_c* dir_c = argv[2];
  c3_d  eve_d = u3m_boot(dir_c);

  fprintf(stderr, "urbit-worker: %s at event %" PRIu64 "\r\n", dir_c, eve_d);
}

/* _cw_grab(); gc pier.
*/
static void
_cw_grab(c3_i argc, c3_c* argv[])
{
  c3_assert( 3 <= argc );

  c3_c* dir_c = argv[2];
  u3m_boot(dir_c);
  u3_serf_grab();
}

/* _cw_cram(); jam persistent state (rock), and exit.
*/
static void
_cw_cram(c3_i argc, c3_c* argv[])
{
  c3_assert( 3 <= argc );

  c3_c* dir_c = argv[2];
  c3_d  eve_d = u3m_boot(dir_c);

  fprintf(stderr, "urbit-worker: cram: preparing\r\n");

  if ( c3n == u3m_rock_stay(dir_c, eve_d) ) {
    fprintf(stderr, "urbit-worker: cram: unable to jam state\r\n");
    exit(1);
  }

  fprintf(stderr, "urbit-worker: cram: rock saved at event %" PRIu64 "\r\n", eve_d);
}

/* _cw_queu(); cue rock, save, and exit.
*/
static void
_cw_queu(c3_i argc, c3_c* argv[])
{
  c3_assert( 4 <= argc );

  c3_c* dir_c = argv[2];
  c3_c* eve_c = argv[3];
  c3_d  eve_d;

  if ( 1 != sscanf(eve_c, "%" PRIu64 "", &eve_d) ) {
    fprintf(stderr, "urbit-worker: queu: invalid number '%s'\r\n", eve_c);
    exit(1);
  }
  else {
    fprintf(stderr, "urbit-worker: queu: preparing\r\n");

    memset(&u3V, 0, sizeof(u3V));
    u3V.dir_c = strdup(dir_c);
    u3V.sen_d = u3V.dun_d = u3m_boot(dir_c);
    u3_serf_uncram(&u3V, eve_d);
    u3e_save();

    fprintf(stderr, "urbit-worker: queu: rock loaded at event %" PRIu64 "\r\n", eve_d);
  }
}


//  stack frame for recording head vs tail iteration
//
//    In Hoon, this structure would be as follows:
//
//    $%  [%root ~]
//        [%head cell=^]
//        [%tail cell=^ hed-mug=@]
//    ==
//

#define STACK_ROOT 0
#define STACK_HEAD 1
#define STACK_TAIL 2

typedef struct ur_temp_s
{
  uint64_t prev;
  uint64_t size;
  void    *base;
  void     *top;
} ur_temp_t;

void
ur_temp_init(ur_temp_t  *t)
{
  uint64_t fib19 = 4181, fib20 = 6765;

  t->prev = fib19;
  t->size = fib20;
  t->base = malloc(fib20);
  t->top  = t->base;
}

void*
ur_temp_push(ur_temp_t *t, size_t wide)
{
  uint64_t fill = t->top - t->base;
  uint64_t grow = t->size + wide;

  if ( fill > (t->size + wide) ) {
    uint64_t next = t->prev + t->size;
    t->base = realloc(t->base, next);
    t->top  = t->base + fill;
  }

  {
    void* ptr = t->top;
    t->top += wide;
    return ptr;
  }
}

void*
ur_temp_peek(ur_temp_t *t, size_t wide)
{
  return t->top - wide;
}

void
ur_temp_pop(ur_temp_t *t, size_t wide)
{
  t->top -= wide;
  assert( t->top >= t->base );
}

typedef struct _ur_frame_s
{
  c3_y      tag_y;
  u3a_cell* cel_u;
  ur_nref     ref;
} _ur_frame_t;

typedef struct _ur_stack_s
{
  uint32_t prev;
  uint32_t size;
  uint32_t fill;
  _ur_frame_t *entries;
} _ur_stack_t;

static inline void
_stack_push(_ur_stack_t *s, c3_y tag_y, u3a_cell* cel_u, ur_nref ref)
{
  if ( s->fill == s->size ) {
    uint32_t next = s->prev + s->size;
    s->entries = realloc(s->entries, next * sizeof(_ur_frame_t));
    s->prev = s->size;
    s->size = next;
  }

  _ur_frame_t* fam_u = &(s->entries[s->fill++]);
  fam_u->tag_y = tag_y;
  fam_u->cel_u = cel_u;
  fam_u->ref   = ref;
}

static ur_nref
_from_loom(ur_root_t *r, u3_noun a)
{
  ur_nref   ref;

  _ur_stack_t s;
  s.prev = 89;
  s.size = 144;
  s.fill = 0;
  s.entries = malloc((s.prev + s.size) * sizeof(_ur_frame_t));
  _stack_push(&s, STACK_ROOT, 0, 0);

  // ur_temp_t t;
  // ur_temp_init(&t);

  // {
  //   _ur_frame_t *fam_u = ur_temp_push(&t, sizeof(_ur_frame_t));
  //   fam_u->tag_y = STACK_ROOT;
  // }

  advance: {
    //  u3 direct == ur direct
    //
    if ( c3y == u3a_is_cat(a) ) {
      ref = (ur_nref)a;
      goto retreat;
    }
    else {
      u3a_noun* som_u = u3a_to_ptr(a);
      u3a_box*  box_u = u3a_botox(som_u);
      c3_w*     box_w = (void*)box_u;

      //  all bits set == already reallocated
      //
      if ( 0xffffffff == box_w[0] ) {
        ref = ( ((uint64_t)box_w[2]) << 32
              | ((uint64_t)box_w[1]) );
        goto retreat;
      }
      else if ( c3y == u3a_is_atom(a) ) {
        u3a_atom* vat_u = (u3a_atom*)som_u;

        //  coin an nref
        //
        switch ( vat_u->len_w ) {
          case 2: {
            ref = ur_coin64(r, ( ((uint64_t)vat_u->buf_w[1]) << 32
                               | ((uint64_t)vat_u->buf_w[0]) ));
          } break;

          case 1: {
            ref = ur_coin64(r, (uint64_t)vat_u->buf_w[0]);
          } break;


          default: {
            c3_assert( vat_u->len_w );

            uint8_t *byt = (uint8_t*)vat_u->buf_w;
            uint64_t len = u3r_met(3, a);

            ref = ur_coin_bytes(r, byt, len);
          } break;
        }

        //  overwrite u3a_atom with reallocated reference
        //
        box_w[0] = 0xffffffff;
        box_w[1] = ref & 0xffffffff;
        box_w[2] = ref >> 32;

        goto retreat;
      }
      else {
        u3a_cell* cel_u = (u3a_cell*)som_u;
        _stack_push(&s, STACK_HEAD, cel_u, 0);
        // {
        //   _ur_frame_t *fam_u = ur_temp_push(&t, sizeof(_ur_frame_t));
        //   fam_u->tag_y = STACK_HEAD;
        //   fam_u->cel_u = cel_u;
        // }
        a = cel_u->hed;
        goto advance;
      }
    }
  }

  retreat: {
    _ur_frame_t fam_u = s.entries[--s.fill];

    // c3_y      tag_y;
    // u3a_cell* cel_u;
    // ur_nref     hed;
    // {
    //   _ur_frame_t *fam_u = ur_temp_peek(&t, sizeof(_ur_frame_t));

    //   tag_y = fam_u->tag_y;
    //   cel_u = fam_u->cel_u;
    //   hed   = fam_u->ref;

    //   ur_temp_pop(&t, sizeof(_ur_frame_t));
    // }

    switch ( fam_u.tag_y ) {
    // switch ( tag_y ) {
      default: {
        c3_assert(0);
      }

      case STACK_ROOT: {
        break;
      }

      case STACK_HEAD: {
        _stack_push(&s, STACK_TAIL, fam_u.cel_u, ref);
        // {
        //   _ur_frame_t *fam_u = ur_temp_push(&t, sizeof(_ur_frame_t));
        //   fam_u->tag_y = STACK_TAIL;
        //   fam_u->cel_u = cel_u;
        //   fam_u->ref   = ref;
        // }

        a = fam_u.cel_u->tel;
        // a = cel_u->tel;
        goto advance;
      }

      case STACK_TAIL: {
        u3a_cell* cel_u = fam_u.cel_u;
        u3a_box*  box_u = u3a_botox(cel_u);
        c3_w*     box_w = (void*)box_u;

        ref = ur_cons(r, fam_u.ref, ref);
        // ref = ur_cons(r, hed, ref);

        //  overwrite u3a_atom with reallocated reference
        //
        box_w[0] = 0xffffffff;
        box_w[1] = ref & 0xffffffff;
        box_w[2] = ref >> 32;

        goto retreat;
      }
    }
  }

  free(s.entries);
  // free(t.base);

  return ref;
}

typedef struct ur_nvec_s {
  void*    data;
  uint64_t fill;
  ur_nref* refs;
} ur_nvec_t;

void
ur_nvec_init(ur_nvec_t *v, uint64_t size, void* ptr)
{
  v->data = ptr;
  v->fill = 0;
  v->refs = calloc(size, sizeof(ur_nref));
}

//  XX u3h_use()
static c3_w
_hamt_count(u3p(u3h_root) har_p)
{
  u3h_root* har_u = u3to(u3h_root, har_p);
  return har_u->use_w;
}

static void
_from_hamt(u3_noun kev, void* ptr)
{
  ur_nvec_t *v = (ur_nvec_t*)ptr;
  ur_root_t *r = v->data;

  v->refs[v->fill++] = _from_loom(r, kev);
}

static u3_noun
_ref_to_noun(ur_nref ref, u3_noun* vat, u3_noun* cel)
{
  switch ( ur_nref_tag(ref) ) {
    default: assert(0);

    case ur_direct: {
      if ( 0x7fffffffULL > ref ) {
        return (u3_atom)ref;
      }
      else {
        c3_w wor_w[2];

        wor_w[0] = ref & 0xffffffff;
        wor_w[1] = ref >> 32;

        return u3i_words(2, wor_w);
      }
    } break;

    case ur_iatom:  return vat[ur_nref_idx(ref)];

    case ur_icell:  return cel[ur_nref_idx(ref)];
  }
}

void
do_stuff(void)
{
  ur_root_t *r = ur_hcon_init();

  //  allow read/write on the whole loom, bypassing page tracking
  //
  if ( 0 != mprotect((void *)u3_Loom, u3a_bytes, (PROT_READ | PROT_WRITE)) ) {
    c3_assert(0);
  }

  fprintf(stderr, "hc: cells fill %" PRIu64 " size %" PRIu64 "\r\n", r->cells.fill, r->cells.size);
  fprintf(stderr, "hc: atoms fill %" PRIu64 " size %" PRIu64 "\r\n", r->atoms.fill, r->atoms.size);

  ur_nref  ken = _from_loom(r, u3A->roc);

  fprintf(stderr, "hc: cells fill %" PRIu64 " size %" PRIu64 "\r\n", r->cells.fill, r->cells.size);
  fprintf(stderr, "hc: atoms fill %" PRIu64 " size %" PRIu64 "\r\n", r->atoms.fill, r->atoms.size);


  c3_w   cod_w = _hamt_count(u3R->jed.cod_p);
  ur_nvec_t  v;

  fprintf(stderr, "hc: cold count %u\r\n", cod_w);

  ur_nvec_init(&v, cod_w, r);
  u3h_walk_with(u3R->jed.cod_p, _from_hamt, &v);

  fprintf(stderr, "hc: cells fill %" PRIu64 " size %" PRIu64 "\r\n", r->cells.fill, r->cells.size);
  fprintf(stderr, "hc: atoms fill %" PRIu64 " size %" PRIu64 "\r\n", r->atoms.fill, r->atoms.size);

  u3m_pave(c3y, c3n);
  // XX wtf?
  u3R->jed.hot_p = u3h_new();

  u3_atom *vat;
  u3_noun *cel;

  {
    ur_atoms_t *atoms = &(r->atoms);
    uint64_t    *lens = atoms->lens;
    uint8_t    **byts = atoms->bytes;
    uint64_t  i, fill = atoms->fill;

    vat = calloc(fill, sizeof(u3_atom));

    for ( i = 0; i < fill; i++ ) {
      vat[i] = u3i_bytes(lens[i], byts[i]);
      //  XX mug?
    }
  }

  {
    ur_cells_t *cells = &(r->cells);
    ur_nref     *heds = cells->heads, *tals = cells->tails;
    uint64_t  i, fill = cells->fill;
    u3_noun  hed, tal;

    cel = calloc(fill, sizeof(u3_noun));

    for ( i = 0; i < fill; i++ ) {
      hed = _ref_to_noun(heds[i], vat, cel);
      tal = _ref_to_noun(tals[i], vat, cel);
      cel[i] = u3nc(hed, tal);
      //  XX mug?
    }
  }

  u3A->roc = cel[ur_nref_idx(ken)];

  {
    uint32_t  i;
    ur_nref ref;
    u3_noun kev;

    for ( i = 0; i < cod_w; i++) {
      ref = v.refs[i];
      kev = cel[ur_nref_idx(ref)];
      u3h_put(u3R->jed.cod_p, u3h(kev), u3k(u3t(kev)));
      u3z(kev);
    }
  }

  //  mark all pages dirty
  //
  memset((void*)u3P.dit_w, 0xff, u3a_pages >> 3);
}

/* _cw_uniq(); deduplicate persistent nouns
*/
static void
_cw_uniq(c3_i argc, c3_c* argv[])
{
  c3_assert( 3 <= argc );

  c3_c* dir_c = argv[2];
  c3_d  eve_d = u3m_boot(dir_c);

  u3_serf_grab();

  do_stuff();

  u3_serf_grab();

  u3A->ent_d = eve_d;

  u3e_save();
}

/* _cw_pack(); compact memory, save, and exit.
*/
static void
_cw_pack(c3_i argc, c3_c* argv[])
{
  c3_assert( 3 <= argc );

  c3_c* dir_c = argv[2];

  u3m_boot(dir_c);
  u3a_print_memory(stderr, "urbit-worker: pack: gained", u3m_pack());

  u3e_save();
}

/* _cw_usage(): print urbit-worker usage.
*/
static void
_cw_usage(c3_i argc, c3_c* argv[])
{
  fprintf(stderr,
          "\rurbit-worker usage:\n"
          "  print pier info:\n"
          "    %s info <pier>\n\n"
          "  gc persistent state:\n"
          "    %s grab <pier>\n\n"
          "  compact persistent state:\n"
          "    %s pack <pier>\n\n"
          "  jam persistent state:\n"
          "    %s cram <pier>\n\n"
          "  cue persistent state:\n"
          "    %s queu <pier> <at-event>\n\n"
          "  run as a 'serf':\n"
          "    %s serf <pier> <key> <flags> <cache-size> <at-event>\n",
          argv[0], argv[0], argv[0], argv[0], argv[0], argv[0]);
}

/* main(): main() when run as urbit-worker
*/
c3_i
main(c3_i argc, c3_c* argv[])
{
  //  urbit-worker commands and positional arguments, by analogy
  //
  //    $@  ~               ;; usage
  //    $%  [%cram dir=@t]
  //        [%queu dir=@t eve=@ud]
  //        [%pack dir=@t]
  //        [%serf dir=@t key=@t wag=@t hap=@ud eve=@ud]
  //    ==
  //
  //    NB: don't print to anything other than stderr;
  //    other streams may have special requirements (in the case of "serf")
  //
  if ( 2 > argc ) {
    _cw_usage(argc, argv);
    exit(1);
  }
  else {
    if ( 0 == strcmp("serf", argv[1]) ) {
      _cw_serf_commence(argc, argv);
    }
    else if ( 0 == strcmp("info", argv[1]) ) {
      _cw_info(argc, argv);
    }
    else if ( 0 == strcmp("grab", argv[1]) ) {
      _cw_grab(argc, argv);
    }
    else if ( 0 == strcmp("cram", argv[1]) ) {
      _cw_cram(argc, argv);
    }
    else if ( 0 == strcmp("queu", argv[1]) ) {
      _cw_queu(argc, argv);
    }
    else if ( 0 == strcmp("uniq", argv[1]) ) {
      _cw_uniq(argc, argv);
    }
    else if ( 0 == strcmp("pack", argv[1]) ) {
      _cw_pack(argc, argv);
    }
    else {
      fprintf(stderr, "unknown command '%s'\r\n", argv[1]);
      _cw_usage(argc, argv);
      exit(1);
    }
  }

  return 0;
}
