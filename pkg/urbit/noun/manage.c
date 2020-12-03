/* n/m.c
**
*/
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <ctype.h>
#include <sigsegv.h>
#include <curl/curl.h>
#include <openssl/crypto.h>

#include "all.h"
#include "vere/vere.h"

//  XX stack-overflow recovery should be gated by -a
//
#undef NO_OVERFLOW

      /* (u3_noun)setjmp(u3R->esc.buf): setjmp within road.
      */
#if 0
        c3_o
        u3m_trap(void);
#else
#       define u3m_trap() (u3_noun)(_setjmp(u3R->esc.buf))
#endif

      /* u3m_signal(): treat a nock-level exception as a signal interrupt.
      */
        void
        u3m_signal(u3_noun sig_l);

      /* u3m_dump(): dump the current road to stderr.
      */
        void
        u3m_dump(void);

      /* u3m_fall(): return to parent road.
      */
        void
        u3m_fall(void);

      /* u3m_leap(): in u3R, create a new road within the existing one.
      */
        void
        u3m_leap(c3_w pad_w);

      /* u3m_golf(): record cap length for u3m_flog().
      */
        c3_w
        u3m_golf(void);

      /* u3m_flog(): pop the cap.
      **
      **    A common sequence for inner allocation is:
      **
      **    c3_w gof_w = u3m_golf();
      **    u3m_leap();
      **    //  allocate some inner stuff...
      **    u3m_fall();
      **    //  inner stuff is still valid, but on cap
      **    u3m_flog(gof_w);
      **
      ** u3m_flog(0) simply clears the cap.
      */
        void
        u3m_flog(c3_w gof_w);

      /* u3m_soft_top(): top-level safety wrapper.
      */
        u3_noun
        u3m_soft_top(c3_w    mil_w,                     //  timer ms
                     c3_w    pad_w,                     //  base memory pad
                     u3_funk fun_f,
                     u3_noun   arg);


static sigjmp_buf u3_Signal;

#ifndef SIGSTKSZ
# define SIGSTKSZ 16384
#endif
#ifndef NO_OVERFLOW
static uint8_t Sigstk[SIGSTKSZ];
#endif

#if 0
/* _cm_punt(): crudely print trace.
*/
static void
_cm_punt(u3_noun tax)
{
  u3_noun xat;

  for ( xat = tax; xat; xat = u3t(xat) ) {
    u3m_p("&", u3h(xat));
  }
}
#endif

static void _write(int  fd,  const  void  *buf,  size_t count)
{
  if (count != write(fd,  buf, count)){
    u3l_log("write failed\r\n");
    c3_assert(0);
  }
}


/* _cm_emergency(): write emergency text to stderr, never failing.
*/
static void
_cm_emergency(c3_c* cap_c, c3_l sig_l)
{
  _write(2, "\r\n", 2);
  _write(2, cap_c, strlen(cap_c));

  if ( sig_l ) {
    _write(2, ": ", 2);
    _write(2, &sig_l, 4);
  }
  _write(2, "\r\n", 2);
}

static void _cm_overflow(void *arg1, void *arg2, void *arg3)
{
  (void)(arg1);
  (void)(arg2);
  (void)(arg3);
  siglongjmp(u3_Signal, c3__over);
}

/* _cm_signal_handle(): handle a signal in general.
*/
static void
_cm_signal_handle(c3_l sig_l)
{
  if ( c3__over == sig_l ) {
    sigsegv_leave_handler(_cm_overflow, NULL, NULL, NULL);
  }
  else {
    siglongjmp(u3_Signal, sig_l);
  }
}

#ifndef NO_OVERFLOW
static void
_cm_signal_handle_over(int emergency, stackoverflow_context_t scp)
{
  _cm_signal_handle(c3__over);
}
#endif

static void
_cm_signal_handle_term(int x)
{
  //  Ignore if we are using base memory from work memory, very rare.
  //
  if ( (0 != u3H->rod_u.kid_p) && (&(u3H->rod_u) == u3R) ) {
    _cm_emergency("ignored", c3__term);
  }
  else {
    _cm_signal_handle(c3__term);
  }
}

static void
_cm_signal_handle_intr(int x)
{
  //  Interrupt: stop work.  Ignore if not working, or (rarely) using base.
  //
  if ( &(u3H->rod_u) == u3R ) {
    _cm_emergency("ignored", c3__intr);
  }
  else {
    _cm_signal_handle(c3__intr);
  }
}

static void
_cm_signal_handle_alrm(int x)
{
  _cm_signal_handle(c3__alrm);
}

/* _cm_signal_reset(): reset top road after signal longjmp.
*/
static void
_cm_signal_reset(void)
{
  u3R = &u3H->rod_u;
  u3R->cap_p = u3R->mat_p;
  u3R->ear_p = 0;
  u3R->kid_p = 0;
}

#if 0
/* _cm_stack_recover(): recover stack trace, with lacunae.
*/
static u3_noun
_cm_stack_recover(u3a_road* rod_u)
{
  c3_w len_w;

  len_w = 0;
  {
    u3_noun tax = rod_u->bug.tax;

    while ( tax ) {
      len_w++;
      tax = u3t(tax);
    }

    if ( len_w < 4096 ) {
      return u3a_take(rod_u->bug.tax);
    }
    else {
      u3_noun beg, fin;
      c3_w i_w;

      tax = rod_u->bug.tax;
      beg = u3_nul;
      for ( i_w = 0; i_w < 2048; i_w++ ) {
        beg = u3nc(u3a_take(u3h(tax)), beg);
        tax = u3t(tax);
      }
      beg = u3kb_flop(beg);

      for ( i_w = 0; i_w < (len_w - 4096); i_w++ ) {
        tax = u3t(tax);
      }
      fin = u3nc(u3nc(c3__lose, c3__over), u3a_take(tax));

      return u3kb_weld(beg, fin);
    }
  }
}
#endif

/* _cm_stack_unwind(): unwind to the top level, preserving all frames.
*/
static u3_noun
_cm_stack_unwind(void)
{
  u3_noun tax;

  while ( u3R != &(u3H->rod_u) ) {
    u3_noun yat = u3m_love(u3R->bug.tax);

    u3R->bug.tax = u3kb_weld(yat, u3R->bug.tax);
  }
  tax = u3R->bug.tax;

  u3R->bug.tax = 0;
  return tax;
}

/* _cm_signal_recover(): recover from a deep signal, after longjmp.  Free arg.
*/
static u3_noun
_cm_signal_recover(c3_l sig_l, u3_noun arg)
{
  u3_noun tax;

  //  Unlikely to be set, but it can be made to happen.
  //
  tax = u3H->rod_u.bug.tax;
  u3H->rod_u.bug.tax = 0;

  if ( &(u3H->rod_u) == u3R ) {
    //  A top-level crash - rather odd.  We should GC.
    //
    _cm_emergency("recover: top", sig_l);
    u3C.wag_w |= u3o_check_corrupt;

    //  Reset the top road - the problem could be a fat cap.
    //
    _cm_signal_reset();

    if ( (c3__meme == sig_l) && (u3a_open(u3R) <= 256) ) {
      // Out of memory at the top level.  Error becomes c3__full,
      // and we release the emergency buffer.  To continue work,
      // we need to readjust the image, eg, migrate to 64 bit.
      //
      u3z(u3R->bug.mer);
      u3R->bug.mer = 0;
      sig_l = c3__full;
    }
    return u3nt(3, sig_l, tax);
  }
  else {
    u3_noun pro;

    //  A signal was generated while we were within Nock.
    //
    _cm_emergency("recover: dig", sig_l);

#if 0
    //  Descend to the innermost trace, collecting stack.
    //
    {
      u3a_road* rod_u;

      u3R = &(u3H->rod_u);
      rod_u = u3R;

      while ( rod_u->kid_p ) {
#if 0
        u3l_log("collecting %d frames\r\n",
              u3kb_lent((u3to(u3_road, rod_u->kid_p)->bug.tax));
#endif
        tax = u3kb_weld(_cm_stack_recover(u3to(u3_road, rod_u->kid_p)), tax);
        rod_u = u3to(u3_road, rod_u->kid_p);
      }
    }
#else
    tax = _cm_stack_unwind();
#endif
    pro = u3nt(3, sig_l, tax);
    _cm_signal_reset();

    u3z(arg);
    return pro;
  }
}

/* _cm_signal_deep(): start deep processing; set timer for [mil_w] or 0.
*/
static void
_cm_signal_deep(c3_w mil_w)
{
  //  disable outer system signal handling
  //
  if ( 0 != u3C.sign_hold_f ) {
    u3C.sign_hold_f();
  }

#ifndef NO_OVERFLOW
  stackoverflow_install_handler(_cm_signal_handle_over, Sigstk, SIGSTKSZ);
#endif
  signal(SIGINT, _cm_signal_handle_intr);
  signal(SIGTERM, _cm_signal_handle_term);

  // Provide a little emergency memory, for use in case things
  // go utterly haywire.
  //
  if ( 0 == u3H->rod_u.bug.mer ) {
    u3H->rod_u.bug.mer = u3i_string("emergency buffer");
  }

  if ( mil_w ) {
    struct itimerval itm_u;

    timerclear(&itm_u.it_interval);
    itm_u.it_value.tv_sec  = (mil_w / 1000);
    itm_u.it_value.tv_usec = 1000 * (mil_w % 1000);

    if ( setitimer(ITIMER_VIRTUAL, &itm_u, 0) ) {
      u3l_log("loom: set timer failed %s\r\n", strerror(errno));
    }
    else {
      signal(SIGVTALRM, _cm_signal_handle_alrm);
    }
  }

  u3t_boot();
}

/* _cm_signal_done():
*/
static void
_cm_signal_done()
{
  signal(SIGINT, SIG_IGN);
  signal(SIGTERM, SIG_IGN);
  signal(SIGVTALRM, SIG_IGN);

#ifndef NO_OVERFLOW
  stackoverflow_deinstall_handler();
#endif
  {
    struct itimerval itm_u;

    timerclear(&itm_u.it_interval);
    timerclear(&itm_u.it_value);

    if ( setitimer(ITIMER_VIRTUAL, &itm_u, 0) ) {
      u3l_log("loom: clear timer failed %s\r\n", strerror(errno));
    }
  }

  //  restore outer system signal handling
  //
  if ( 0 != u3C.sign_move_f ) {
    u3C.sign_move_f();
  }

  u3t_boff();
}

/* u3m_signal(): treat a nock-level exception as a signal interrupt.
*/
void
u3m_signal(u3_noun sig_l)
{
  siglongjmp(u3_Signal, sig_l);
}

/* u3m_file(): load file, as atom, or bail.
*/
u3_noun
u3m_file(c3_c* pas_c)
{
  struct stat buf_b;
  c3_i        fid_i = open(pas_c, O_RDONLY, 0644);
  c3_w        fln_w, red_w;
  c3_y*       pad_y;

  if ( (fid_i < 0) || (fstat(fid_i, &buf_b) < 0) ) {
    u3l_log("%s: %s\r\n", pas_c, strerror(errno));
    return u3m_bail(c3__fail);
  }
  fln_w = buf_b.st_size;
  pad_y = c3_malloc(buf_b.st_size);

  red_w = read(fid_i, pad_y, fln_w);
  close(fid_i);

  if ( fln_w != red_w ) {
    c3_free(pad_y);
    return u3m_bail(c3__fail);
  }
  else {
    u3_noun pad = u3i_bytes(fln_w, (c3_y *)pad_y);
    c3_free(pad_y);

    return pad;
  }
}

/* _find_north(): in restored image, point to a north home.
*/
static u3_road*
_find_north(c3_w* mem_w, c3_w siz_w, c3_w len_w)
{
  return (void *) ((mem_w + len_w) - siz_w);
}

#if 0
/* _find_south(): in restored image, point to a south home.
*/
static u3_road*
_find_south(c3_w* mem_w, c3_w siz_w, c3_w len_w)
{
  return (void *)mem_w;
}
#endif

static u3_road*
_pave_north(c3_w* mem_w, c3_w siz_w, c3_w len_w)
{
  c3_w*    rut_w = mem_w;
  c3_w*    hat_w = rut_w;
  c3_w*    mat_w = ((mem_w + len_w) - siz_w);
  c3_w*    cap_w = mat_w;
  u3_road* rod_u = (void*) mat_w;

  // memset(mem_w, 0, 4 * len_w);     // enable in case of corruption
  memset(rod_u, 0, 4 * siz_w);

  rod_u->rut_p = u3of(c3_w, rut_w);
  rod_u->hat_p = u3of(c3_w, hat_w);

  rod_u->mat_p = u3of(c3_w, mat_w);
  rod_u->cap_p = u3of(c3_w, cap_w);

  return rod_u;
}

/* _pave_south(): install a south road.
*/
static u3_road*
_pave_south(c3_w* mem_w, c3_w siz_w, c3_w len_w)
{
  c3_w*    rut_w = (mem_w + len_w);
  c3_w*    hat_w = rut_w;
  c3_w*    mat_w = mem_w;
  c3_w*    cap_w = mat_w + siz_w;
  u3_road* rod_u = (void*) mat_w;

  //  memset(mem_w, 0, 4 * len_w);    //  enable in case of corruption
  memset(rod_u, 0, 4 * siz_w);

  rod_u->rut_p = u3of(c3_w, rut_w);
  rod_u->hat_p = u3of(c3_w, hat_w);

  rod_u->mat_p = u3of(c3_w, mat_w);
  rod_u->cap_p = u3of(c3_w, cap_w);

  return rod_u;
}

/* _pave_parts(): build internal tables.
*/
static void
_pave_parts(void)
{
  u3R->cax.har_p = u3h_new_cache(u3_Host.ops_u.hap_w);
  u3R->jed.war_p = u3h_new();
  u3R->jed.cod_p = u3h_new();
  u3R->jed.han_p = u3h_new();
  u3R->jed.bas_p = u3h_new();
  u3R->byc.har_p = u3h_new();
}

/* u3m_mark(): mark all nouns in the road.
*/
c3_w
u3m_mark(FILE* fil_u)
{
  c3_w tot_w = 0;
  tot_w += u3v_mark(fil_u);
  tot_w += u3j_mark(fil_u);
  tot_w += u3n_mark(fil_u);
  tot_w += u3a_mark_road(fil_u);
  return tot_w;
}

/* u3m_pave(): instantiate or activate image.
*/
void
u3m_pave(c3_o nuu_o, c3_o bug_o)
{
  if ( c3y == nuu_o ) {
    u3H = (void *)_pave_north(u3_Loom + 1,
                              c3_wiseof(u3v_home),
                              u3a_words - 1);
    u3R = &u3H->rod_u;

    _pave_parts();
  }
  else {
    u3H = (void *)_find_north(u3_Loom + 1,
                              c3_wiseof(u3v_home),
                              u3a_words - 1);
    u3R = &u3H->rod_u;
  }
}

#if 0
/* u3m_clear(): clear all allocated data in road.
*/
void
u3m_clear(void)
{
  u3h_free(u3R->cax.har_p);
  u3j_free();
  u3n_free();
}

void
u3m_dump(void)
{
  c3_w hat_w;
  c3_w fre_w = 0;
  c3_w i_w;

  hat_w = _(u3a_is_north(u3R)) ? u3R->hat_w - u3R->rut_w
                                : u3R->rut_w - u3R->hat_w;

  for ( i_w = 0; i_w < u3_cc_fbox_no; i_w++ ) {
    u3a_fbox* fre_u = u3R->all.fre_u[i_w];

    while ( fre_u ) {
      fre_w += fre_u->box_u.siz_w;
      fre_u = fre_u->nex_u;
    }
  }
  u3l_log("dump: hat_w %x, fre_w %x, allocated %x\n",
          hat_w, fre_w, (hat_w - fre_w));

  if ( 0 != (hat_w - fre_w) ) {
    c3_w* box_w = _(u3a_is_north(u3R)) ? u3R->rut_w : u3R->hat_w;
    c3_w  mem_w = 0;

    while ( box_w < (_(u3a_is_north(u3R)) ? u3R->hat_w : u3R->rut_w) ) {
      u3a_box* box_u = (void *)box_w;

      if ( 0 != box_u->use_w ) {
#ifdef U3_MEMORY_DEBUG
        // u3l_log("live %d words, code %x\n", box_u->siz_w, box_u->cod_w);
#endif
        mem_w += box_u->siz_w;
      }
      box_w += box_u->siz_w;
    }

    u3l_log("second count: %x\n", mem_w);
  }
}
#endif

/* u3m_bail(): bail out.  Does not return.
**
**  Bail motes:
**
**    %evil               ::  erroneous cryptography
**    %exit               ::  semantic failure
**    %oops               ::  assertion failure
**    %intr               ::  interrupt
**    %fail               ::  computability failure
**    %over               ::  stack overflow (a kind of %fail)
**    %meme               ::  out of memory
**
**  These are equivalents of the full exception noun, the error ball:
**
**    $%  [%0 success]
**        [%1 paths]
**        [%2 trace]
**        [%3 code trace]
**    ==
**
**  XX several of these abort() calls should be gated by -a
*/
c3_i
u3m_bail(u3_noun how)
{
  if ( (c3__exit == how) && (u3R == &u3H->rod_u) ) {
    abort();
  }

  /* Printf some metadata.
  */
  if ( c3__exit != how && (_(u3ud(how)) || 1 != u3h(how)) ) {
    if ( _(u3ud(how)) ) {
      c3_c str_c[5];

      str_c[0] = ((how >> 0) & 0xff);
      str_c[1] = ((how >> 8) & 0xff);
      str_c[2] = ((how >> 16) & 0xff);
      str_c[3] = ((how >> 24) & 0xff);
      str_c[4] = 0;
      fprintf(stderr, "\r\nbail: %s\r\n", str_c);
    }
    else {
      c3_assert(_(u3ud(u3h(how))));
      fprintf(stderr, "\r\nbail: %d\r\n", u3h(how));
    }
  }

  //  intercept fatal errors
  //
  switch ( how ) {
    case c3__foul:
    case c3__meme:
    case c3__oops: {
      fprintf(stderr, "bailing out\r\n");
      abort();
    }
  }

  if ( &(u3H->rod_u) == u3R ) {
    //  For top-level errors, which shouldn't happen often, we have no
    //  choice but to use the signal process; and we require the flat
    //  form of how.
    //
    c3_assert(_(u3a_is_cat(how)));
    u3m_signal(how);
  }

  /* Reconstruct a correct error ball.
  */
  if ( _(u3ud(how)) ) {
    switch ( how ) {
      case c3__exit: {
        how = u3nc(2, u3R->bug.tax);
      } break;

      default: {
        how = u3nt(3, how, u3R->bug.tax);
      } break;
    }
  }

  /* Longjmp, with an underscore.
  */
  _longjmp(u3R->esc.buf, how);
}

int c3_cooked() { return u3m_bail(c3__oops); }

/* u3m_error(): bail out with %exit, ct_pushing error.
*/
c3_i
u3m_error(c3_c* str_c)
{
  u3t_mean(u3i_string(str_c));
  return u3m_bail(c3__exit);
}

/* u3m_leap(): in u3R, create a new road within the existing one.
*/
void
u3m_leap(c3_w pad_w)
{
  c3_w     len_w;
  u3_road* rod_u;

  /* Measure the pad - we'll need it.
  */
  {
#if 0
    if ( pad_w < u3R->all.fre_w ) {
      pad_w = 0;
    }
    else {
      pad_w -= u3R->all.fre_w;
    }
#endif
    if ( (pad_w + c3_wiseof(u3a_road)) >= u3a_open(u3R) ) {
      u3m_bail(c3__meme);
    }
    len_w = u3a_open(u3R) - (pad_w + c3_wiseof(u3a_road));
  }

  /* Allocate a region on the cap.
  */
  {
    u3p(c3_w) bot_p;

    if ( c3y == u3a_is_north(u3R) ) {
      bot_p = (u3R->cap_p - len_w);
      u3R->cap_p -= len_w;

      rod_u = _pave_south(u3a_into(bot_p), c3_wiseof(u3a_road), len_w);
#if 0
      fprintf(stderr, "leap: from north %p (cap 0x%x), to south %p\r\n",
              u3R,
              u3R->cap_p + len_w,
              rod_u);
#endif
    }
    else {
      bot_p = u3R->cap_p;
      u3R->cap_p += len_w;

      rod_u = _pave_north(u3a_into(bot_p), c3_wiseof(u3a_road), len_w);
#if 0
      fprintf(stderr, "leap: from south %p (cap 0x%x), to north %p\r\n",
              u3R,
              u3R->cap_p - len_w,
              rod_u);
#endif
    }
  }

  /* Attach the new road to its parents.
  */
  {
    c3_assert(0 == u3R->kid_p);
    rod_u->par_p = u3of(u3_road, u3R);
    u3R->kid_p = u3of(u3_road, rod_u);
  }

  /* Set up the new road.
  */
  {
    u3R = rod_u;
    _pave_parts();
  }
#ifdef U3_MEMORY_DEBUG
  rod_u->all.fre_w = 0;
#endif
}

void
_print_diff(c3_c* cap_c, c3_w a, c3_w b)
{
  c3_w diff = a<b ? b-a : a-b;
  u3a_print_memory(stderr, cap_c, diff);
}

/* u3m_fall(): in u3R, return an inner road to its parent.
*/
void
u3m_fall()
{
  c3_assert(0 != u3R->par_p);

#if 0
  /*  If you're printing a lot of these you need to change
   *  u3a_print_memory from fprintf to u3l_log
  */
  fprintf(stderr, "fall: from %s %p, to %s %p (cap 0x%x, was 0x%x)\r\n",
          _(u3a_is_north(u3R)) ? "north" : "south",
          u3R,
          _(u3a_is_north(u3to(u3_road, u3R->par_p))) ? "north" : "south",
          u3to(u3_road, u3R->par_p),
          u3R->hat_p,
          u3R->rut_p);
  _print_diff("unused free", u3R->hat_p, u3R->cap_p);
  _print_diff("freeing", u3R->rut_p, u3R->hat_p);
  _print_diff("stack", u3R->cap_p, u3R->mat_p);
#endif

  u3to(u3_road, u3R->par_p)->pro.nox_d += u3R->pro.nox_d;
  u3to(u3_road, u3R->par_p)->pro.cel_d += u3R->pro.cel_d;

  /* The new cap is the old hat - it's as simple as that.
  */
  u3to(u3_road, u3R->par_p)->cap_p = u3R->hat_p;

  /* And, we're back home.
  */
  u3R = u3to(u3_road, u3R->par_p);
  u3R->kid_p = 0;
}

/* u3m_hate(): new, integrated leap mechanism (enter).
*/
void
u3m_hate(c3_w pad_w)
{
  c3_assert(0 == u3R->ear_p);

  u3R->ear_p = u3R->cap_p;
  u3m_leap(pad_w);
}

/* u3m_love(): return product from leap.
*/
u3_noun
u3m_love(u3_noun pro)
{
  //  save cache pointers from current road
  //
  u3p(u3h_root) byc_p = u3R->byc.har_p;
  u3a_jets      jed_u = u3R->jed;

  //  fallback to parent road (child heap on parent's stack)
  //
  u3m_fall();

  //  copy product and caches off our stack
  //
  pro   = u3a_take(pro);
  jed_u = u3j_take(jed_u);
  byc_p = u3n_take(byc_p);

  //  pop the stack
  //
  u3R->cap_p = u3R->ear_p;
  u3R->ear_p = 0;

  //  integrate junior caches
  //
  u3j_reap(jed_u);
  u3n_reap(byc_p);

  return pro;
}

/* u3m_golf(): record cap_p length for u3m_flog().
*/
c3_w
u3m_golf(void)
{
  if ( c3y == u3a_is_north(u3R) ) {
    return u3R->mat_p - u3R->cap_p;
  }
  else {
    return u3R->cap_p - u3R->mat_p;
  }
}

/* u3m_flog(): reset cap_p.
*/
void
u3m_flog(c3_w gof_w)
{
  //  Enable memsets in case of memory corruption.
  //
  if ( c3y == u3a_is_north(u3R) ) {
    u3_post bot_p = (u3R->mat_p - gof_w);
    // c3_w  len_w = (bot_w - u3R->cap_w);

    // memset(u3R->cap_w, 0, 4 * len_w);
    u3R->cap_p = bot_p;
  }
  else {
    u3_post bot_p = u3R->mat_p + gof_w;
    // c3_w  len_w = (u3R->cap_w - bot_w);

    // memset(bot_w, 0, 4 * len_w);   //
    u3R->cap_p = bot_p;
  }
}

/* u3m_water(): produce watermarks.
*/
void
u3m_water(c3_w* low_w, c3_w* hig_w)
{
  c3_assert(u3R == &u3H->rod_u);

  *low_w = u3a_heap(u3R);
  *hig_w = u3a_temp(u3R) + c3_wiseof(u3v_home);
}

/* u3m_soft_top(): top-level safety wrapper.
*/
u3_noun
u3m_soft_top(c3_w    mil_w,                     //  timer ms
             c3_w    pad_w,                     //  base memory pad
             u3_funk fun_f,
             u3_noun   arg)
{
  u3_noun why, pro;
  c3_l    sig_l;

  /* Enter internal signal regime.
  */
  _cm_signal_deep(mil_w);

  if ( 0 != (sig_l = sigsetjmp(u3_Signal, 1)) ) {
    //  reinitialize trace state
    //
    u3t_init();

    //  return to blank state
    //
    _cm_signal_done();

    //  recover memory state from the top down
    //
    return _cm_signal_recover(sig_l, arg);
  }

  /* Record the cap, and leap.
  */
  u3m_hate(pad_w);

  /* Trap for ordinary nock exceptions.
  */
  if ( 0 == (why = (u3_noun)_setjmp(u3R->esc.buf)) ) {
    pro = fun_f(arg);

    /* Make sure the inner routine did not create garbage.
    */
    if ( u3C.wag_w & u3o_debug_ram ) {
#ifdef U3_CPU_DEBUG
      if ( u3R->all.max_w > 1000000 ) {
        u3a_print_memory(stderr, "execute: top", u3R->all.max_w);
      }
#endif
      u3m_grab(pro, u3_none);
    }

    /* Revert to external signal regime.
    */
    _cm_signal_done();

    /* Produce success, on the old road.
    */
    pro = u3nc(0, u3m_love(pro));
  }
  else {
    /* Overload the error result.
    */
    pro = u3m_love(why);
  }

  /* Revert to external signal regime.
  */
  _cm_signal_done();

  /* Free the argument.
  */
  u3z(arg);

  /* Return the product.
  */
  return pro;
}

/* u3m_soft_sure(): top-level call assumed correct.
*/
u3_noun
u3m_soft_sure(u3_funk fun_f, u3_noun arg)
{
  u3_noun pro, pru = u3m_soft_top(0, (1 << 18), fun_f, arg);

  c3_assert(_(u3du(pru)));
  pro = u3k(u3t(pru));
  u3z(pru);

  return pro;
}

/* u3m_soft_slam: top-level call.
*/
u3_noun _cm_slam(u3_noun arg) { return u3n_slam_on(u3h(arg), u3t(arg)); }
u3_noun
u3m_soft_slam(u3_noun gat, u3_noun sam)
{
  return u3m_soft_sure(_cm_slam, u3nc(gat, sam));
}

/* u3m_soft_nock: top-level nock.
*/
u3_noun _cm_nock(u3_noun arg) { return u3n_nock_on(u3h(arg), u3t(arg)); }
u3_noun
u3m_soft_nock(u3_noun bus, u3_noun fol)
{
  return u3m_soft_sure(_cm_nock, u3nc(bus, fol));
}

/* u3m_soft_run(): descend into virtualization context.
*/
u3_noun
u3m_soft_run(u3_noun gul,
             u3_funq fun_f,
             u3_noun aga,
             u3_noun agb)
{
  u3_noun why = 0, pro;

  /* Record the cap, and leap.
  */
  u3m_hate(1 << 18);

  /* Configure the new road.
  */
  {
    u3R->ski.gul = u3nc(gul, u3to(u3_road, u3R->par_p)->ski.gul);
    u3R->pro.don = u3to(u3_road, u3R->par_p)->pro.don;
    u3R->pro.trace = u3to(u3_road, u3R->par_p)->pro.trace;
    u3R->bug.tax = 0;
  }
  u3t_on(coy_o);

  /* Trap for exceptions.
  */
  if ( 0 == (why = (u3_noun)_setjmp(u3R->esc.buf)) ) {
    u3t_off(coy_o);
    pro = fun_f(aga, agb);

#ifdef U3_CPU_DEBUG
    if ( u3R->all.max_w > 1000000 ) {
      u3a_print_memory(stderr, "execute: run", u3R->all.max_w);
    }
#endif
    /* Produce success, on the old road.
    */
    pro = u3nc(0, u3m_love(pro));
  }
  else {
    u3t_init();

    /* Produce - or fall again.
    */
    {
      c3_assert(_(u3du(why)));
      switch ( u3h(why) ) {
        default: c3_assert(0); return 0;

        case 0: {                             //  unusual: bail with success.
          pro = u3m_love(why);
        } break;

        case 1: {                             //  blocking request
          pro = u3m_love(why);
        } break;

        case 2: {                             //  true exit
          pro = u3m_love(why);
        } break;

        case 3: {                             //  failure; rebail w/trace
          u3_noun yod = u3m_love(u3t(why));

          u3m_bail
            (u3nt(3,
                  u3a_take(u3h(yod)),
                  u3kb_weld(u3t(yod), u3k(u3R->bug.tax))));
        } break;

        case 4: {                             //  meta-bail
          u3m_bail(u3m_love(u3t(why)));
        } break;
      }
    }
  }

  /* Release the arguments.
  */
  {
    u3z(gul);
    u3z(aga);
    u3z(agb);
  }

  /* Return the product.
  */
  return pro;
}

/* u3m_soft_esc(): namespace lookup.  Produces direct result.
*/
u3_noun
u3m_soft_esc(u3_noun ref, u3_noun sam)
{
  u3_noun why, gul, pro;

  /* Assert preconditions.
  */
  {
    c3_assert(0 != u3R->ski.gul);
    gul = u3h(u3R->ski.gul);
  }

  /* Record the cap, and leap.
  */
  u3m_hate(1 << 18);

  /* Configure the new road.
  */
  {
    u3R->ski.gul = u3t(u3to(u3_road, u3R->par_p)->ski.gul);
    u3R->pro.don = u3to(u3_road, u3R->par_p)->pro.don;
    u3R->pro.trace = u3to(u3_road, u3R->par_p)->pro.trace;
    u3R->bug.tax = 0;
  }

  /* Trap for exceptions.
  */
  if ( 0 == (why = (u3_noun)_setjmp(u3R->esc.buf)) ) {
    pro = u3n_slam_on(gul, u3nc(ref, sam));

    /* Fall back to the old road, leaving temporary memory intact.
    */
    pro = u3m_love(pro);
  }
  else {
    u3t_init();

    /* Push the error back up to the calling context - not the run we
    ** are in, but the caller of the run, matching pure nock semantics.
    */
    u3m_bail(u3nc(4, u3m_love(why)));
  }

  /* Release the sample.  Note that we used it above, but in a junior
  ** road, so its refcount is intact.
  */
  u3z(ref);
  u3z(sam);

  /* Return the product.
  */
  return pro;
}

/* u3m_grab(): garbage-collect the world, plus extra roots.
*/
void
u3m_grab(u3_noun som, ...)   // terminate with u3_none
{
  // u3h_free(u3R->cax.har_p);
  // u3R->cax.har_p = u3h_new();

  u3m_mark(0);
  {
    va_list vap;
    u3_noun tur;

    va_start(vap, som);

    if ( som != u3_none ) {
      u3a_mark_noun(som);

      while ( u3_none != (tur = va_arg(vap, u3_noun)) ) {
        u3a_mark_noun(tur);
      }
    }
    va_end(vap);
  }
  u3a_sweep();
}

/* u3m_soft(): top-level wrapper.
**
** Produces [0 product] or [%error (list tank)], top last.
*/
u3_noun
u3m_soft(c3_w    mil_w,
         u3_funk fun_f,
         u3_noun   arg)
{
  u3_noun why;

  why = u3m_soft_top(mil_w, (1 << 20), fun_f, arg);   // 2MB pad

  if ( 0 == u3h(why) ) {
    return why;
  }
  else {
    //  don't use .^ at the top level!
    //
    c3_assert(1 != u3h(why));

    //  don't call +mook if we have no kernel
    //
    //    This is required to soft the boot sequence.
    //    XX produce specific error motes instead of %2?
    //
    if ( 0 == u3A->roc ) {

      {
        u3_noun tax = u3t(u3t(why));

        u3m_p("mote", u3h(u3t(why)));

        while ( u3_nul != tax ) {
          u3_noun dat, mot, val;
          u3x_cell(tax, &dat, &tax);

          if ( c3y == u3r_cell(dat, &mot, &val) ) {
            if ( c3__spot == mot ) {
              u3m_p("tax", val);
            }
            else if (  (c3__mean == mot)
                    && (c3y == u3a_is_atom(val)) )
            {
              u3m_p("men", val);
            }
            else {
              u3m_p("mot", mot);
            }
          }
        }
      }

      u3z(why);
      return u3nc(c3__fail, u3_nul);
    }
    else {
      u3_noun tax, cod, pro, mok;

      if ( 2 == u3h(why) ) {
        cod = c3__exit;
        tax = u3k(u3t(why));
      }
      else {
        c3_assert(3 == u3h(why));

        cod = u3k(u3h(u3t(why)));
        tax = u3k(u3t(u3t(why)));
      }

      mok = u3dc("mook", 2, tax);
      pro = u3nc(cod, u3k(u3t(mok)));

      u3z(mok);
      u3z(why);

      return pro;
    }
  }
}

/* _cm_is_tas(): yes iff som (RETAIN) is @tas.
*/
static c3_o
_cm_is_tas(u3_atom som, c3_w len_w)
{
  c3_w i_w;

  for ( i_w = 0; i_w < len_w; i_w++ ) {
    c3_c c_c = u3r_byte(i_w, som);

    if ( islower(c_c) ||
        (isdigit(c_c) && (0 != i_w) && ((len_w - 1) != i_w))
        || '-' == c_c )
    {
      continue;
    }
    return c3n;
  }
  return c3y;
}

/* _cm_is_ta(): yes iff som (RETAIN) is @ta.
*/
static c3_o
_cm_is_ta(u3_noun som, c3_w len_w)
{
  c3_w i_w;

  for ( i_w = 0; i_w < len_w; i_w++ ) {
    c3_c c_c = u3r_byte(i_w, som);

    if ( (c_c < 32) || (c_c > 127) ) {
      return c3n;
    }
  }
  return c3y;
}

/* _cm_hex(): hex byte.
*/
c3_y _cm_hex(c3_y c_y)
{
  if ( c_y < 10 )
    return '0' + c_y;
  else return 'a' + (c_y - 10);
}

/* _cm_in_pretty: measure/cut prettyprint.
*/
static c3_w
_cm_in_pretty(u3_noun som, c3_o sel_o, c3_c* str_c)
{
  if ( _(u3du(som)) ) {
    c3_w sel_w, one_w, two_w;

    sel_w = 0;
    if ( _(sel_o) ) {
      if ( str_c ) { *(str_c++) = '['; }
      sel_w += 1;
    }

    one_w = _cm_in_pretty(u3h(som), c3y, str_c);
    if ( str_c ) {
      str_c += one_w;
      *(str_c++) = ' ';
    }
    two_w = _cm_in_pretty(u3t(som), c3n, str_c);
    if ( str_c ) { str_c += two_w; }

    if ( _(sel_o) ) {
      if ( str_c ) { *(str_c++) = ']'; }
      sel_w += 1;
    }
    return one_w + two_w + 1 + sel_w;
  }
  else {
    if ( som < 65536 ) {
      c3_c buf_c[6];
      c3_w len_w;

      snprintf(buf_c, 6, "%d", som);
      len_w = strlen(buf_c);

      if ( str_c ) { strcpy(str_c, buf_c); str_c += len_w; }
      return len_w;
    }
    else {
      c3_w len_w = u3r_met(3, som);

      if ( _(_cm_is_tas(som, len_w)) ) {
        c3_w len_w = u3r_met(3, som);

        if ( str_c ) {
          *(str_c++) = '%';
          u3r_bytes(0, len_w, (c3_y *)str_c, som);
          str_c += len_w;
        }
        return len_w + 1;
      }
      else if ( _(_cm_is_ta(som, len_w)) ) {
        if ( str_c ) {
          *(str_c++) = '\'';
          u3r_bytes(0, len_w, (c3_y *)str_c, som);
          str_c += len_w;
          *(str_c++) = '\'';
        }
        return len_w + 2;
      }
      else {
        c3_w len_w = u3r_met(3, som);
        c3_c *buf_c = c3_malloc(2 + (2 * len_w) + 1);
        c3_w i_w = 0;
        c3_w a_w = 0;

        buf_c[a_w++] = '0';
        buf_c[a_w++] = 'x';

        for ( i_w = 0; i_w < len_w; i_w++ ) {
          c3_y c_y = u3r_byte(len_w - (i_w + 1), som);

          if ( (i_w == 0) && (c_y <= 0xf) ) {
            buf_c[a_w++] = _cm_hex(c_y);
          } else {
            buf_c[a_w++] = _cm_hex(c_y >> 4);
            buf_c[a_w++] = _cm_hex(c_y & 0xf);
          }
        }
        buf_c[a_w] = 0;
        len_w = a_w;

        if ( str_c ) { strcpy(str_c, buf_c); str_c += len_w; }

        c3_free(buf_c);
        return len_w;
      }
    }
  }
}

/* u3m_pretty(): dumb prettyprint to string.
*/
c3_c*
u3m_pretty(u3_noun som)
{
  c3_w len_w = _cm_in_pretty(som, c3y, 0);
  c3_c* pre_c = c3_malloc(len_w + 1);

  _cm_in_pretty(som, c3y, pre_c);
  pre_c[len_w] = 0;
  return pre_c;
}

/*  _cm_in_pretty_path: measure/cut prettyprint.
 *
 *  Modeled after _cm_in_pretty(), the backend to u3m_p(), but with the
 *  assumption that we're always displaying a path.
 */
static c3_w
_cm_in_pretty_path(u3_noun som, c3_c* str_c)
{
  if ( _(u3du(som)) ) {
    c3_w sel_w, one_w, two_w;
    if ( str_c ) {
      *(str_c++) = '/';
    }
    sel_w = 1;

    one_w = _cm_in_pretty_path(u3h(som), str_c);
    if ( str_c ) {
      str_c += one_w;
    }

    two_w = _cm_in_pretty_path(u3t(som), str_c);
    if ( str_c ) {
      str_c += two_w;
    }

    return sel_w + one_w + two_w;
  }
  else {
    c3_w len_w = u3r_met(3, som);
    if ( str_c && len_w ) {
      u3r_bytes(0, len_w, (c3_y *)str_c, som);
      str_c += len_w;
    }
    return len_w;
  }
}

/* u3m_pretty_path(): prettyprint a path to string.
*/
c3_c*
u3m_pretty_path(u3_noun som)
{
  c3_w len_w = _cm_in_pretty_path(som, NULL);
  c3_c* pre_c = c3_malloc(len_w + 1);

  _cm_in_pretty_path(som, pre_c);
  pre_c[len_w] = 0;
  return pre_c;
}

/* u3m_p(): dumb print with caption.
*/
void
u3m_p(const c3_c* cap_c, u3_noun som)
{
  c3_c* pre_c = u3m_pretty(som);

  u3l_log("%s: %s\r\n", cap_c, pre_c);
  c3_free(pre_c);
}

/* u3m_tape(): dump a tape to stdout.
*/
void
u3m_tape(u3_noun tep)
{
  u3_noun tap = tep;

  while ( u3_nul != tap ) {
    c3_c car_c;

    if ( u3h(tap) >= 127 ) {
      car_c = '?';
    } else car_c = u3h(tap);

    putc(car_c, stdout);
    tap = u3t(tap);
  }
  u3z(tep);
}

/* u3m_wall(): dump a wall to stdout.
*/
void
u3m_wall(u3_noun wol)
{
  u3_noun wal = wol;

  while ( u3_nul != wal ) {
    u3m_tape(u3k(u3h(wal)));

    putc(13, stdout);
    putc(10, stdout);

    wal = u3t(wal);
  }
  u3z(wol);
}

/* _cm_limits(): set up global modes and limits.
*/
static void
_cm_limits(void)
{
  struct rlimit rlm;

  //  Moar stack.
  //
  {
    c3_assert( 0 == getrlimit(RLIMIT_STACK, &rlm) );

    rlm.rlim_cur = c3_min(rlm.rlim_max, (65536 << 10));

    if ( 0 != setrlimit(RLIMIT_STACK, &rlm) ) {
      u3l_log("boot: stack size: %s\r\n", strerror(errno));
      exit(1);
    }
  }

  //  Moar filez.
  //
  {
    getrlimit(RLIMIT_NOFILE, &rlm);

  #ifdef U3_OS_osx
    rlm.rlim_cur = c3_min(OPEN_MAX, rlm.rlim_max);
  #else
    rlm.rlim_cur = rlm.rlim_max;
  #endif

    //  no exit, not a critical limit
    //
    if ( 0 != setrlimit(RLIMIT_NOFILE, &rlm) ) {
      u3l_log("boot: open file limit: %s\r\n", strerror(errno));
    }
  }

  // Moar core.
  //
# ifndef ASAN_ENABLED
  {
    getrlimit(RLIMIT_CORE, &rlm);
    rlm.rlim_cur = RLIM_INFINITY;

    //  no exit, not a critical limit
    //
    if ( 0 != setrlimit(RLIMIT_CORE, &rlm) ) {
      u3l_log("boot: core limit: %s\r\n", strerror(errno));
    }
  }
# endif
}

/* _cm_signals(): set up interrupts, etc.
*/
static void
_cm_signals(void)
{
  if ( 0 != sigsegv_install_handler(u3e_fault) ) {
    u3l_log("boot: sigsegv install failed\n");
    exit(1);
  }
  // signal(SIGINT, _loom_stop);


  //  Block SIGPROF, so that if/when we reactivate it on the
  //  main thread for profiling, we won't get hits in parallel
  //  on other threads.
  if ( u3C.wag_w & u3o_debug_cpu ) {
    sigset_t set;

    sigemptyset(&set);
    sigaddset(&set, SIGPROF);

    if ( 0 != pthread_sigmask(SIG_BLOCK, &set, NULL) ) {
      u3l_log("boot: thread mask SIGPROF: %s\r\n", strerror(errno));
      exit(1);
    }
  }
}

/* u3m_init(): start the environment.
*/
void
u3m_init(void)
{
  _cm_limits();
  _cm_signals();

  /* Make sure GMP uses our malloc.
  */
  mp_set_memory_functions(u3a_malloc, u3a_realloc2, u3a_free2);

  /* Map at fixed address.
  */
  {
    c3_w  len_w = u3a_bytes;
    void* map_v;

    map_v = mmap((void *)u3_Loom,
                 len_w,
                 (PROT_READ | PROT_WRITE),
                 (MAP_ANON | MAP_FIXED | MAP_PRIVATE),
                 -1, 0);

    if ( -1 == (c3_ps)map_v ) {
      void* dyn_v = mmap((void *)0,
                         len_w,
                         PROT_READ,
                         MAP_ANON | MAP_PRIVATE,
                         -1, 0);

      u3l_log("boot: mapping %dMB failed\r\n", (len_w / (1024 * 1024)));
      u3l_log("see urbit.org/using/install/#about-swap-space"
              " for adding swap space\r\n");
      if ( -1 != (c3_ps)map_v ) {
        u3l_log("if porting to a new platform, try U3_OS_LoomBase %p\r\n",
                dyn_v);
      }
      exit(1);
    }

    u3l_log("loom: mapped %dMB\r\n", len_w >> 20);
  }
}

/* u3m_boot(): start the u3 system. return next event, starting from 1.
*/
c3_d
u3m_boot(c3_c* dir_c)
{
  c3_o nuu_o;

  /* Activate the loom.
  */
  u3m_init();

  /* In the worker, set the openssl memory allocation functions to always
  ** work on the loom.
  */
  CRYPTO_set_mem_functions(u3a_malloc_ssl, u3a_realloc_ssl, u3a_free_ssl);

  /* Activate the storage system.
  */
  nuu_o = u3e_live(c3n, dir_c);

  /* Activate tracing.
  */
  u3C.slog_f = 0;
  u3C.sign_hold_f = 0;
  u3C.sign_move_f = 0;
  u3t_init();

  /* Construct or activate the allocator.
  */
  u3m_pave(nuu_o, c3n);

  /* Initialize the jet system.
  */
  {
    c3_w len_w = u3j_boot(nuu_o);
    u3l_log("boot: installed %d jets\r\n", len_w);
  }

  /* Reactivate jets on old kernel.
  */
  if ( !_(nuu_o) ) {
    u3j_ream();
    u3n_ream();

    //  XX unused, removed
    //
    //    u3z() temporarily preserved to avoid leaking
    //    checkpointed values
    //
    u3z(u3A->wen);
    u3A->wen = 0;

    return u3A->ent_d;
  }
  else {
  /* Basic initialization.
  */
    memset(u3A, 0, sizeof(*u3A));
    return 0;
  }
}

/* u3m_boot_lite(): start without checkpointing.
*/
c3_d
u3m_boot_lite(void)
{
  /* Activate the loom.
  */
  u3m_init();

  /* Activate tracing.
  */
  u3C.slog_f = 0;
  u3C.sign_hold_f = 0;
  u3C.sign_move_f = 0;
  u3t_init();

  /* Construct or activate the allocator.
  */
  u3m_pave(c3y, c3n);

  /* Initialize the jet system.
  */
  u3j_boot(c3y);

  /* Basic initialization.
  */
  memset(u3A, 0, sizeof(*u3A));
  return 0;
}

/* u3m_reclaim: clear persistent caches to reclaim memory
*/
void
u3m_reclaim(void)
{
  u3v_reclaim();
  u3j_reclaim();
  u3n_reclaim();
  u3a_reclaim();
}

/* _cm_pack_rewrite(): trace through arena, rewriting pointers.
*/
static void
_cm_pack_rewrite(void)
{
  //  XX fix u3a_rewrit* to support south roads
  //
  c3_assert( &(u3H->rod_u) == u3R );

  //  NB: these implementations must be kept in sync with u3m_reclaim();
  //  anything not reclaimed must be rewritable
  //
  u3v_rewrite_compact();
  u3j_rewrite_compact();
  u3n_rewrite_compact();
  u3a_rewrite_compact();
}

/* u3m_pack: compact (defragment) memory.
*/
c3_w
u3m_pack(void)
{
  c3_w pre_w = u3a_open(u3R);

  //  reclaim first, to free space, and discard anything we can't/don't rewrite
  //
  u3m_reclaim();

  //  sweep the heap, finding and saving new locations
  //
  u3a_pack_seek(u3R);

  //  trace roots, rewriting inner pointers
  //
  _cm_pack_rewrite();

  //  sweep the heap, relocating objects to their new locations
  //
  u3a_pack_move(u3R);

  return (u3a_open(u3R) - pre_w);
}
