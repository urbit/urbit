/* g/m.c
**
** This file is in the public domain.
*/
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <ctype.h>
#include <sigsegv.h>

#include "all.h"

static jmp_buf u3_Signal;

#ifndef SIGSTKSZ
# define SIGSTKSZ 16384
#endif
static uint8_t Sigstk[SIGSTKSZ];

void u3_unix_ef_hold(void);         //  suspend system signal regime
void u3_unix_ef_move(void);         //  restore system signal regime

extern void
u3_lo_sway(c3_l tab_l, u3_noun tax);

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

/* _cm_emergency(): write emergency text to stderr, never failing.
*/
static void
_cm_emergency(c3_c* cap_c, c3_l sig_l)
{
  write(2, "\r\n", 2);
  write(2, cap_c, strlen(cap_c));

  if ( sig_l ) {
    write(2, ": ", 2);
    write(2, &sig_l, 4);
  }
  write(2, "\r\n", 2);
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

static void
_cm_signal_handle_over(int emergency, stackoverflow_context_t scp)
{
  _cm_signal_handle(c3__over);
}

static void
_cm_signal_handle_term(int x)
{
  //  Ignore if we are using base memory from work memory, very rare.
  //
  if ( (0 != u3H->rod_u.kid_u) && (&(u3H->rod_u) == u3R) ) {
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
  u3R->kid_u = 0;
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
    u3H->rod_u.how.fag_w |= u3a_flag_gc;

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

    //  Descend to the innermost trace, collecting stack.
    //
    {
      u3a_road* rod_u;

      u3R = &(u3H->rod_u);
      rod_u = u3R;
      
      while ( rod_u->kid_u ) {
        tax = u3_ckb_weld(u3a_take(rod_u->kid_u->bug.tax), tax);
        rod_u = rod_u->kid_u;
      }
    }

    pro = u3nt(3, sig_l, tax);
    _cm_signal_reset();

    u3z(arg);
    return pro;
  }
}
 
/* _cm_signal_deep(): start deep processing; set timer for sec_w or 0.
*/
static void
_cm_signal_deep(c3_w sec_w)
{
  u3_unix_ef_hold();

  stackoverflow_install_handler(_cm_signal_handle_over, Sigstk, SIGSTKSZ);
  signal(SIGINT, _cm_signal_handle_intr);
  signal(SIGTERM, _cm_signal_handle_term);

  // Provide a little emergency memory, for use in case things 
  // go utterly haywire.
  //
  if ( 0 == u3H->rod_u.bug.mer ) {
    u3H->rod_u.bug.mer = u3i_string("emergency buffer");
  }

  if ( sec_w ) {
    struct itimerval itm_u;

    timerclear(&itm_u.it_interval);
    itm_u.it_value.tv_sec = sec_w;
    itm_u.it_value.tv_usec = 0;

    setitimer(ITIMER_VIRTUAL, &itm_u, 0);
    signal(SIGVTALRM, _cm_signal_handle_alrm);
  }
}

/* _cm_signal_done():
*/
static void
_cm_signal_done()
{
  // signal(SIGINT, SIG_IGN);
  signal(SIGTERM, SIG_IGN);
  signal(SIGVTALRM, SIG_IGN);

  stackoverflow_deinstall_handler();
  {
    struct itimerval itm_u;

    timerclear(&itm_u.it_interval);
    timerclear(&itm_u.it_value);

    setitimer(ITIMER_VIRTUAL, &itm_u, 0);
  }
  u3_unix_ef_move();
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
    fprintf(stderr, "%s: %s\r\n", pas_c, strerror(errno));
    return u3m_bail(c3__fail);
  }
  fln_w = buf_b.st_size;
  pad_y = c3_malloc(buf_b.st_size);

  red_w = read(fid_i, pad_y, fln_w);
  close(fid_i);

  if ( fln_w != red_w ) {
    free(pad_y);
    return u3m_bail(c3__fail);
  }
  else {
    u3_noun pad = u3i_bytes(fln_w, (c3_y *)pad_y);
    free(pad_y);

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
_boot_north(c3_w* mem_w, c3_w siz_w, c3_w len_w)
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

/* _boot_south(): install a south road.
*/
static u3_road*
_boot_south(c3_w* mem_w, c3_w siz_w, c3_w len_w)
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

/* _boot_parts(): build internal tables.
*/
static void
_boot_parts(void)
{
  u3R->cax.har_p = u3h_new();
  u3R->jed.har_p = u3h_new();
  u3R->jed.das = u3nc(u3_nul, u3_nul);
}

/* u3m_mark(): mark all nouns in the road.
*/
void
u3m_mark(void)
{
  u3h_mark(u3R->jed.har_p);
  u3a_mark_noun(u3R->jed.das);
  u3a_mark_noun(u3R->ski.flu);
  u3a_mark_noun(u3R->bug.tax);
  u3a_mark_noun(u3R->bug.mer);
  u3a_mark_noun(u3R->pro.don);
  u3a_mark_noun(u3R->pro.day);
  u3h_mark(u3R->cax.har_p);
}

/* u3m_boot(): instantiate or activate image.
*/
void
u3m_boot(c3_o nuu_o, c3_o bug_o)
{
  if ( c3y == nuu_o ) {
    u3H = (void *)_boot_north(u3_Loom + 1, 
                              c3_wiseof(u3v_home), 
                              u3a_words - 1);
    u3R = &u3H->rod_u;

    _boot_parts();
  } 
  else {
    u3H = (void *)_find_north(u3_Loom + 1, 
                              c3_wiseof(u3v_home), 
                              u3a_words - 1);
    u3R = &u3H->rod_u;
  }

  if ( _(bug_o) ) {
    u3R->how.fag_w |= u3a_flag_debug;
  }
}

/* u3m_clear(): clear all allocated data in road.
*/
void
u3m_clear(void)
{
  u3h_free(u3R->cax.har_p);
  u3h_free(u3R->jed.har_p);
  u3a_lose(u3R->jed.das);
}

#if 0
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
  fprintf(stderr, "dump: hat_w %x, fre_w %x, allocated %x\n",
          hat_w, fre_w, (hat_w - fre_w));

  if ( 0 != (hat_w - fre_w) ) {
    c3_w* box_w = _(u3a_is_north(u3R)) ? u3R->rut_w : u3R->hat_w;
    c3_w  mem_w = 0;

    while ( box_w < (_(u3a_is_north(u3R)) ? u3R->hat_w : u3R->rut_w) ) {
      u3a_box* box_u = (void *)box_w;

      if ( 0 != box_u->use_w ) {
#ifdef U3_MEMORY_DEBUG
        // printf("live %d words, code %x\n", box_u->siz_w, box_u->cod_w);
#endif
        mem_w += box_u->siz_w;
      }
      box_w += box_u->siz_w;
    }

    fprintf(stderr, "second count: %x\n", mem_w);
  }
}
#endif

c3_w Exit;

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
**    %need               ::  namespace block
**    %meme               ::  out of memory
**
**  These are equivalents of the full exception noun, the error ball:
**
**    $%  [%0 success]
**        [%1 paths]
**        [%2 trace]
**        [%3 code trace]
**    ==
*/ 
c3_i
u3m_bail(u3_noun how)
{
  if ( (c3__exit == how) && (u3R == &u3H->rod_u) ) {
    abort();
  }
  if ( c3__fail == how ) {
    abort();
  }
  if ( c3__foul == how ) {
    abort();
  }
  /* Printf some metadata.
  */
  if ( c3__exit != how ) {
    if ( _(u3ud(how)) ) {
      c3_c str_c[5];

      str_c[0] = ((how >> 0) & 0xff);
      str_c[1] = ((how >> 8) & 0xff);
      str_c[2] = ((how >> 16) & 0xff);
      str_c[3] = ((how >> 24) & 0xff);
      str_c[4] = 0;
      fprintf(stderr, "bail: %s\r\n", str_c);
    } 
    else {
      c3_assert(_(u3ud(u3h(how))));

      fprintf(stderr, "bail: %d\r\n", u3h(how));
      u3m_p("bail", u3t(how));
    }
  }

  if ( c3__oops == how ) {
    abort();
  }

  if ( &(u3H->rod_u) == u3R ) {
    //  For top-level errors, which shouln't happen often, we have no
    //  choice but to use the signal process; and we require the flat
    //  form of how.
    //
    c3_assert(_(u3a_is_cat(how)));
    u3m_signal(how);
  }

  /* Reconstruct a correct error ball.
  */
  {
    if ( _(u3ud(how)) ) {
      switch ( how ) {
        case c3__exit: {
          how = u3nc(2, u3R->bug.tax);
          break;
        }
        case c3__need: {
          c3_assert(0);
        }
        default: {
          how = u3nt(3, how, u3R->bug.tax);
          break;
        }
      }
    }
  }
 
  /* Longjmp, with an underscore.
  */
  _longjmp(u3R->esc.buf, how);
  return 0;
}

int c3_cooked() { return u3m_bail(c3__oops); }

/* u3m_error(): bail out with %exit, ct_pushing error.
*/
c3_i
u3m_error(c3_c* str_c)
{
  fprintf(stderr, "error: %s\r\n", str_c);   // rong
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
    if ( pad_w < u3R->all.fre_w ) {
      pad_w = 0;
    } 
    else {
      pad_w -= u3R->all.fre_w;
    }
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

      rod_u = _boot_south(u3a_into(bot_p), c3_wiseof(u3a_road), len_w);
#if 0
      fprintf(stderr, "leap: from north %p (cap %x), to south %p\r\n",
              u3R,
              u3R->cap_p + len_p,
              rod_u); 
#endif
    }
    else {
      bot_p = u3R->cap_p;
      u3R->cap_p += len_w;

      rod_u = _boot_north(u3a_into(bot_p), c3_wiseof(u3a_road), len_w);
#if 0
      fprintf(stderr, "leap: from north %p (cap %p), to south %p\r\n",
              u3R,
              u3R->cap_p - len_p,
              rod_u); 
#endif
    }
  }

  /* Attach the new road to its parents.
  */
  {
    c3_assert(0 == u3R->kid_u);
    rod_u->par_u = u3R;
    u3R->kid_u = rod_u;
  }

  /* Set up the new road.
  */
  {
    if ( u3R->how.fag_w & u3a_flag_debug ) {
      rod_u->how.fag_w |= u3a_flag_debug;
    }
    u3R = rod_u;
    _boot_parts();
  }
}

/* u3m_fall(): in u3R, return an inner road to its parent.
*/
void
u3m_fall()
{
  c3_assert(0 != u3R->par_u);

#if 0
  fprintf(stderr, "fall: from %s %p, to %s %p (cap %p, was %p)\r\n",
          _(u3a_is_north(u3R)) ? "north" : "south",
          u3R,
          _(u3a_is_north(u3R)) ? "north" : "south",
          u3R->par_u,
          u3R->hat_w,
          u3R->rut_w);
#endif

  /* The new cap is the old hat - it's as simple as that.
  */
  u3R->par_u->cap_p = u3R->hat_p;

  /* And, we're back home.
  */
  u3R = u3R->par_u;
  u3R->kid_u = 0;
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
  u3_noun das           = u3R->jed.das;
  u3p(u3h_root) har_p = u3R->jed.har_p;

  u3m_fall();

  pro = u3a_take(pro);

  u3j_reap(das, har_p);

  u3R->cap_p = u3R->ear_p;
  u3R->ear_p = 0;

  return pro;
}

/* u3m_golf(): record cap_p length for u3_flog().
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

  *low_w = (u3H->rod_u.hat_p - u3H->rod_u.rut_p);
  *hig_w = (u3H->rod_u.mat_p - u3H->rod_u.cap_p) + c3_wiseof(u3v_home);
}

/* u3m_soft_top(): top-level safety wrapper.
*/
u3_noun 
u3m_soft_top(c3_w    sec_w,                     //  timer seconds
               c3_w    pad_w,                     //  base memory pad
               u3_funk fun_f,
               u3_noun arg)
{
  u3_noun why, pro;
  c3_l    sig_l;

  /* Enter internal signal regime.
  */
  _cm_signal_deep(0);

  if ( 0 != (sig_l = sigsetjmp(u3_Signal, 1)) ) {
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
  if ( 0 == (why = u3m_trap()) ) {
    pro = fun_f(arg);

    /* Make sure the inner routine did not create garbage.
    */
    if ( u3R->how.fag_w & u3a_flag_debug ) {
      u3e_grab("top", pro, u3_none);
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
  u3_noun pro, pru = u3m_soft_top(0, 32768, fun_f, arg);

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

/* u3m_soft_run(): descend into virtualization context.
*/
u3_noun 
u3m_soft_run(u3_noun fly,
               u3_funq fun_f,
               u3_noun aga,
               u3_noun agb)
{
  u3_noun why, pro;

  /* Record the cap, and leap.
  */
  u3m_hate(32768);
 
  /* Configure the new road.
  */
  {
    u3R->ski.flu = u3nc(fly, u3R->par_u->ski.flu);
    u3R->pro.don = u3R->par_u->pro.don;
    u3R->bug.tax = 0;
  }

  /* Trap for exceptions.
  */
  if ( 0 == (why = u3m_trap()) ) {
    pro = fun_f(aga, agb);

    if ( u3R->how.fag_w & u3a_flag_debug ) {
      u3e_grab("top", pro, u3_none);
    }
 
    /* Produce success, on the old road.
    */
    pro = u3nc(0, u3m_love(pro));
  }
  else {
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
                  u3_ckb_weld(u3t(yod), u3k(u3R->bug.tax))));
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
    u3z(fly);
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
u3m_soft_esc(u3_noun sam)
{
  u3_noun why, fly, pro;
 
  /* Assert preconditions. 
  */
  {
    c3_assert(0 != u3R->ski.flu);
    fly = u3h(u3R->ski.flu);
  }

  /* Record the cap, and leap.
  */
  u3m_hate(32768);
 
  /* Configure the new road.
  */
  {
    u3R->ski.flu = u3t(u3R->par_u->ski.flu);
    u3R->pro.don = u3R->par_u->pro.don;
    u3R->bug.tax = 0;
  }

  /* Trap for exceptions.
  */
  if ( 0 == (why = u3m_trap()) ) {
    pro = u3n_slam_on(fly, sam);

    /* Fall back to the old road, leaving temporary memory intact.
    */
    pro = u3m_love(pro);
  }
  else {
    /* Push the error back up to the calling context - not the run we
    ** are in, but the caller of the run, matching pure nock semantics.
    */
    u3m_bail(u3nc(4, u3m_love(why)));
  }

  /* Release the sample.
  */
  u3z(sam);

  /* Return the product.
  */
  return pro;
}

/* u3m_soft(): top-level wrapper.  
**
** Produces [0 product] or [%error (list tank)], top last.
*/
u3_noun 
u3m_soft(c3_w    sec_w,
           u3_funk fun_f,
           u3_noun arg)
{
  u3_noun why;
 
  why = u3m_soft_top(sec_w, (1 << 17), fun_f, arg);   // 512K pad

  if ( 0 == u3h(why) ) {
    return why;
  } else {
    u3_noun tax, cod, pro, mok;

    c3_assert(1 != u3h(why));  //  don't use .^ at the top level!
    
    if ( 2 == u3h(why) ) {
      cod = c3__exit;
      tax = u3k(u3t(why));
    } 
    else {
      c3_assert(3 == u3h(why));

      cod = u3k(u3h(u3t(why)));
      tax = u3k(u3t(u3t(why)));
    }
    mok = u3_dc("mook", 2, tax);
    pro = u3nc(cod, u3k(u3t(mok)));

    u3z(mok);
    u3z(why);

    return pro;
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
        c3_c *buf_c = malloc(2 + (2 * len_w) + 1);
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

        free(buf_c);
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
  c3_c* pre_c = malloc(len_w + 1);

  _cm_in_pretty(som, c3y, pre_c);
  pre_c[len_w] = 0;
  return pre_c;
}

/* u3m_p(): dumb print with caption.
*/
void
u3m_p(const c3_c* cap_c, u3_noun som)
{
  c3_c* pre_c = u3m_pretty(som);

  fprintf(stderr, "%s: %s\r\n", cap_c, pre_c);
  free(pre_c);
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
