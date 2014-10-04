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

static jmp_buf Signal_u;
static c3_l Signal_l;

#ifndef SIGSTKSZ
# define SIGSTKSZ 16384
#endif
static uint8_t Sigstk[SIGSTKSZ];

void u3_unix_ef_hold(void);         //  suspend system signal regime
void u3_unix_ef_move(void);         //  restore system signal regime

extern void
u3_lo_sway(c3_l tab_l, u3_noun tax);

#if 1
/* _cm_punt(): crudely print trace.
*/
static void
_cm_punt(u3_noun tax)
{
  u3_noun xat;

  for ( xat = tax; xat; xat = u3t(xat) ) {
    u3_cm_p("&", u3h(xat));
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

static void _cm_overflow_over(void *arg1, void *arg2, void *arg3)
{
  (void)(arg1);
  (void)(arg2);
  (void)(arg3);
  siglongjmp(Signal_u, c3__over);
}
static void _cm_overflow_dire(void *arg1, void *arg2, void *arg3)
{
  (void)(arg1);
  (void)(arg2);
  (void)(arg3);
  siglongjmp(Signal_u, c3__dire);
}

/* _cm_signal_handle(): handle a signal in general.
*/
static void
_cm_signal_handle(c3_l sig_l) 
{
  c3_l org_l = sig_l;

  _cm_emergency("signal", sig_l);

  if ( Signal_l || (u3R == &u3H->rod_u) ) {
    _cm_emergency("signal overlap: old", Signal_l);
    _cm_emergency("signal overlap: new", sig_l);

    if ( c3__prof == sig_l ) {
      // Ignore strange profiling event.
      //
      return;
    }
    sig_l = c3__dire;
  }
  Signal_l = sig_l;

  if ( c3__over == org_l ) {
    if ( c3__dire == sig_l ) {
      sigsegv_leave_handler(_cm_overflow_dire, NULL, NULL, NULL);
    } 
    else {
      sigsegv_leave_handler(_cm_overflow_over, NULL, NULL, NULL);
    }
  }
  else {
    siglongjmp(Signal_u, sig_l);
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
  _cm_signal_handle(c3__term);
}

static void
_cm_signal_handle_intr(int x)
{
  _cm_signal_handle(c3__intr);
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
  u3R->cap_w = u3R->mat_w;
  u3R->kid_u = 0;
}

/* _cm_signal_recover(): recover from a deep signal, after longjmp.
*/
static u3_noun
_cm_signal_recover(void)
{
  _cm_emergency("recover", Signal_l);

  if ( c3__dire == Signal_l ) {
    Signal_l = 0;
    _cm_signal_reset();
    return u3nt(3, c3__dire, 0);
  }
  else {
    u3_noun tax = u3_nul;
    u3_noun pro;

    //  Descend to the road that caused the problem - in almost all
    //  cases the innermost road, except for a 
    //
    {
      u3_cs_road* rod_u;

      u3R = &(u3H->rod_u);
      rod_u = u3R;
      
      while ( rod_u->kid_u ) {
        tax = u3_ckb_weld(u3_ca_take(rod_u->kid_u->bug.tax), tax);
        rod_u = rod_u->kid_u;
      }
    }
    _cm_signal_reset();
    pro = u3nt(3, Signal_l, tax);

    _cm_punt(tax);
    u3_lo_sway(2, u3k(tax));

    Signal_l = 0;
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

/* u3_cm_file(): load file, as atom, or bail.
*/
u3_noun
u3_cm_file(c3_c* pas_c)
{
  struct stat buf_b;
  c3_i        fid_i = open(pas_c, O_RDONLY, 0644);
  c3_w        fln_w, red_w;
  c3_y*       pad_y;

  if ( (fid_i < 0) || (fstat(fid_i, &buf_b) < 0) ) {
    fprintf(stderr, "%s: %s\r\n", pas_c, strerror(errno));
    return u3_cm_bail(c3__fail);
  }
  fln_w = buf_b.st_size;
  pad_y = c3_malloc(buf_b.st_size);

  red_w = read(fid_i, pad_y, fln_w);
  close(fid_i);

  if ( fln_w != red_w ) {
    free(pad_y);
    return u3_cm_bail(c3__fail);
  }
  else {
    u3_noun pad = u3_ci_bytes(fln_w, (c3_y *)pad_y);
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

  rod_u->rut_w = rut_w;
  rod_u->hat_w = hat_w;
 
  rod_u->mat_w = mat_w;
  rod_u->cap_w = cap_w;
  
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

  rod_u->rut_w = rut_w;
  rod_u->hat_w = hat_w;
 
  rod_u->mat_w = mat_w;
  rod_u->cap_w = cap_w;
  
  return rod_u;
}

/* _boot_parts(): build internal tables.
*/
static void
_boot_parts(void)
{
  u3R->cax.har_u = u3_ch_new();
  u3R->jed.har_u = u3_ch_new();
}

/* u3_cm_boot(): instantiate or activate image.
*/
void
u3_cm_boot(c3_o nuu_o)
{
  if ( u3_yes == nuu_o ) {
    u3H = (void *)_boot_north(u3_Loom, c3_wiseof(u3_cs_home), u3_cc_words);
    u3R = &u3H->rod_u;

    _boot_parts();
  } 
  else {
    u3H = (void *)_find_north(u3_Loom, c3_wiseof(u3_cs_home), u3_cc_words);
    u3R = &u3H->rod_u;
  }
}

/* u3_cm_clear(): clear all allocated data in road.
*/
void
u3_cm_clear(void)
{
  u3_ch_free(u3R->jed.har_u);
}

void
u3_cm_dump(void)
{
  c3_w hat_w;
  c3_w fre_w = 0;
  c3_w i_w;

  hat_w = u3_so(u3_co_is_north(u3R)) ? u3R->hat_w - u3R->rut_w 
                                : u3R->rut_w - u3R->hat_w;

  for ( i_w = 0; i_w < u3_cc_fbox_no; i_w++ ) {
    u3_cs_fbox* fre_u = u3R->all.fre_u[i_w];
    
    while ( fre_u ) {
      fre_w += fre_u->box_u.siz_w;
      fre_u = fre_u->nex_u;
    }
  }
  fprintf(stderr, "dump: hat_w %x, fre_w %x, allocated %x\n",
          hat_w, fre_w, (hat_w - fre_w));

  if ( 0 != (hat_w - fre_w) ) {
    c3_w* box_w = u3_so(u3_co_is_north(u3R)) ? u3R->rut_w : u3R->hat_w;
    c3_w  mem_w = 0;

    while ( box_w < (u3_so(u3_co_is_north(u3R)) ? u3R->hat_w : u3R->rut_w) ) {
      u3_cs_box* box_u = (void *)box_w;

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

/* u3_cm_bail(): bail out.  Does not return.
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
u3_cm_bail(u3_noun how)
{
  /* Printf some metadata.
  */
  {
    if ( u3_so(u3ud(how)) ) {
      c3_c str_c[5];

      str_c[0] = ((how >> 0) & 0xff);
      str_c[1] = ((how >> 8) & 0xff);
      str_c[2] = ((how >> 16) & 0xff);
      str_c[3] = ((how >> 24) & 0xff);
      str_c[4] = 0;
      fprintf(stderr, "bail: %s (at %llu)\r\n", str_c, u3N);
    } 
    else {
      c3_assert(u3_so(u3ud(u3h(how))));

      fprintf(stderr, "bail: %d (at %llu)\r\n", u3h(how), u3N);
      u3_cm_p("bail", u3t(how));
    }
  }
  // _cm_punt();
  // u3_lo_sway(2, u3k(u3R->bug.tax));
  abort();

  /* Reconstruct a correct error ball.
  */
  {
    if ( u3_so(u3ud(how)) ) {
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

int c3_cooked() { return u3_cm_bail(c3__oops); }

/* u3_cm_error(): bail out with %exit, ct_pushing error.
*/
c3_i
u3_cm_error(c3_c* str_c)
{
  fprintf(stderr, "error: %s\r\n", str_c);   // rong
  return u3_cm_bail(c3__exit);
}

/* u3_cm_leap(): in u3R, create a new road within the existing one.
*/
void
u3_cm_leap(c3_w pad_w)
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
    if ( (pad_w + c3_wiseof(u3_cs_road)) >= u3_co_open(u3R) ) {
      u3_cm_bail(c3__meme);
    }
    len_w = u3_co_open(u3R) - (pad_w + c3_wiseof(u3_cs_road));
  }

  /* Allocate a region on the cap.
  */
  {
    c3_w* bot_w;

    if ( u3_yes == u3_co_is_north(u3R) ) {
      bot_w = (u3R->cap_w - len_w);
      u3R->cap_w -= len_w;

      rod_u = _boot_south(bot_w, c3_wiseof(u3_cs_road), len_w);
#if 0
      fprintf(stderr, "leap: from north %p (cap %p), to south %p\r\n",
              u3R,
              u3R->cap_w + len_w,
              rod_u); 
#endif
    }
    else {
      bot_w = u3R->cap_w;
      u3R->cap_w += len_w;

      rod_u = _boot_north(bot_w, c3_wiseof(u3_cs_road), len_w);
#if 0
      fprintf(stderr, "leap: from north %p (cap %p), to south %p\r\n",
              u3R,
              u3R->cap_w - len_w,
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
    u3R = rod_u;
    _boot_parts();
  }
}

/* u3_cm_fall(): in u3R, return an inner road to its parent.
*/
void
u3_cm_fall()
{
  c3_assert(0 != u3R->par_u);

#if 0
  fprintf(stderr, "fall: from %s %p, to %s %p (cap %p, was %p)\r\n",
          u3_so(u3_co_is_north(u3R)) ? "north" : "south",
          u3R,
          u3_so(u3_co_is_north(u3R)) ? "north" : "south",
          u3R->par_u,
          u3R->hat_w,
          u3R->rut_w);
#endif

  /* The new cap is the old hat - it's as simple as that.
  */
  u3R->par_u->cap_w = u3R->hat_w;

  /* And, we're back home.
  */
  u3R = u3R->par_u;
  u3R->kid_u = 0;
}

/* u3_cm_golf(): record cap_w length for u3_flog().
*/
c3_w
u3_cm_golf(void)
{
  if ( u3_yes == u3_co_is_north(u3R) ) {
    return u3R->mat_w - u3R->cap_w;
  } 
  else {
    return u3R->cap_w - u3R->mat_w;
  }
}

/* u3_cm_flog(): reset cap_w.
*/
void
u3_cm_flog(c3_w gof_w)
{
  //  Enable memsets in case of memory corruption.
  //
  if ( u3_yes == u3_co_is_north(u3R) ) {
    c3_w* bot_w = (u3R->mat_w - gof_w);
    // c3_w  len_w = (bot_w - u3R->cap_w);

    // memset(u3R->cap_w, 0, 4 * len_w);
    u3R->cap_w = bot_w;
  } 
  else {
    c3_w* bot_w = u3R->mat_w + gof_w;
    // c3_w  len_w = (u3R->cap_w - bot_w);

    // memset(bot_w, 0, 4 * len_w);   //  
    u3R->cap_w = bot_w;
  }
}

/* u3_cm_water(): produce watermarks.
*/
void
u3_cm_water(c3_w* low_w, c3_w* hig_w)
{
  c3_assert(u3R == &u3H->rod_u);

  *low_w = (u3H->rod_u.hat_w - u3H->rod_u.rut_w);
  *hig_w = (u3H->rod_u.mat_w - u3H->rod_u.cap_w) + c3_wiseof(u3_cs_home);
}

/* u3_cm_soft_top(): top-level safety wrapper.
*/
u3_noun 
u3_cm_soft_top(c3_w    sec_w,                     //  timer seconds
               c3_w    pad_w,                     //  base memory pad
               u3_funk fun_f,
               u3_noun arg)
{
  u3_noun why, pro;
  c3_w    gof_w;
 
  /* Enter internal signal regime.
  */
  _cm_signal_deep(0);

  if ( 0 != sigsetjmp(Signal_u, 1) ) {
    //  return to blank state
    //
    _cm_signal_done();

    return _cm_signal_recover();
  }

  /* Record the cap, and leap.
  */
  {
    gof_w = u3_cm_golf();
    u3_cm_leap(pad_w);
  }

  /* Trap for ordinary nock exceptions.
  */
  if ( 0 == (why = u3_cm_trap()) ) {
    pro = fun_f(arg);

    /* Revert to external signal regime.
    */
    _cm_signal_done();

    /* Fall back to the old road, leaving temporary memory intact.
    */
    u3_cm_fall();

    /* Produce success, on the old road.
    */
    pro = u3nc(0, u3_ca_take(pro));
  }
  else {
    /* Fall back to the old road, leaving temporary memory intact.
    */
    u3_cm_fall();

    /* Overload the error result.
    */
    pro = u3_ca_take(why);
  }
  
  /* Clean up temporary memory.
  */
  u3_cm_flog(gof_w);

  /* Revert to external signal regime.
  */
  _cm_signal_done();

  /* Return the product.
  */
  return pro;
}

/* u3_cm_soft_run(): descend into virtualization context.
*/
u3_noun 
u3_cm_soft_run(u3_noun fly,
               u3_funq fun_f,
               u3_noun aga,
               u3_noun agb)
{
  u3_noun why, pro;
  c3_w    gof_w;

  u3_cm_wash(aga);
  u3_cm_wash(agb);

  /* Record the cap, and leap.
  */
  {
    gof_w = u3_cm_golf();
    u3_cm_leap(32768);
  }
 
  /* Configure the new road.
  */
  {
    u3R->ski.flu = u3nc(fly, u3R->par_u->ski.flu);
    u3R->pro.don = u3R->par_u->pro.don;
    u3R->bug.tax = 0;
  }

  /* Trap for exceptions.
  */
  if ( 0 == (why = u3_cm_trap()) ) {
    pro = fun_f(aga, agb);

    /* Fall back to the old road, leaving temporary memory intact.
    */
    u3_cm_fall();

    /* Produce success, on the old road.
    */
    pro = u3nc(0, u3_ca_take(pro));
  }
  else {
    /* Fall back to the old road, leaving temporary memory intact.
    */
    u3_cm_fall();

    /* Produce - or fall again.
    */
    {
      c3_assert(u3_so(u3du(why)));
      switch ( u3h(why) ) {
        default: c3_assert(0); return 0;

        case 0: {                             //  unusual: bail with success.
          pro = u3_ca_take(why);
        } break;

        case 1: {                             //  blocking request
          pro = u3_ca_take(why);
        } break;

        case 2: {                             //  true exit
          pro = u3_ca_take(why);
        } break;

        case 3: {                             //  failure; rebail w/trace
          u3_cm_bail
            (u3nt(3, 
                  u3_ca_take(u3h(u3t(why))),
                  u3_ckb_weld(u3_ca_take(u3t(u3t(why))),
                              u3k(u3R->bug.tax))));
        } break;

        case 4: {                             //  meta-bail
          u3_cm_bail(u3_ca_take(u3t(why)));
        } break;
      }
    }
  }

  /* Clean up temporary memory.
  */
  u3_cm_flog(gof_w);

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

/* u3_cm_soft_esc(): namespace lookup.  Produces direct result.
*/
u3_noun
u3_cm_soft_esc(u3_noun sam)
{
  u3_noun why, fly, pro;
  c3_w    gof_w;
 
  /* Assert preconditions. 
  */
  {
    c3_assert(0 != u3R->ski.flu);
    fly = u3h(u3R->ski.flu);
  }

  /* Record the cap, and leap.
  */
  {
    gof_w = u3_cm_golf();
    u3_cm_leap(32768);
  }
 
  /* Configure the new road.
  */
  {
    u3R->ski.flu = u3t(u3R->par_u->ski.flu);
    u3R->pro.don = u3R->par_u->pro.don;
    u3R->bug.tax = 0;
  }

  /* Trap for exceptions.
  */
  if ( 0 == (why = u3_cm_trap()) ) {
    pro = u3_cn_slam_on(fly, sam);

    /* Fall back to the old road, leaving temporary memory intact.
    */
    u3_cm_fall();

    /* Produce success, on the old road.
    */
    pro = u3_ca_take(pro);
  }
  else {
    /* Fall back to the old road, leaving temporary memory intact.
    */
    u3_cm_fall();

    /* Push the error back up to the calling context - not the run we
    ** are in, but the caller of the run, matching pure nock semantics.
    */
    return u3_cm_bail(u3nc(4, u3_ca_take(why)));
  }

  /* Clean up temporary memory.
  */
  u3_cm_flog(gof_w);

  /* Release the sample.
  */
  u3z(sam);

  /* Return the product.
  */
  return pro;
}

/* u3_cm_soft(): wrapper for old calls.
*/
u3_noun 
u3_cm_soft(c3_w    sec_w,
           u3_funk fun_f,
           u3_noun arg)
{
  u3_noun why = u3_cm_soft_top(sec_w, (1 << 17), fun_f, arg);   // 512K pad
  u3_noun pro;

  switch ( u3h(why) ) {
    default: c3_assert(0); break;
    case 0: pro = why; break;
    case 2: pro = u3nc(c3__exit, u3k(u3t(why))); u3z(why); break;
    case 3: pro = u3k(u3t(why)); u3z(why); break;
  }
  return pro;
}

/* _cm_is_tas(): yes iff som (RETAIN) is @tas.
*/
static c3_o
_cm_is_tas(u3_atom som, c3_w len_w)
{
  c3_w i_w;

  for ( i_w = 0; i_w < len_w; i_w++ ) {
    c3_c c_c = u3_cr_byte(i_w, som);

    if ( islower(c_c) || 
        (isdigit(c_c) && (0 != i_w) && ((len_w - 1) != i_w))
        || '-' == c_c )
    {
      continue;
    }
    return u3_no;
  }
  return u3_yes;
}

/* _cm_is_ta(): yes iff som (RETAIN) is @ta.
*/
static c3_o
_cm_is_ta(u3_noun som, c3_w len_w)
{
  c3_w i_w;

  for ( i_w = 0; i_w < len_w; i_w++ ) {
    c3_c c_c = u3_cr_byte(i_w, som);

    if ( (c_c < 32) || (c_c > 127) ) {
      return u3_no;
    }
  }
  return u3_yes;
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
  if ( u3_so(u3du(som)) ) {
    c3_w sel_w, one_w, two_w;

    sel_w = 0;
    if ( u3_so(sel_o) ) {
      if ( str_c ) { *(str_c++) = '['; }
      sel_w += 1;
    }

    one_w = _cm_in_pretty(u3h(som), u3_yes, str_c);
    if ( str_c ) {
      str_c += one_w;
      *(str_c++) = ' ';
    }
    two_w = _cm_in_pretty(u3t(som), u3_no, str_c);
    if ( str_c ) { str_c += two_w; }

    if ( u3_so(sel_o) ) {
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
      c3_w len_w = u3_cr_met(3, som);

      if ( u3_so(_cm_is_tas(som, len_w)) ) {
        c3_w len_w = u3_cr_met(3, som);

        if ( str_c ) {
          *(str_c++) = '%'; 
          u3_cr_bytes(0, len_w, (c3_y *)str_c, som);
          str_c += len_w;
        }
        return len_w + 1;
      }
      else if ( u3_so(_cm_is_ta(som, len_w)) ) {
        if ( str_c ) {
          *(str_c++) = '\''; 
          u3_cr_bytes(0, len_w, (c3_y *)str_c, som);
          str_c += len_w;
          *(str_c++) = '\''; 
        }
        return len_w + 2;
      }
      else {
        c3_w len_w = u3_cr_met(3, som);
        c3_c *buf_c = malloc(2 + (2 * len_w) + 1);
        c3_w i_w = 0;
        c3_w a_w = 0;

        buf_c[a_w++] = '0';
        buf_c[a_w++] = 'x';
       
        for ( i_w = 0; i_w < len_w; i_w++ ) {
          c3_y c_y = u3_cr_byte(len_w - (i_w + 1), som);

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

/* u3_cm_pretty(): dumb prettyprint to string.
*/
c3_c* 
u3_cm_pretty(u3_noun som)
{
  c3_w len_w = _cm_in_pretty(som, u3_yes, 0);
  c3_c* pre_c = malloc(len_w + 1);

  _cm_in_pretty(som, u3_yes, pre_c);
  pre_c[len_w] = 0;
  return pre_c;
}

/* u3_cm_p(): dumb print with caption.
*/
void
u3_cm_p(const c3_c* cap_c, u3_noun som)
{
  c3_c* pre_c = u3_cm_pretty(som);

  fprintf(stderr, "%s: %s\r\n", cap_c, pre_c);
  free(pre_c);
}

/* u3_cm_tape(): dump a tape to stdout.
*/
void
u3_cm_tape(u3_noun tep)
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

/* u3_cm_wall(): dump a wall to stdout.
*/
void
u3_cm_wall(u3_noun wol)
{
  u3_noun wal = wol;

  while ( u3_nul != wal ) {
    u3_cm_tape(u3k(u3h(wal)));

    putc(13, stdout);
    putc(10, stdout);

    wal = u3t(wal);
  }
  u3z(wol);
}
