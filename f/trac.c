/* f/trac.c
**
** This file is in the public domain.
*/
#include "all.h"
#include <sys/time.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <ev.h>
#include <sigsegv.h>
#include <curses.h>
#include <termios.h>
#include <term.h>

#include "v/vere.h"

  /**  Jet dependencies.  Minimize these.
  **/
#   define Pt3Y   k_192__a__b__c
#   define Pt4Y   k_192__a__b__c__d

    u2_noun
    j2_mbc(Pt3Y, gor)(u2_wire, u2_noun a, u2_noun b);

    u2_noun
    j2_mcc(Pt4Y, by, put)(u2_wire, u2_noun a, u2_noun b, u2_noun c);

#   define _tx_gor  j2_mbc(Pt3Y, gor)
#   define _tx_put  j2_mcc(Pt4Y, by, put)


  /** Static sampling data structures, for signal handling use.
  **/
#   define U2_TRAC_SAMPLE_MAX 10000
    
    u2_loom_knot  _tx_knots[U2_TRAC_SAMPLE_MAX];
    u2_loom_knot* _tx_top_k;
    u2_ray        _tx_rac_r;
    c3_w          _tx_knot_cur;
    c3_t          _tx_on;

    static u2_loom_knot*
    _tx_knot_new(void)
    {
      if ( _tx_knot_cur == U2_TRAC_SAMPLE_MAX ) {
        return 0;
      } else {
        u2_loom_knot *new_k = &_tx_knots[_tx_knot_cur];

        _tx_knot_cur++;
        return new_k;
      }
    }

    static u2_loom_knot*
    _tx_sample_in(u2_noun don)
    {
      if ( u2_nul == don ) {
        _tx_top_k->fin_w += 1;
        return _tx_top_k;
      }
      else {
        u2_noun       hed   = u2_h(don);
        c3_c          hed_c[32];
        u2_loom_knot* par_k = _tx_sample_in(u2_t(don));
        u2_loom_knot* dis_k;

        if ( (0 == par_k) && (u2_no == u2_stud(hed)) ) {
          return 0;
        }
        u2_bytes(0, 32, (c3_y *)hed_c, hed);
        hed_c[31] = 0;

        for ( dis_k = par_k->fam_k; dis_k; dis_k = dis_k->nex_k ) {
          //  linear search, should be fine in normal cases
          //
          if ( !strcmp(hed_c, dis_k->lic_c) ) {
            dis_k->fin_w += 1;
            return dis_k;
          }
        }
        if ( 0 == (dis_k = _tx_knot_new()) ) {
          return 0;
        } else {
          strcpy(dis_k->lic_c, hed_c);
          dis_k->fin_w = 1;
          dis_k->fam_k = 0;
          dis_k->nex_k = par_k->fam_k;
          par_k->fam_k = dis_k;

          return dis_k;
        }
      }
    }
    static void
    _tx_sample(c3_i x)
    {
      u2_ray rac_r = _tx_rac_r;

      c3_assert(_tx_on == 1);
      // printf("sample sys %d\n", u2_trac_at(rac_r, wer.sys));

      if ( u2_yes == u2_trac_at(rac_r, wer.sys) ) {
        if ( u2_yes == u2_trac_at(rac_r, wer.glu) ) {
          u2_trac_be(rac_r, c3_d, wer.com_d) += 1;
        } else {
          u2_trac_be(rac_r, c3_d, wer.jet_d) += 1;
        }
      } else {
        u2_trac_be(rac_r, c3_d, wer.erp_d) += 1;
      }

      _tx_sample_in(u2_trac_at(rac_r, duz.don));
    }

/* u2_tx_samp_on(): turn profile sampling on, clear count.
*/
static void
_tx_samp_on(u2_ray rac_r)
{
  c3_assert(_tx_on == 0);
  c3_assert(_tx_knot_cur == 0);
 
  _tx_on = 1;
  _tx_rac_r = rac_r;

  _tx_top_k = _tx_knot_new();
  _tx_top_k->lic_c[0] = 0;
  _tx_top_k->fin_w = 0;
  _tx_top_k->fam_k = _tx_top_k->nex_k = 0;

  {
    struct itimerval itm_v;
    struct sigaction sig_s;
#if defined(U2_OS_osx)
    sig_s.__sigaction_u.__sa_handler = _tx_sample;
    sig_s.sa_mask = 0;
    sig_s.sa_flags = 0;
#elif defined(U2_OS_linux)
    // TODO: support profiling on linux
#endif
    sigaction(SIGPROF, &sig_s, 0);

    itm_v.it_interval.tv_sec = 0;
    itm_v.it_interval.tv_usec = 10000;
    itm_v.it_value = itm_v.it_interval;

    setitimer(ITIMER_PROF, &itm_v, 0);
  }
}

/* _tx_samp_off(): turn profile sampling off.
*/
static void
_tx_samp_off(u2_ray rac_r)
{
  c3_assert(_tx_on == 1);
  struct sigaction sig_s;
  struct itimerval itm_v;

  _tx_on = 0;
  _tx_knot_cur = 0;

  itm_v.it_interval.tv_sec = 0;
  itm_v.it_interval.tv_usec = 0;
  itm_v.it_value = itm_v.it_interval;

  setitimer(ITIMER_PROF, &itm_v, 0);

#if defined(U2_OS_osx)
  sig_s.__sigaction_u.__sa_handler = SIG_DFL;
  sig_s.sa_mask = 0;
  sig_s.sa_flags = 0;
#elif defined(U2_OS_linux)
  // TODO: support profiling on linux
#endif

  sigaction(SIGPROF, &sig_s, 0);
}

/* _tx_samples_in(): sample list.
*/
static u2_weak
_tx_samples_in(u2_wire wir_r, u2_loom_knot *fam_k)
{
  if ( 0 == fam_k ) {
    return u2_nul;
  } else {
    return u2_rc
      (wir_r, u2_rt(wir_r, u2_rl_string(wir_r, fam_k->lic_c),
                           u2_rl_words(wir_r, 1, &fam_k->fin_w),
                           _tx_samples_in(wir_r, fam_k->fam_k)),
              _tx_samples_in(wir_r, fam_k->nex_k));
  }
}

/* _tx_samples(): dump samples.
*/
static u2_weak                                                    //  produce
_tx_samples(u2_wire wir_r)
{
  u2_loom_knot *not_k = _tx_top_k;

  return u2_rc(wir_r, u2_rl_words(wir_r, 1, &not_k->fin_w),
                         _tx_samples_in(wir_r, not_k->fam_k));
}

/* _tx_d(): noun from c3_d.
*/
static u2_weak
_tx_d(u2_wire wir_r, c3_d dat_d)
{
  c3_w dat_w[2];

  dat_w[0] = (dat_d & 0xffffffff);
  dat_w[1] = (dat_d >> 32ULL);

  return u2_rl_words(wir_r, 2, dat_w);
}

/* _tx_event(): add system counter to user event list.
*/
static u2_noun                                                    //  produce
_tx_event(u2_wire wir_r,
          c3_c*   str_c,
          c3_d    val_d,
          u2_noun cot)                                            //  submit
{
  u2_noun vez, val, str;

  if ( 0 == val_d ) {
    return cot;
  }
  if ( u2_none == (str = u2_rl_string(wir_r, str_c)) ) {
    return cot;
  }
  if ( u2_none == (val = _tx_d(wir_r, val_d)) ) {
    u2_rz(wir_r, str); return cot;
  }
  if ( u2_none == (vez = _tx_put(wir_r, cot, str, val)) ) {
    u2_rz(wir_r, str); u2_rz(wir_r, val); return cot;
  }
  u2_rz(wir_r, cot);
  return vez;
}

/* u2_tx_events(): produce event list, including counts.
*/
static u2_noun                                                    //  produce
_tx_events(u2_wire wir_r, 
           u2_noun cot)                                           //  retain
{
  u2_ray rac_r = u2_wire_rac_r(wir_r);

  cot = u2_rx(wir_r, cot);

  cot = _tx_event(wir_r, "sys-hops", u2_trac_be(rac_r, c3_d, sys.hop_d), cot);
  cot = _tx_event(wir_r, "sys-jets", u2_trac_be(rac_r, c3_d, sys.jet_d), cot);
  cot = _tx_event(wir_r, "sys-tests", u2_trac_be(rac_r, c3_d, sys.tes_d), cot);
  cot = _tx_event(wir_r, "sys-nods", u2_trac_be(rac_r, c3_d, sys.nod_d), cot);

  cot = _tx_event(wir_r, "sys-cache-finds", 
                         u2_trac_be(rac_r, c3_d, sys.fin_d), cot);
  cot = _tx_event(wir_r, "sys-cache-saves", 
                          u2_trac_be(rac_r, c3_d, sys.pod_d), cot);

  cot = _tx_event(wir_r, "sys-stack", u2_trac_at(rac_r, sys.cas_x.max_w), cot);

#if 0
  cot = _tx_event(wir_r, "sys-memory-used", 
        u2_trac_be(rac_r, c3_w, sys.men_x.max_w), cot);
  cot = _tx_event(wir_r, "sys-memory-held",
        u2_trac_be(rac_r, c3_w, sys.men_x.med_w), cot);
  cot = _tx_event(wir_r, "sys-basket", 
        u2_trac_be(rac_r, c3_w, sys.bek_x.max_w), cot);
  cot = _tx_event(wir_r, "sys-memory-active", 
                        4 * (u2_soup_liv_w(u2_rail_rut_r(wir_r)) - 
                             u2_trac_at(rac_r, sys.lif_w)),
                        cot);

  cot = _tx_event(wir_r, "sys-memory-basket", 
                        4 * 
                        (u2_soup_liv_w(u2_rail_rut_r(u2_wire_bas_r(wir_r))) - 
                         u2_trac_at(rac_r, sys.bos_w)),
                        cot);
#endif

#if 1
  // These numbers are bogus for some bizarre reason - non-random samples???
  //
  {
    c3_d com_d = u2_trac_be(rac_r, c3_d, wer.com_d);
    c3_d jet_d = u2_trac_be(rac_r, c3_d, wer.jet_d);
    c3_d erp_d = u2_trac_be(rac_r, c3_d, wer.erp_d);

    // printf("com_d %llu, jet_d %llu, erp_d %llu\n", com_d, jet_d, erp_d);
    if ( com_d + erp_d + jet_d ) {
      c3_d sof_d = (erp_d * 100ULL) / (com_d + erp_d + jet_d);
      c3_d fun_d = (jet_d * 100ULL) / (com_d + erp_d + jet_d);

      cot = _tx_event(wir_r, "sys-softpercent", sof_d, cot);
      cot = _tx_event(wir_r, "sys-jetpercent", fun_d, cot);
    }
  }
#endif

  /* sys-time
  */
  {
    struct timeval tv;
    c3_w sec_w = u2_trac_at(rac_r, sys.sec_w);
    c3_w usc_w = u2_trac_at(rac_r, sys.usc_w);
    c3_w ums_w;
    c3_d old_d, new_d;

    old_d = sec_w;
    old_d *= 1000000ULL;
    old_d += usc_w;

    gettimeofday(&tv, 0);
    new_d = tv.tv_sec;
    new_d *= 1000000ULL;
    new_d += tv.tv_usec;

    ums_w = (c3_w) (((new_d - old_d) + 999ULL) / 1000ULL);
    cot = _tx_event(wir_r, "sys-msec", ums_w, cot);
  }
  return cot;
}

/* u2_tx_sys_bit(): set system bit, returning old value.
*/
u2_bean
u2_tx_sys_bit(u2_ray wir_r, u2_bean val)
{
  u2_bean bit = u2_wrac_at(wir_r, wer.sys);

  u2_wrac_at(wir_r, wer.sys) = val;
  return bit;
}
 
/* u2_tx_glu_bit(): set glutem bit, returning old value.
*/
u2_bean
u2_tx_glu_bit(u2_ray wir_r, u2_bean val)
{
  u2_bean bit = u2_wrac_at(wir_r, wer.glu);

  u2_wrac_at(wir_r, wer.glu) = val;
  return bit;
}
 
/* u2_tx_init(): initialize state.
*/
u2_ray
u2_tx_init(u2_wire wir_r)
{
  u2_ray rac_r = u2_rl_ralloc(wir_r, c3_wiseof(u2_loom_trac));

  u2_trac_at(rac_r, cor.deb) = u2_no;
  u2_trac_at(rac_r, cor.pro) = u2_no;

  return rac_r;
}

/* u2_tx_open(): open/clear trace state.
*/
void
u2_tx_open(u2_wire wir_r)
{
  u2_ray rac_r = u2_wire_rac_r(wir_r);

  u2_trac_at(rac_r, wer.ryp) = u2_nul;
  u2_trac_at(rac_r, wer.sys) = u2_yes;
  u2_trac_at(rac_r, wer.glu) = u2_yes;
  u2_trac_be(rac_r, c3_d, wer.erp_d) = 0;
  u2_trac_be(rac_r, c3_d, wer.com_d) = 0;
  u2_trac_be(rac_r, c3_d, wer.jet_d) = 0;

  u2_trac_at(rac_r, duz.don) = u2_nul;
  u2_trac_at(rac_r, duz.cot) = u2_nul;

  u2_trac_be(rac_r, c3_d, sys.hop_d) = 0;
  u2_trac_be(rac_r, c3_d, sys.jet_d) = 0;
  u2_trac_be(rac_r, c3_d, sys.tes_d) = 0;
  u2_trac_be(rac_r, c3_d, sys.nod_d) = 0;
  u2_trac_be(rac_r, c3_d, sys.fin_d) = 0;
  u2_trac_be(rac_r, c3_d, sys.pod_d) = 0;

  u2_trac_at(rac_r, sys.cas_x.med_w) = 
  u2_trac_at(rac_r, sys.cas_x.max_w) = 0;

  u2_trac_at(rac_r, sys.men_x.med_w) = 
  u2_trac_at(rac_r, sys.men_x.max_w) = 0;

  u2_trac_at(rac_r, sys.bek_x.med_w) = 
  u2_trac_at(rac_r, sys.bek_x.max_w) = 0;

  u2_trac_at(rac_r, sys.lif_w) = u2_soup_liv_w(u2_rail_rut_r(wir_r));
  u2_trac_at(rac_r, sys.bos_w) = 
    u2_soup_liv_w(u2_rail_rut_r(u2_wire_bas_r(wir_r)));

  {
    struct timeval tv;

    gettimeofday(&tv, 0);
    u2_trac_at(rac_r, sys.sec_w) = tv.tv_sec;
    u2_trac_at(rac_r, sys.usc_w) = tv.tv_usec;
  }

  if ( u2_yes == u2_trac_at(rac_r, cor.pro) ) {
    _tx_samp_on(rac_r);
  }
}

/* u2_tx_done(): produce a profile slab to render.  Free internal state.
*/ 
u2_noun                                                           //  produce
u2_tx_done(u2_wire wir_r)
{
  u2_ray rac_r = u2_wire_rac_r(wir_r);
  u2_noun p_sab = u2_nul, q_sab = u2_nul, r_sab = u2_nul;

  if ( u2_yes == u2_trac_at(rac_r, cor.deb) ) {
    p_sab = u2_rx(wir_r, u2_trac_at(rac_r, wer.ryp));
  }
  if ( u2_yes == u2_trac_at(rac_r, cor.pro) ) {
    _tx_samp_off(rac_r);

    q_sab = _tx_events(wir_r, u2_trac_at(rac_r, duz.cot));
    r_sab = _tx_samples(wir_r);

    if ( u2_none == q_sab ) q_sab = u2_nul;
    if ( u2_none == r_sab ) r_sab = u2_nul;
  }
  u2_rz(wir_r, u2_trac_at(rac_r, wer.ryp));
  u2_rz(wir_r, u2_trac_at(rac_r, duz.don));
  u2_rz(wir_r, u2_trac_at(rac_r, duz.cot));

  return u2_bt(wir_r, p_sab, q_sab, r_sab);
}

/* u2_tx_do_debug(): set debug bean.  Return old value.
*/
u2_bean 
u2_tx_do_debug(u2_ray wir_r, u2_bean lag)
{
  u2_ray  rac_r = u2_wire_rac_r(wir_r);
  u2_bean old   = u2_trac_at(rac_r, cor.deb);

  u2_trac_at(rac_r, cor.deb) = lag;
  return old;
}

/* u2_tx_in_debug(): get debug bean.
*/
u2_bean 
u2_tx_in_debug(u2_ray wir_r)
{
  u2_ray rac_r = u2_wire_rac_r(wir_r);

  return u2_trac_at(rac_r, cor.deb);
}

/* u2_tx_do_profile(): set profile bean.  Return old value.
*/
u2_bean 
u2_tx_do_profile(u2_ray wir_r, u2_bean lag)
{
  u2_ray  rac_r = u2_wire_rac_r(wir_r);
  u2_bean old   = u2_trac_at(rac_r, cor.pro);

  u2_trac_at(rac_r, cor.pro) = lag;
  return old;
}

/* u2_tx_in_profile(): get profile bean.
*/
u2_bean 
u2_tx_in_profile(u2_ray wir_r)
{
  u2_ray rac_r = u2_wire_rac_r(wir_r);

  return u2_trac_at(rac_r, cor.pro);
}

static u2_bean
_tx_map_ok(u2_wire wir_r, 
           u2_noun a)
{
  if ( u2_nul == a ) {
    return u2_yes;
  } else {
    u2_noun l_a, n_a, r_a, lr_a;
    u2_noun pn_a, qn_a;

    u2_as_cell(a, &n_a, &lr_a);
    u2_as_cell(lr_a, &l_a, &r_a);
    u2_as_cell(n_a, &pn_a, &qn_a);

    c3_assert(l_a != a);
    c3_assert(r_a != a);

    _tx_map_ok(wir_r, l_a);
    _tx_map_ok(wir_r, r_a);
    return u2_yes;
  }
}


static u2_noun
_tx_increment_soft(u2_wire wir_r,
                   u2_noun a,                                     //  transfer
                   u2_noun b)                                     //  retain
{
  _tx_map_ok(wir_r, a);

  if ( u2_nul == a ) {
    u2_noun nuu = u2_rt(wir_r, u2_rc(wir_r, u2_rx(wir_r, b), _1),
                               u2_nul,
                               u2_nul);
    if ( u2_none == nuu ) {
      return u2_nul;
    }
    else return nuu;
  } else {
    u2_noun l_a, n_a, r_a, lr_a;
    u2_noun pn_a, qn_a;

    u2_as_cell(a, &n_a, &lr_a);
    u2_as_cell(lr_a, &l_a, &r_a);
    u2_as_cell(n_a, &pn_a, &qn_a);

    if ( (u2_yes == u2_sing(b, pn_a)) ) {
      if ( u2_fly_is_cat(qn_a) && u2_fly_is_cat(qn_a + 1) ) {
        *u2_at_pom_tel(n_a) = (qn_a + 1);
      } else {
        u2_noun nyx = u2_rl_vint(wir_r, qn_a);

        c3_assert(!"heavy increment");
        u2_rz(wir_r, qn_a);
        *u2_at_pom_tel(n_a) = nyx;
      }
    }
    else {
      if ( u2_yes == _tx_gor(wir_r, b, pn_a) ) {
        *u2_at_pom_hed(lr_a) = _tx_increment_soft(wir_r, l_a, b);
      } else {
        *u2_at_pom_tel(lr_a) = _tx_increment_soft(wir_r, r_a, b);
      }
    }
    return a;
  }
}

/* u2_tx_did_act(): record user actions.
*/
void 
u2_tx_did_act(u2_wire wir_r, 
              u2_noun did)                                        //  retain
{
  u2_ray  rac_r = u2_wire_rac_r(wir_r);

  if ( u2_yes == u2_trac_at(rac_r, cor.pro) ) {
    u2_noun cot   = u2_trac_at(rac_r, duz.cot);

    u2_trac_at(rac_r, duz.cot) = _tx_increment_soft(wir_r, cot, did);
    _tx_map_ok(wir_r, u2_trac_at(rac_r, duz.cot));
  }
}

/* u2_tx_task_in(): enter a task for profiling purposes.
**
** u2_yes iff the task is not already in the stack.
*/
u2_bean
u2_tx_task_in(u2_wire wir_r, 
              u2_noun tak)                                        //  retain
{
  //  Temporarily disabled due to bail issues.
  //
#if 0
  u2_ray rac_r = u2_wire_rac_r(wir_r);
  u2_noun don = u2_trac_at(rac_r, duz.don);
  u2_noun dim;

  /* Test if we're already doing this.
  */
  {
    dim = don;

    while ( dim != u2_nul ) {
      if ( u2_yes == u2_sing(tak, u2_h(dim)) ) {
        return u2_no;
      }
      dim = u2_t(dim);
    }
  }

  dim = u2_rc(wir_r, u2_rx(wir_r, tak), u2_rx(wir_r, don));
  if ( u2_none == dim ) {
    return u2_no;
  }
  else {
    u2_rz(wir_r, don);
    u2_trac_at(rac_r, duz.don) = dim;

    return u2_yes;
  }
#endif
  return u2_no;
}

/* u2_tx_task_out(): leave a task for profiling purposes.
*/
void
u2_tx_task_out(u2_wire wir_r)
{
  //  Temporarily disabled due to bail issues.
  //
#if 0
  u2_ray  rac_r = u2_wire_rac_r(wir_r);
  u2_noun don = u2_trac_at(rac_r, duz.don);
  u2_noun dim;

  c3_assert((u2_nul != don) && (u2_yes == u2_dust(don)));
  dim = u2_t(don);
  u2_rx(wir_r, dim);
  u2_rz(wir_r, don);
  u2_trac_at(rac_r, duz.don) = dim;
#endif
}

/* _print_tape(): print a byte tape.
*/
static void
_print_tape(u2_noun som,
            FILE*   fil_F)
{
  u2_noun h_som;

  while ( (u2_yes == u2_dust(som)) && ((h_som = u2_h(som)) < 128) ) {
    putc(h_som, fil_F);
    som = u2_t(som);
  }
}

#if 0
/* _print_term(): print a terminal.
*/
static void
_print_term(u2_noun som,
            FILE*   fil_F)
{
  if ( u2_yes == u2_stud(som) ) {
    c3_w len_w = u2_met(3, som);
    c3_y *som_y = alloca(len_w) + 1;

    u2_bytes(0, len_w, som_y, som);
    som_y[len_w] = 0;
    fprintf(fil_F, "%s", (c3_c *)som_y);
  }
}

/* _print_space(): print `feq_w` spaces.
*/
static void
_print_space(c3_w  feq_w,
             FILE* fil_F)
{
  while ( feq_w-- ) {
    putc(' ', fil_F);
  }
}
#endif

/* _print_wall(): print debug wall.
*/
static void
_print_wall(u2_noun wal,
            FILE* fil_F)
{
  while ( u2_yes == u2_dust(wal) ) {
    _print_tape(u2_h(wal), fil_F);
    putc('\r', fil_F);
    putc('\n', fil_F);
    wal = u2_t(wal);
  }
}
            
/* u2_tx_slog(): print debug syslog [0-3 tank] 0=debug 3=alarm
*/
void
u2_tx_slog(u2_ray  wir_r,
           u2_noun luf)                                           //  retain
{
  {
    if ( u2_yes == u2du(luf) ) {
      u2_noun pri = u2h(luf);

      switch ( pri ) {
        case 3: printf(">>> "); break;
        case 2: printf(">> "); break;
        case 1: printf("> "); break;
      }
      u2_ve_tank(0, u2k(u2t(luf)));
    }
  }
}

/* u2_tx_warn(): report a warning by internal file and line.
*/
void
u2_tx_warn(u2_ray      wir_r,
           const c3_c* fil_c,
           c3_w        lyn_w)
{
  fprintf(stderr, "nock: warn: %s, %d\n", fil_c, lyn_w);
}
