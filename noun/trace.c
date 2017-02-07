/* g/t.c  -
**
** This file is in the public domain.
*/
#include "all.h"
#include <pthread.h>

/* u3t_push(): push on trace stack.
*/
void
u3t_push(u3_noun mon)
{
  u3R->bug.tax = u3nc(mon, u3R->bug.tax);
}

/* u3t_mean(): push `[%mean roc]` on trace stack.
*/
void
u3t_mean(u3_noun roc)
{
  u3R->bug.tax = u3nc(u3nc(c3__mean, roc), u3R->bug.tax);
}

/* u3t_drop(): drop from meaning stack.
*/
void
u3t_drop(void)
{
  c3_assert(_(u3du(u3R->bug.tax)));
  {
    u3_noun tax = u3R->bug.tax;

    u3R->bug.tax = u3k(u3t(tax));
    u3z(tax);
  }
}

extern void
u3_pier_tank(c3_l tab_l, u3_noun tac);

#ifdef GHETTO
/* _t_ghetto(): ghetto timelapse.
*/
void
_t_ghetto(void)
{
  static int old;
  static struct timeval b4, f2, d0;
  static c3_d b4_d;
  c3_w ms_w;
          
  if ( old ) {
    gettimeofday(&f2, 0); 
    timersub(&f2, &b4, &d0);
    ms_w = (d0.tv_sec * 1000) + (d0.tv_usec / 1000);
    if (ms_w > 1) {
#if 0
      fprintf(stderr, "%6d.%02dms: %9d ", 
              ms_w, (int) (d0.tv_usec % 1000) / 10,
              ((int) (u3R->pro.nox_d - b4_d)));
#else
      fprintf(stderr, "%6d.%02dms ", 
              ms_w, (int) (d0.tv_usec % 1000) / 10);
#endif
      gettimeofday(&b4, 0);
      b4_d = u3R->pro.nox_d;
    }
    else {
      printf("            ");
    }
  }
  else {
    gettimeofday(&b4, 0);
    b4_d = u3R->pro.nox_d;
  }
  old = 1;
}
#endif

/* u3t_slog(): print directly.
*/
void
u3t_slog(u3_noun hod)
{
#ifdef GHETTO
  _t_ghetto();
#endif

  if ( c3y == u3du(hod) ) {
    u3_noun pri = u3h(hod);

    switch ( pri ) {
      case 3: fprintf(stderr, ">>> "); break;
      case 2: fprintf(stderr, ">> "); break;
      case 1: fprintf(stderr, "> "); break;
    }
    u3_pier_tank(0, u3k(u3t(hod)));
  }
  u3z(hod);
}

/* u3t_shiv(): quick print.
*/
void 
u3t_shiv(u3_noun hod)
{
#ifdef GHETTO
  _t_ghetto();
#endif

  if ( c3n == u3ud(hod) ) {
  }
  else {
    c3_c *str_c = u3r_string(hod);
    fprintf(stderr, "%s\r\n", str_c);
    free(str_c);
  }
}

/* u3t_heck(): profile point.
*/
void
u3t_heck(u3_atom cog)
{
  //  Profile sampling, because it allocates on the home road,
  //  only works on when we're not at home.
  //
  if ( &(u3H->rod_u) != u3R ) {
    u3a_road* rod_u;
 
    rod_u = u3R;
    u3R = &(u3H->rod_u);
    {
      if ( 0 == u3R->pro.day ) { 
        u3R->pro.day = u3v_do("doss", 0);
      }
      u3R->pro.day = u3dc("pi-heck", u3a_take(cog), u3R->pro.day);
    }
    u3R = rod_u;
  }
}

/* _t_jet_label():
*/
u3_weak
_t_jet_label(u3a_road* rod_u, u3_noun bat)
{
  while ( 1 ) {
    u3_weak cax = u3h_git(rod_u->jed.har_p, bat);

    if ( u3_none != cax ) {
      return u3h(u3t(u3t(u3h(cax))));
    }

    if ( rod_u->par_p ) {
      rod_u = u3to(u3_road, rod_u->par_p);
    }
    else return u3_none;
  }
}

#if 1
/* _t_samp_process(): process raw sample data from live road.
*/
static u3_noun
_t_samp_process(u3_road* rod_u)
{
  u3_noun pef   = u3_nul;           // (list (pair path (map path ,@ud)))
  u3_noun muf   = u3_nul;           // (map path ,@ud)
  c3_w    len_w = 0;

  //  Accumulate a label/map stack which collapses recursive segments.
  //
  while ( rod_u ) {
    u3_noun don = rod_u->pro.don;

    while ( u3_nul != don ) {
      u3_noun bat = u3h(don);
      u3_noun lab;

      //  Find the label from this battery, surface allocated.
      //
      {
        u3_noun laj = _t_jet_label(rod_u, bat);
        if ( u3_none == laj ) { abort(); }

        // lab = u3nc(u3i_string("foobar"), u3_nul);
        lab = u3a_take(laj); u3a_wash(laj);
      }
      //  Add the label to the traced label stack, trimming recursion.
      //  
      {
        u3_noun old;

        if ( u3_none == (old = u3kdb_get(u3k(muf), u3k(lab))) ) {
          muf = u3kdb_put(muf, u3k(lab), len_w);
          pef = u3nc(u3nc(lab, u3k(muf)), pef);
          len_w += 1;
        }
        else {
          u3_assure(u3a_is_cat(old));

          u3z(muf);
          while ( len_w > (old + 1) ) {
            u3_noun t_pef = u3k(u3t(pef));
 
            len_w -= 1;
            u3z(pef);
            pef = t_pef;
          }
          muf = u3k(u3t(u3h(pef)));
          u3z(lab);
        }
      }
      don = u3t(don);
    }
    rod_u = u3tn(u3_road, rod_u->par_p);
  }
  u3z(muf);
  
  //  Lose the maps and save a pure label stack in original order.
  //
  {
    u3_noun pal = u3_nul;

    while ( u3_nul != pef ) {
      u3_noun h_pef = u3h(pef);
      u3_noun t_pef = u3k(u3t(pef));

      pal = u3nc(u3k(u3h(h_pef)), pal);

      u3z(pef);
      pef = t_pef;
    }

    // fprintf(stderr, "sample: stack length %d\r\n", u3kb_lent(u3k(pal)));
    return pal;
  }
}
#endif

/* u3t_samp(): sample.
*/
void
u3t_samp(void)
{
  u3C.wag_w &= ~u3o_debug_cpu;

  static int home = 0;
  static int away = 0;

  //  Profile sampling, because it allocates on the home road,
  //  only works on when we're not at home.
  //
  if ( &(u3H->rod_u) != u3R ) {
    home++;
    c3_l      mot_l;
    u3a_road* rod_u;
  
    if ( _(u3T.mal_o) ) {
      mot_l = c3_s3('m','a','l');
    }
    else if ( _(u3T.coy_o) ) {
      mot_l = c3_s3('c','o','y');
    }
    else if ( _(u3T.euq_o) ) {
      mot_l = c3_s3('e','u','q');
    }
    else if ( _(u3T.far_o) ) {
      mot_l = c3_s3('f','a','r');
    }
    else if ( _(u3T.noc_o) ) {
      c3_assert(!_(u3T.glu_o));
      mot_l = c3_s3('n','o','c');
    }
    else if ( _(u3T.glu_o) ) {
      mot_l = c3_s3('g','l','u');
    }
    else {
      mot_l = c3_s3('f','u','n');
    }

    rod_u = u3R;
    u3R = &(u3H->rod_u);
    {
      u3_noun lab = _t_samp_process(rod_u);

      c3_assert(u3R == &u3H->rod_u);
      if ( 0 == u3R->pro.day ) { 
        u3R->pro.day = u3v_do("doss", 0);
      }
      u3R->pro.day = u3dt("pi-noon", mot_l, lab, u3R->pro.day);
    }
    u3R = rod_u;
  }
  else {
    away++;
    // fprintf(stderr,"home: %06d away: %06d\r\n", home, away);
  }
  u3C.wag_w |= u3o_debug_cpu;
}

/* u3t_come(): push on profile stack; return yes if active push.  RETAIN.
*/
c3_o
u3t_come(u3_noun bat)
{
  if ( (u3_nul == u3R->pro.don) || !_(u3r_sing(bat, u3h(u3R->pro.don))) ) {
    u3R->pro.don = u3nc(u3k(bat), u3R->pro.don);
    return c3y;
  } 
  else return c3n;
}

/* u3t_flee(): pop off profile stack.
*/
void
u3t_flee(void)
{
  u3_noun t_don = u3k(u3t(u3R->pro.don));

  u3R->pro.don = t_don;
  u3z(u3R->pro.don);
}

/* u3t_damp(): print and clear profile data.
*/
void
u3t_damp(void)
{
  if ( 0 != u3R->pro.day ) {
    u3_noun wol = u3do("pi-tell", u3R->pro.day);
    u3m_wall(wol);

    u3R->pro.day = u3v_do("doss", 0);
  }
#if 0
  if ( 0 != u3R->pro.nox_d ) {
    printf("knox: %llu\r\n", (u3R->pro.nox_d / 1000ULL));
    u3R->pro.nox_d = 0;
  }
#endif
}

/* _ct_sigaction(): profile sigaction callback.
*/
void _ct_sigaction(c3_i x_i) 
{ 
  // fprintf(stderr, "itimer!\r\n"); abort();
  u3t_samp();
}

/* u3t_init(): initialize tracing layer.
*/
void
u3t_init(void)
{
  u3T.noc_o = c3n;
  u3T.glu_o = c3n;
  u3T.mal_o = c3n;
  u3T.far_o = c3n;
  u3T.coy_o = c3n;
  u3T.euq_o = c3n;
}

/* u3t_boot(): turn sampling on.
*/
void
u3t_boot(void)
{
  if ( u3C.wag_w & u3o_debug_cpu ) { 
#if defined(U3_OS_osx)
#if 1
    {
      struct itimerval itm_v;
      struct sigaction sig_s;
      sigset_t set;

      sig_s.__sigaction_u.__sa_handler = _ct_sigaction;
      sig_s.sa_mask = 0;
      sig_s.sa_flags = 0;
      sigaction(SIGPROF, &sig_s, 0);

      sigemptyset(&set);
      sigaddset(&set, SIGPROF);
      if ( 0 != pthread_sigmask(SIG_UNBLOCK, &set, NULL) ) {
        perror("pthread_sigmask");
      }

      itm_v.it_interval.tv_sec = 0;
      itm_v.it_interval.tv_usec = 10000;
      // itm_v.it_interval.tv_usec = 100000;
      itm_v.it_value = itm_v.it_interval;

      setitimer(ITIMER_PROF, &itm_v, 0);
    }
#endif
#elif defined(U3_OS_linux)
    {
      struct itimerval itm_v;
      struct sigaction sig_s;
      sigset_t set;

      sig_s.sa_handler = _ct_sigaction;
      sigemptyset(&(sig_s.sa_mask));
      sig_s.sa_flags = 0;
      sigaction(SIGPROF, &sig_s, 0);

      sigemptyset(&set);
      sigaddset(&set, SIGPROF);
      if ( 0 != pthread_sigmask(SIG_UNBLOCK, &set, NULL) ) {
        perror("pthread_sigmask");
      }

      itm_v.it_interval.tv_sec = 0;
      itm_v.it_interval.tv_usec = 10000;
      // itm_v.it_interval.tv_usec = 100000;
      itm_v.it_value = itm_v.it_interval;

      setitimer(ITIMER_PROF, &itm_v, 0);
    }
#elif defined(U3_OS_bsd)
    // TODO: support profiling on bsd
#else
#   error "port: profiling"
#endif
  }
}

/* u3t_boff(): turn profile sampling off.
*/
void
u3t_boff(void)
{
  if ( u3C.wag_w & u3o_debug_cpu ) {
#if defined(U3_OS_osx)
    struct sigaction sig_s;
    struct itimerval itm_v;
    sigset_t set;

    sigemptyset(&set);
    sigaddset(&set, SIGPROF);
    if ( 0 != pthread_sigmask(SIG_BLOCK, &set, NULL) ) {
      perror("pthread_sigmask");
    }

    itm_v.it_interval.tv_sec = 0;
    itm_v.it_interval.tv_usec = 0;
    itm_v.it_value = itm_v.it_interval;

    setitimer(ITIMER_PROF, &itm_v, 0);
    sigaction(SIGPROF, &sig_s, 0);

#elif defined(U3_OS_linux)
    // TODO: support profiling on linux
#elif defined(U3_OS_bsd)
    // TODO: support profiling on bsd
#else
#   error "port: profiling"
#endif
  }
}
