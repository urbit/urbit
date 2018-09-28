/* g/t.c  -
**
** This file is in the public domain.
*/
#include "all.h"
#include <pthread.h>

static c3_o _ct_lop_o;

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
u3_lo_tank(c3_l tab_l, u3_noun tac);

/* u3t_slog(): print directly.
*/
void
u3t_slog(u3_noun hod)
{
#ifdef GHETTO
  static int old;
  static struct timeval b4, f2, d0;
  c3_w ms_w;
          
  if ( old ) {
    gettimeofday(&f2, 0); 
    timersub(&f2, &b4, &d0);
    ms_w = (d0.tv_sec * 1000) + (d0.tv_usec / 1000);
    if (ms_w > 10) {
      printf("%6d.%02dms ", ms_w, (int) (d0.tv_usec % 1000) / 10);
      gettimeofday(&b4, 0);
    }
    else {
      printf("            ");
    }
  }
  else gettimeofday(&b4, 0);
  old = 1;

#endif
  if ( c3y == u3du(hod) ) {
    u3_noun pri = u3h(hod);

    switch ( pri ) {
      case 3: printf(">>> "); break;
      case 2: printf(">> "); break;
      case 1: printf("> "); break;
    }
    u3_lo_tank(0, u3k(u3t(hod)));
  }
  u3z(hod);
}

/* u3t_heck(): profile point.
*/
void
u3t_heck(u3_atom cog)
{
#if 0
  u3R->pro.cel_d++;
#else
  c3_w len_w = u3r_met(3, cog);
  c3_c* str_c = alloca(1 + len_w);

  u3r_bytes(0, len_w, (c3_y *)str_c, cog);
  str_c[len_w] = 0;

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
      u3R->pro.day = u3dc("pi-heck", u3i_string(str_c), u3R->pro.day);
    }
    u3R = rod_u;
  }
#endif
}

#if 0
static void
_ct_sane(u3_noun lab)
{
  if ( u3_nul != lab ) {
    c3_assert(c3y == u3du(lab));
    c3_assert(c3y == u3ud(u3h(lab)));
    _ct_sane(u3t(lab));
  }
}
#endif

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
      //  Get surface allocated label
      //
      //  u3_noun lab = u3nc(u3i_string("foobar"), 0);
      u3_noun laj = u3h(don),
              lab = u3a_take(laj);
      u3a_wash(laj);

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
  if ( c3y == _ct_lop_o ) {
    // _ct_lop_o here is a mutex for modifying pro.don. we
    // do not want to sample in the middle of doing that, as
    // it can cause memory errors.
    return;
  }
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
        /* bunt a +doss
        */
        u3R->pro.day = u3nt(u3nq(0, 0, 0, u3nq(0, 0, 0, 0)), 0, 0);
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
u3t_come(u3_noun lab)
{
  if ( (u3_nul == u3R->pro.don) || !_(u3r_sing(lab, u3h(u3R->pro.don))) ) {
    u3a_gain(lab);
    _ct_lop_o = c3y;
    u3R->pro.don = u3nc(lab, u3R->pro.don);
    _ct_lop_o = c3n;
    return c3y;
  } 
  else return c3n;
}

/* u3t_flee(): pop off profile stack.
*/
void
u3t_flee(void)
{
  _ct_lop_o = c3y;
  u3_noun don  = u3R->pro.don;
  u3R->pro.don = u3k(u3t(don));
  _ct_lop_o = c3n;
  u3z(don);
}

extern FILE*
u3_term_io_hija(void);

extern void
u3_term_io_loja(int x);

extern void
u3_term_tape(u3_noun tep);

extern void
u3_term_wall(u3_noun wol);

/* u3t_print_steps: print step counter.
*/
void
u3t_print_steps(c3_c* cap_c, c3_d sep_d)
{
  FILE* fil_f = u3_term_io_hija();

  c3_w gib_w = (sep_d / 1000000000ULL);
  c3_w mib_w = (sep_d % 1000000000ULL) / 1000000ULL;
  c3_w kib_w = (sep_d % 1000000ULL) / 1000ULL;
  c3_w bib_w = (sep_d % 1000ULL);

  if ( sep_d ) {
    if ( gib_w ) {
      fprintf(fil_f, "%s: G/%d.%03d.%03d.%03d\r\n", 
          cap_c, gib_w, mib_w, kib_w, bib_w);
    }
    else if ( mib_w ) {
      fprintf(fil_f, "%s: M/%d.%03d.%03d\r\n", cap_c, mib_w, kib_w, bib_w);
    }
    else if ( kib_w ) {
      fprintf(fil_f, "%s: K/%d.%03d\r\n", cap_c, kib_w, bib_w);
    }
    else if ( bib_w ) {
      fprintf(fil_f, "%s: %d\r\n", cap_c, bib_w);
    }
  }
  u3_term_io_loja(0);
}

/* u3t_damp(): print and clear profile data.
*/
void
u3t_damp(void)
{
  fprintf(stderr, "\r\n");

  if ( 0 != u3R->pro.day ) {
    u3_noun wol = u3do("pi-tell", u3R->pro.day);
    u3_term_wall(wol);

    /* bunt a +doss
    */
    u3R->pro.day = u3nt(u3nq(0, 0, 0, u3nq(0, 0, 0, 0)), 0, 0);
  }

  u3t_print_steps("nocks", u3R->pro.nox_d);
  u3t_print_steps("cells", u3R->pro.cel_d);

  u3R->pro.nox_d = 0;
  u3R->pro.cel_d = 0;
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
    _ct_lop_o = c3n;
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
