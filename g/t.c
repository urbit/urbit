/* g/t.c
**
** This file is in the public domain.
*/
#include "all.h"

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
  printf("ct: heck %s\r\n", u3r_string(cog));

  if ( 0 == u3R->pro.day ) { u3R->pro.day = u3v_do("doss", 0); }

  u3R->pro.day = u3_dc("pi-heck", cog, u3R->pro.day);
}

/* u3t_samp(): sample.
*/
void
u3t_samp(void)
{
  if ( 0 == u3R->pro.day ) { u3R->pro.day = u3v_do("doss", 0); }

  u3R->pro.day = u3_dc("pi-noon", u3k(u3R->pro.don), u3R->pro.day);
}

/* u3t_come(): push on profile stack.
*/
void
u3t_come(u3_atom cog)
{
  printf("ct: come %s\r\n", u3r_string(cog));

  u3R->pro.don = u3nc(cog, u3R->pro.don);
}

/* u3t_flee(): pop off profile stack.
*/
void
u3t_flee(void)
{
  c3_assert(_(u3du(u3R->pro.don)));
  {
    u3_noun tax = u3R->bug.tax;

    u3R->bug.tax = u3k(u3t(tax));
    u3z(tax);
  }
}

/* u3t_damp(): print and clear profile data.
*/
void
u3t_damp(void)
{
  if ( 0 != u3R->pro.day ) {
    u3_noun wol = u3_do("pi-tell", u3R->pro.day);
    u3m_wall(wol);

    u3R->pro.day = u3v_do("doss", 0);
  }

  if ( 0 != u3R->pro.nox_d ) {
    printf("knox: %llu\r\n", (u3R->pro.nox_d / 1000ULL));
    u3R->pro.nox_d = 0;
  }
}

/* _ct_sigaction(): profile sigaction callback.
*/
void _ct_sigaction(c3_i x_i) { u3t_samp(); } 

/* u3t_boot(): turn sampling on.
*/
void
u3t_boot(void)
{
  printf("ct: now profiling.\r\n");

  printf("knox: %llu\r\n", (u3R->pro.nox_d / 1000ULL));
  u3R->pro.nox_d = 0;

#if defined(U3_OS_osx)
#if 0
  {
    struct itimerval itm_v;
    struct sigaction sig_s;

    sig_s.__sigaction_u.__sa_handler = _ct_sigaction;
    sig_s.sa_mask = 0;
    sig_s.sa_flags = 0;
    sigaction(SIGPROF, &sig_s, 0);

    itm_v.it_interval.tv_sec = 0;
    itm_v.it_interval.tv_usec = 10000;
    itm_v.it_value = itm_v.it_interval;

    setitimer(ITIMER_PROF, &itm_v, 0);
  }
#endif
#elif defined(U3_OS_linux)
    // TODO: support profiling on linux
#elif defined(U3_OS_bsd)
    // TODO: support profiling on bsd
#else
   #error "port: profiling"
#endif
}

/* u3t_boff(): turn profile sampling off.
*/
void
u3t_boff(void)
{
#if defined(U3_OS_osx)
  struct sigaction sig_s;
  struct itimerval itm_v;

  printf("ct: end profiling.\r\n");

  itm_v.it_interval.tv_sec = 0;
  itm_v.it_interval.tv_usec = 0;
  itm_v.it_value = itm_v.it_interval;

  setitimer(ITIMER_PROF, &itm_v, 0);
  sigaction(SIGPROF, &sig_s, 0);

  u3t_damp();
#elif defined(U3_OS_linux)
    // TODO: support profiling on linux
#elif defined(U3_OS_bsd)
    // TODO: support profiling on bsd
#else
   #error "port: profiling"
#endif
}
