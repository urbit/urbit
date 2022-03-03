/* vere/time.c
**
*/
#include "noun/all.h"
#include "vere/vere.h"

/* u3_time_sec_in(): urbit seconds from unix time.
**
** Adjust for future leap secs!
*/
c3_d
u3_time_sec_in(c3_w unx_w)
{
  return 0x8000000cce9e0d80ULL + (c3_d)unx_w;
}

/* u3_time_sec_out(): unix time from urbit seconds.
**
** Adjust for future leap secs!
*/
c3_w
u3_time_sec_out(c3_d urs_d)
{
  c3_d adj_d = (urs_d - 0x8000000cce9e0d80ULL);

  if ( adj_d > 0xffffffffULL ) {
    fprintf(stderr, "Agh! It's 2106! And no one's fixed this shite!\n");
    exit(1);
  }
  return (c3_w)adj_d;
}

/* u3_time_fsc_in(): urbit fracto-seconds from unix microseconds.
*/
c3_d
u3_time_fsc_in(c3_w usc_w)
{
  c3_d usc_d = usc_w;

  return ((usc_d * 65536ULL) / 1000000ULL) << 48ULL;
}

/* u3_time_fsc_out: unix microseconds from urbit fracto-seconds.
*/
c3_w
u3_time_fsc_out(c3_d ufc_d)
{
  return (c3_w) (((ufc_d >> 48ULL) * 1000000ULL) / 65536ULL);
}

/* u3_time_msc_out: unix microseconds from urbit fracto-seconds.
*/
c3_w
u3_time_msc_out(c3_d ufc_d)
{
  return (c3_w) (((ufc_d >> 48ULL) * 1000ULL) / 65536ULL);
}

/* u3_time_in_tv(): urbit time from struct timeval.
*/
u3_atom
u3_time_in_tv(struct timeval* tim_tv)
{
  c3_w unx_w = tim_tv->tv_sec;
  c3_w usc_w = tim_tv->tv_usec;
  c3_d cub_d[2];

  cub_d[0] = u3_time_fsc_in(usc_w);
  cub_d[1] = u3_time_sec_in(unx_w);

  return u3i_chubs(2, cub_d);
}

/* u3_time_out_tv(): struct timeval from urbit time.
*/
void
u3_time_out_tv(struct timeval* tim_tv, u3_noun now)
{
  c3_d ufc_d = u3r_chub(0, now);
  c3_d urs_d = u3r_chub(1, now);

  tim_tv->tv_sec = u3_time_sec_out(urs_d);
  tim_tv->tv_usec = u3_time_fsc_out(ufc_d);

  u3z(now);
}

/* u3_time_in_ts(): urbit time from struct timespec.
*/
u3_atom
u3_time_in_ts(struct timespec* tim_ts)
{
  struct timeval tim_tv;

  tim_tv.tv_sec = tim_ts->tv_sec;
  tim_tv.tv_usec = (tim_ts->tv_nsec / 1000);

  return u3_time_in_tv(&tim_tv);
}

#if defined(U3_OS_linux) || defined(U3_OS_mingw)
/* u3_time_t_in_ts(): urbit time from time_t.
*/
u3_atom
u3_time_t_in_ts(time_t tim)
{
  struct timeval tim_tv;

  tim_tv.tv_sec = tim;
  tim_tv.tv_usec = 0;

  return u3_time_in_tv(&tim_tv);
}
#endif // defined(U3_OS_linux) || defined(U3_OS_mingw)

/* u3_time_out_ts(): struct timespec from urbit time.
*/
void
u3_time_out_ts(struct timespec* tim_ts, u3_noun now)
{
  struct timeval tim_tv;

  u3_time_out_tv(&tim_tv, now);

  tim_ts->tv_sec = tim_tv.tv_sec;
  tim_ts->tv_nsec = (tim_tv.tv_usec * 1000);
}

/* u3_time_gap_ms(): (wen - now) in ms.
*/
c3_d
u3_time_gap_ms(u3_noun now, u3_noun wen)
{
  if ( c3n == u3ka_gth(u3k(wen), u3k(now)) ) {
    u3z(wen); u3z(now);
    return 0ULL;
  }
  else {
    u3_noun dif   = u3ka_sub(wen, now);
    c3_d    fsc_d = u3r_chub(0, dif);
    c3_d    sec_d = u3r_chub(1, dif);

    u3z(dif);
    return (sec_d * 1000ULL) + u3_time_msc_out(fsc_d);
  }
}

/* u3_time_gap_double(): (wen - now) in libev resolution.
*/
double
u3_time_gap_double(u3_noun now, u3_noun wen)
{
  mpz_t now_mp, wen_mp, dif_mp;
  double sec_g = (((double)(1ULL << 32ULL)) * ((double)(1ULL << 32ULL)));
  double gap_g, dif_g;

  u3r_mp(now_mp, now);
  u3r_mp(wen_mp, wen);
  mpz_init(dif_mp);
  mpz_sub(dif_mp, wen_mp, now_mp);

  u3z(now);
  u3z(wen);

  dif_g = mpz_get_d(dif_mp) / sec_g;
  gap_g = (dif_g > 0.0) ? dif_g : 0.0;
  mpz_clear(dif_mp); mpz_clear(wen_mp); mpz_clear(now_mp);

  return gap_g;
}
