/* g/t.c  -
**
** This file is in the public domain.
*/
#include "all.h"
#include "vere/vere.h"
#include <pthread.h>
#include <time.h>
#include <sys/stat.h>

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

/* u3t_slog(): print directly.
*/
void
u3t_slog(u3_noun hod)
{
  if ( 0 != u3C.slog_f ) {
    u3C.slog_f(hod);
  }
  else {
    u3z(hod);
  }
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
        u3R->pro.day = u3do("doss", 0);
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

    // u3l_log("sample: stack length %d\r\n", u3kb_lent(u3k(pal)));
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

  c3_w old_wag = u3C.wag_w;
  u3C.wag_w &= ~u3o_debug_cpu;
  u3C.wag_w &= ~u3o_trace;

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
  u3C.wag_w = old_wag;
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

/*  u3t_trace_open(): opens a trace file and writes the preamble.
*/
void
u3t_trace_open(c3_c* dir_c)
{
  c3_c fil_c[2048];
  snprintf(fil_c, 2048, "%s/.urb/put/trace", dir_c);

  struct stat st;
  if ( -1 == stat(fil_c, &st) ) {
    c3_mkdir(fil_c, 0700);
  }

  c3_c lif_c[2056];
  snprintf(lif_c, 2056, "%s/%d.json", fil_c, u3_Host.tra_u.fun_w);

  u3_Host.tra_u.fil_u = c3_fopen(lif_c, "w");
  u3_Host.tra_u.nid_w = (int)getpid();

  fprintf(u3_Host.tra_u.fil_u, "[ ");

  // We have two "threads", the event processing and the nock stuff.
  //   tid 1 = event processing
  //   tid 2 = nock processing
  fprintf(u3_Host.tra_u.fil_u,
      "{\"name\": \"process_name\", \"ph\": \"M\", \"pid\": %d, \"args\": "
      "{\"name\": \"urbit\"}},\n",
      u3_Host.tra_u.nid_w);
  fprintf(u3_Host.tra_u.fil_u,
          "{\"name\": \"thread_name\", \"ph\": \"M\", \"pid\": %d, \"tid\": 1, "
          "\"args\": {\"name\": \"Event Processing\"}},\n",
          u3_Host.tra_u.nid_w);
  fprintf(u3_Host.tra_u.fil_u,
          "{\"name\": \"thread_sort_index\", \"ph\": \"M\", \"pid\": %d, "
          "\"tid\": 1, \"args\": {\"sort_index\": 1}},\n",
          u3_Host.tra_u.nid_w);
  fprintf(u3_Host.tra_u.fil_u,
          "{\"name\": \"thread_name\", \"ph\": \"M\", \"pid\": %d, \"tid\": 2, "
          "\"args\": {\"name\": \"Nock\"}},\n",
          u3_Host.tra_u.nid_w);
  fprintf(u3_Host.tra_u.fil_u,
          "{\"name\": \"thread_sort_index\", \"ph\": \"M\", \"pid\": %d, "
          "\"tid\": 2, \"args\": {\"sort_index\": 2}},\n",
          u3_Host.tra_u.nid_w);
  u3_Host.tra_u.con_w = 5;
}

/*  u3t_trace_close(): closes a trace file. optional.
*/
void
u3t_trace_close()
{
  if (!u3_Host.tra_u.fil_u)
    return;

  // We don't terminate the JSON because of the file format.
  fclose(u3_Host.tra_u.fil_u);
  u3_Host.tra_u.con_w = 0;
  u3_Host.tra_u.fun_w++;
}

/*  u3t_trace_time(): microsecond clock
*/
c3_d u3t_trace_time()
{
  struct timeval tim_tv;
  gettimeofday(&tim_tv, 0);
  return 1000000ULL * tim_tv.tv_sec + tim_tv.tv_usec;
}

/* u3t_nock_trace_push(): push a trace onto the trace stack; returns yes if pushed.
 *
 * The trace stack is a stack of [path time-entered].
 */
c3_o
u3t_nock_trace_push(u3_noun lab)
{
  if (!u3_Host.tra_u.fil_u)
    return c3n;

  if ( (u3_nul == u3R->pro.trace) ||
       !_(u3r_sing(lab, u3h(u3h(u3R->pro.trace)))) ) {
    u3a_gain(lab);
    c3_d time = u3t_trace_time();
    u3R->pro.trace = u3nc(u3nc(lab, u3i_chubs(1, &time)), u3R->pro.trace);
    return c3y;
  }
  else {
    return c3n;
  }
}

/* u3t_nock_trace_pop(): pops a trace from the trace stack.
 *
 * When we remove the trace from the stack, we check to see if the sample is
 * large enough to process, as we'll otherwise keep track of individual +add
 * calls. If it is, we write it out to the tracefile.
 */
void
u3t_nock_trace_pop()
{
  if (!u3_Host.tra_u.fil_u)
    return;

  u3_noun trace  = u3R->pro.trace;
  u3R->pro.trace = u3k(u3t(trace));

  u3_noun item = u3h(trace);
  u3_noun lab = u3h(item);
  c3_d start_time = u3r_chub(0, u3t(item));

  // 33microseconds (a 30th of a millisecond).
  c3_d duration = u3t_trace_time() - start_time;
  if (duration > 33) {
    c3_c* name = u3m_pretty_path(lab);

    fprintf(u3_Host.tra_u.fil_u,
            "{\"cat\": \"nock\", \"name\": \"%s\", \"ph\":\"%c\", \"pid\": %d, "
            "\"tid\": 2, \"ts\": %" PRIu64 ", \"dur\": %" PRIu64 "}, \n",
            name,
            'X',
            u3_Host.tra_u.nid_w,
            start_time,
            duration);

    c3_free(name);
    u3_Host.tra_u.con_w++;
  }

  u3z(trace);
}

/* u3t_event_trace(): dumps a simple event from outside nock.
*/
void
u3t_event_trace(const c3_c* name, c3_c type)
{
  if (!u3_Host.tra_u.fil_u)
    return;

  fprintf(u3_Host.tra_u.fil_u,
          "{\"cat\": \"event\", \"name\": \"%s\", \"ph\":\"%c\", \"pid\": %d, "
          "\"tid\": 1, \"ts\": %" PRIu64 ", \"id\": \"0x100\"}, \n",
          name,
          type,
          u3_Host.tra_u.nid_w,
          u3t_trace_time());
  u3_Host.tra_u.con_w++;
}

/* u3t_print_steps: print step counter.
*/
void
u3t_print_steps(FILE* fil_u, c3_c* cap_c, c3_d sep_d)
{
  c3_assert( 0 != fil_u );

  c3_w gib_w = (sep_d / 1000000000ULL);
  c3_w mib_w = (sep_d % 1000000000ULL) / 1000000ULL;
  c3_w kib_w = (sep_d % 1000000ULL) / 1000ULL;
  c3_w bib_w = (sep_d % 1000ULL);

  //  XX prints to stderr since it's called on shutdown, daemon may be gone
  //
  if ( sep_d ) {
    if ( gib_w ) {
      fprintf(fil_u, "%s: G/%d.%03d.%03d.%03d\r\n",
          cap_c, gib_w, mib_w, kib_w, bib_w);
    }
    else if ( mib_w ) {
      fprintf(fil_u, "%s: M/%d.%03d.%03d\r\n", cap_c, mib_w, kib_w, bib_w);
    }
    else if ( kib_w ) {
      fprintf(fil_u, "%s: K/%d.%03d\r\n", cap_c, kib_w, bib_w);
    }
    else if ( bib_w ) {
      fprintf(fil_u, "%s: %d\r\n", cap_c, bib_w);
    }
  }
}

/* u3t_damp(): print and clear profile data.
*/
void
u3t_damp(FILE* fil_u)
{
  c3_assert( 0 != fil_u );

  if ( 0 != u3R->pro.day ) {
    u3_noun wol = u3do("pi-tell", u3R->pro.day);

    //  XX prints to stderr since it's called on shutdown, daemon may be gone
    //
    {
      u3_noun low = wol;

      while ( u3_nul != low ) {
        c3_c* str_c = (c3_c*)u3r_tape(u3h(low));
        fputs(str_c, fil_u);
        fputs("\r\n", fil_u);

        c3_free(str_c);
        low = u3t(low);
      }

      u3z(wol);
    }

    /* bunt a +doss
    */
    u3R->pro.day = u3nt(u3nq(0, 0, 0, u3nq(0, 0, 0, 0)), 0, 0);
  }

  u3t_print_steps(fil_u, "nocks", u3R->pro.nox_d);
  u3t_print_steps(fil_u, "cells", u3R->pro.cel_d);

  u3R->pro.nox_d = 0;
  u3R->pro.cel_d = 0;
}

/* _ct_sigaction(): profile sigaction callback.
*/
void _ct_sigaction(c3_i x_i)
{
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
#if defined(U3_OS_PROF)
    //  skip profiling if we don't yet have an arvo kernel
    //
    if ( 0 == u3A->roc ) {
      return;
    }

    // Register _ct_sigaction to be called on `SIGPROF`.
    {
      struct sigaction sig_s = {{0}};
      sig_s.sa_handler = _ct_sigaction;
      sigemptyset(&(sig_s.sa_mask));
      sigaction(SIGPROF, &sig_s, 0);
    }

    // Unblock `SIGPROF` for this thread (we will block it again when `u3t_boff` is called).
    {
      sigset_t set;
      sigemptyset(&set);
      sigaddset(&set, SIGPROF);
      if ( 0 != pthread_sigmask(SIG_UNBLOCK, &set, NULL) ) {
        u3l_log("trace: thread mask SIGPROF: %s\r\n", strerror(errno));
      }
    }

    // Ask for SIGPROF to be sent every 10ms.
    {
      struct itimerval itm_v = {{0}};
      itm_v.it_interval.tv_usec = 10000;
      itm_v.it_value = itm_v.it_interval;
      setitimer(ITIMER_PROF, &itm_v, 0);
    }
#endif
  }
}

/* u3t_boff(): turn profile sampling off.
*/
void
u3t_boff(void)
{
  if ( u3C.wag_w & u3o_debug_cpu ) {
#if defined(U3_OS_PROF)
    // Mask SIGPROF signals in this thread (and this is the only
    // thread that unblocked them).
    {
      sigset_t set;
      sigemptyset(&set);
      sigaddset(&set, SIGPROF);
      if ( 0 != pthread_sigmask(SIG_BLOCK, &set, NULL) ) {
        u3l_log("trace: thread mask SIGPROF: %s\r\n", strerror(errno));
      }
    }

    // Disable the SIGPROF timer.
    {
      struct itimerval itm_v = {{0}};
      setitimer(ITIMER_PROF, &itm_v, 0);
    }

    // Ignore SIGPROF signals.
    {
      struct sigaction sig_s = {{0}};
      sigemptyset(&(sig_s.sa_mask));
      sig_s.sa_handler = SIG_IGN;
      sigaction(SIGPROF, &sig_s, 0);
    }
#endif
  }
}


/* u3t_slog_cap(): slog a tank with a caption with
** a given priority c3_l (assumed 0-3).
*/
void
u3t_slog_cap(c3_l pri_l, u3_noun cap, u3_noun tan)
{
  u3t_slog(
    u3nc(
      pri_l,
      u3nt(
        c3__rose,
        u3nt(u3nt(':', ' ', u3_nul), u3_nul, u3_nul),
        u3nt(cap, tan, u3_nul)
      )
    )
  );
}

/* _slog_blank(): slog out a blank line with
** a given priority c3_l (assumed 0-3).
*/
void
_slog_blank(c3_l pri_l)
{
  u3t_slog(
    u3nc(
      pri_l,
      u3nt(
        c3__rose,
        u3nt(u3nt(' ', ' ', u3_nul), u3_nul, u3_nul),
        u3nt(u3i_string(" "), u3i_string(" "), u3_nul)
      )
    )
  );
}


/* u3t_slog_trace(): given a c3_l priority pri and a raw stack tax
** flop the order into start-to-end, render, and slog each item
** until done.
*/
void
u3t_slog_trace(c3_l pri_l, u3_noun tax)
{
  // render the stack
  // Note: ton is a reference to a data struct
  // we have just allocated
  // lit is used as a moving cursor pointer through
  // that allocated struct
  // once we finish lit will be null, but ton will still
  // point to the whole valid allocated data structure
  // and thus we can free it safely at the end of the func
  // to clean up after ourselves.
  // Note: flop reverses the stack trace list 'tax'
  u3_noun ton = u3dc("mook", 2, u3kb_flop(tax));
  u3_noun lit = u3t(ton);

  // print the stack one stack item at a time
  while ( u3_nul != lit ) {
    u3t_slog(u3nc(pri_l, u3k(u3h(lit)) ));
    lit = u3t(lit);
  }

  u3z(ton);
}


/* u3t_slog_nara(): slog only the deepest road's trace with
** c3_l priority pri
*/
void
u3t_slog_nara(c3_l pri_l)
{
  u3_noun tax = u3k(u3R->bug.tax);
  u3t_slog_trace(pri_l, tax);
}


/* u3t_slog_hela(): join all roads' traces together into one tax
** and pass it to slog_trace along with the given c3_l priority pri_l
*/
void
u3t_slog_hela(c3_l pri_l)
{
  // rod_u protects us from mutating the global state
  u3_road* rod_u = u3R;

  // inits to the the current road's trace
  u3_noun tax = u3k(rod_u->bug.tax);

  // while there is a parent road ref ...
  while ( &(u3H->rod_u) != rod_u ) {
    // ... point at the next road and append its stack to tax
    rod_u = u3tn(u3_road, rod_u->par_p);
    tax = u3kb_weld(tax, u3k(rod_u->bug.tax));
  }

  u3t_slog_trace(pri_l, tax);
}

/* _roundf(): truncate a float to preciscon
** equivalant to %.2f
*/
float
_roundf(float percent)
{
  // scale the percentage so that all siginificant digits
  // would be retained when truncted to an int, then add 0.5
  // to account for rounding without using round or roundf
  float percent_big_f = (percent*10000)+0.5;
  // truncate to int
  int percent_big_i = (int) percent_big_f;
  // convert to float and scale down such that
  // our last to digits are right of the decimal
  float percent_truncated_f = (float) percent_big_i/100.0;
  return percent_truncated_f;
}


float
_meme_percent(unsigned int small, unsigned int large)
{
  // get the percentage of our inputs as a float
  float percent_raw_f = (float) small/large;
  return _roundf(percent_raw_f);
}

/*
void
_slog_free_discrepancy( )
{
  c3_w fre_w = u3a_idle(u3R);
  if ( fre_w != u3R->all.fre_w ) {
    char* s;
    int result = asprintf(&s,
      "%x\n\t\t\tfre_w: %x\n\t\t    all.fre_w: %x\n\t\t\t diff: %x",
      u3R->par_p,
      fre_w,
      u3R->all.fre_w,
      (u3R->all.fre_w - fre_w)
    );
    if (0 <= result) {
      u3t_slog_cap(2, u3i_string("  free discrepancy at par_p"), u3i_string(s));
      free(s);
    }
  }
}
*/

void _slog_road_depth(c3_l pri_l, u3_road* r, int i) {
  if (r == &(u3H->rod_u)) {
    // slog the info
    unsigned int x = 0;
    char s[8] = "      0";
    while (i > 0 && x < 8) {
      s[6-x] = (i%10)+'0';
      i /= 10;
      x++;
    }
    u3t_slog_cap(pri_l, u3i_string("          road depth"), u3i_string(s));
  } else {
    // recurse
    _slog_road_depth(pri_l, u3tn(u3_road, r->par_p), ++i);
  }
}


int _all_heap_size(u3_road* r) {
  if (r == &(u3H->rod_u)) {
    return u3a_heap(r)*4;
  } else {
    // recurse
    return (u3a_heap(r)*4) + _all_heap_size(u3tn(u3_road, r->par_p));
  }
}

struct
_report_bar {
  char s[105];
};

struct
bar_item {
  unsigned int index;
  unsigned int lower;
  float og;
  float dif;
};

struct
bar_items {
  struct bar_item s[6];
};

float
_boost_small(float x)
{
  return
    // we want zero to be zero,
    // and anything between zero and one to be one
    // all else goes as normal
    0.0 >= x ? 0.0:
    1.0 > x ? 1.0:
    x;
}

int
_global_difference(struct bar_items item)
{
  unsigned int lower_sum = 0;
  for (unsigned int i=0; i < 6; i++) lower_sum += item.s[i].lower;
  return 100 - lower_sum;
}

struct bar_items
_get_roundoff_error(struct bar_items item)
{
  for (unsigned int i=0; i < 6; i++) {
    item.s[i].dif = item.s[i].og - item.s[i].lower;
  }
  return item;
}

struct bar_items
_sort_by_roundoff_error(struct bar_items item)
{
  struct bar_item temp;
  for (unsigned int i=1; i < 6; i++) {
    for (unsigned int j=0; j < 6-i; j++) {
      if (item.s[j+1].dif > item.s[j].dif) {
        temp = item.s[j];
        item.s[j] = item.s[j+1];
        item.s[j+1] = temp;
      }
    }
  }
  return item;
}

struct bar_items
_sort_by_index(struct bar_items item)
{
  struct bar_item temp;
  for (unsigned int i=1; i < 6; i++) {
    for (unsigned int j=0; j < 6-i; j++) {
      if (item.s[j+1].index < item.s[j].index) {
        temp = item.s[j];
        item.s[j] = item.s[j+1];
        item.s[j+1] = temp;
      }
    }
  }
  return item;
}

void
_print_bar_items(struct bar_items item)
{
  const char symbol[6] = "=-%#+~";
  for (int i=0; i < 6; i++) printf(
    "item:%c,%2u, %6.2f,%6.2f, %2u\n",
    symbol[i],
    item.s[i].index,
    item.s[i].og,
    item.s[i].dif,
    item.s[i].lower
  );
}

struct bar_items
_reduce_error(struct bar_items item, int difference)
{
  for (unsigned int i=0; i < 6; i++) {
    if (item.s[i].lower == 0) continue;
    if (item.s[i].lower == 1) continue;
    if (difference > 0) {
      item.s[i].lower++;
      difference--;
    }
    if (difference < 0) {
      item.s[i].lower--;
      difference++;
    }
  }
  return item;
}

struct _report_bar
_report_bargraph(float ih, float sh, float fh, float op, float sk, float ik)
{
  float in[6];
  in[0] = _boost_small(ih);
  in[1] = _boost_small(sh);
  in[2] = _boost_small(fh);
  in[3] = _boost_small(op);
  in[4] = _boost_small(sk);
  in[5] = _boost_small(ik);

  const char symbol[6] = "=-%#+~";

  // init the list of structs
  struct bar_items item;
  for (unsigned int i=0; i < 6; i++) {
    item.s[i].index = i;
    item.s[i].og = in[i];
    item.s[i].lower = (int) item.s[i].og;
  }

  int difference = 0;
  for (int x=0; x<100; x++) {
    item = _get_roundoff_error(item);
    difference = _global_difference(item);
    if (difference == 0) break;
    item = _sort_by_roundoff_error(item);
    item = _reduce_error(item, difference);
  }
  item = _sort_by_index(item);

  struct _report_bar bar = {
    .s = "[                                                                                                    ]"
  };

  // create our bar chart
  int x = 0, y = 0;
  for (int i=0; i < 6; i++) {
    x++;
    for (int j=0; j < item.s[i].lower; j++) {
      bar.s[x+j] = symbol[i];
      y = x+j;
    }
    if (y > 0) x = y;
  }

  return bar;
}


/* u3t_meme(): report memory stats at call time */
void
u3t_slog_meme(c3_l pri_l)
{
  c3_w low  = 0,
       top  = u3a_bytes,
       full = u3a_full(u3R)*4,
       fred = u3a_idle(u3R)*4,
       temp = u3a_temp(u3R)*4,
       heap = u3a_heap(u3R)*4,
       open = u3a_open(u3R)*4;
  c3_w imut = top-full;
  c3_w solid = heap-fred;

  float imut_p = _meme_percent(imut, top),
        heap_p = _meme_percent(solid, top),
        free_p = _meme_percent(fred, top),
        open_p = _meme_percent(open, top),
        stak_p = _meme_percent(temp, top);
  float full_p = heap_p + free_p + open_p + stak_p;

  // TODO: replace all calls of free() with calls of u3a_free() or its alias.

  c3_w imut_heap = _all_heap_size(u3R) - heap;
  c3_w imut_stak = imut - imut_heap;
  float imut_heap_p = _meme_percent(imut_heap, top),
        imut_stak_p = _meme_percent(imut_stak, top);

  u3t_slog_cap(pri_l, u3i_string("Legend | Report"), u3i_string(" "));
  u3t_slog_memory(pri_l, "                loom",  100.0, top);
  u3t_slog_memory(pri_l, "                road", full_p, full);
  _slog_blank(pri_l);
  u3t_slog_memory(pri_l, "  =  immutable  heap", imut_heap_p, imut_heap);
  u3t_slog_memory(pri_l, "  -      solid  heap", heap_p, solid);
  u3t_slog_memory(pri_l,"  \%      freed  heap", free_p, fred);
  u3t_slog_memory(pri_l, "  #       open space", open_p, open);
  u3t_slog_memory(pri_l, "  +            stack", stak_p, temp);
  u3t_slog_memory(pri_l, "  ~  immutable stack", imut_stak_p, imut_stak);
  _slog_blank(pri_l);
  u3t_slog_memory(pri_l, "  $ allocation frame", imut_heap_p, imut_heap);
#ifdef U3_CPU_DEBUG
  /* iff we are using CPU_DEBUG env var
  ** we can report more facts:
  **  max_w: max allocated on the current road (not global, not including child roads)
  **  cel_d: max cells allocated in current road (inc closed kids, but not parents)
  **  nox_d: nock steps performed in current road, less caching
  */
  c3_w max = (u3R->all.max_w*4)+imut;
  float max_p = _meme_percent(max, top);
  c3_d cells = u3R->pro.cel_d;
  c3_d nox = u3R->pro.nox_d;
  u3t_slog_memory(pri_l, "  |  road max memory", max_p, max);
  _slog_blank(pri_l);
  u3t_slog_steps(pri_l,  "     road cells made", cells);
  u3t_slog_steps(pri_l,  "     road nocks made", nox);
#endif
  if ( u3a_is_north(u3R) == c3y ) {
    u3t_slog_cap(pri_l, u3i_string("      road direction"), u3i_string("  North"));
  } else {
    u3t_slog_cap(pri_l, u3i_string("      road direction"), u3i_string("  South"));
  }
  _slog_road_depth(pri_l, u3R, 1);
  _slog_blank(pri_l);

  // warn if any sanity checks have failed
  if (100.01 < (imut_heap_p + heap_p + free_p + open_p + stak_p + imut_stak_p))
    u3t_slog_cap(3, u3i_string("error"), u3i_string("loom sums over 100%"));
  if ( 99.99 > (imut_heap_p + heap_p + free_p + open_p + stak_p + imut_stak_p))
    u3t_slog_cap(3, u3i_string("error"), u3i_string("loom sums under 100%"));

  struct _report_bar bar = _report_bargraph(
    imut_heap_p,
    heap_p,
    free_p,
    open_p,
    stak_p,
    imut_stak_p
  );
  int dol = (int) _roundf(imut_heap_p/100);
  bar.s[dol] = '$';
#ifdef U3_CPU_DEBUG
  // iff we have a max_p we will render it into the bar graph
  // in other words iff we have max_p it will always replace something
  c3_w inc_max = (max_p > imut_heap_p+1.0) ? (c3_w) max_p+0.5 : (c3_w) imut_heap_p+1.5;
  if (max_p > 0.0) bar.s[inc_max] = '|';
#endif
  // TODO: not sure we really need _slog_free_discrepancy()
  //_slog_free_discrepancy();
  u3t_slog_cap(pri_l, u3i_string("Loom"), u3i_string(bar.s));
}

int
_size_prefix(unsigned int wor_w)
{
  return
    (wor_w / 1000000000) ? 'G':
    (wor_w % 1000000000) / 1000000 ? 'M':
    (wor_w % 1000000) / 1000 ? 'K':
    (wor_w % 1000) ? ' ':
    'X';
}

// create a struct to allow passing around a fixed size string
struct _report {
  char s[32];
};

struct _report
report_string(unsigned int wor_w)
{
  struct _report r = {
    .s = "                              "
  };
  // add the G/M/K prefix
  r.s[24] = _size_prefix(wor_w);
  // consume wor_w into a string one base-10 digit at a time
  // including dot formatting
  unsigned int i = 0, j = 0;
  while (wor_w > 0) {
    if (j == 3) {
      r.s[22-i] = '.';
      i++;
      j = 0;
    } else {
      r.s[22-i] = (wor_w%10)+'0';
      wor_w /= 10;
      i++;
      j++;
    }
  }
  // return our fixed size string within a struct
  return r;
}

void
u3t_slog_memory(c3_l pri_l, c3_c* cap_c, float percent, c3_w wor_w)
{
  // create the report string and apply it to our char array s
  struct _report r = report_string(wor_w);
  // add the Bytes postfix to the size report
  r.s[25] = 'B';

  // add the space-percentage into the report
  r.s[2] = '0', r.s[3] = '.', r.s[4] = '0', r.s[5] = '0';
  int per_int = (int) (percent*100);
  unsigned int i = 0;
  while (per_int > 0 && i < 6) {
    if (i == 2) {
      r.s[5-i] = '.';
    } else {
      r.s[5-i] = (per_int%10)+'0';
      per_int /= 10;
    }
    i++;
  }
  // add the percent sign
  r.s[6] = '%';
  // slog it and go home
  u3t_slog_cap(pri_l, u3i_string(cap_c), u3i_string(r.s));
}


void
u3t_slog_steps(c3_l pri_l, c3_c* cap_c, c3_d sep_d)
{
  struct _report r = report_string(sep_d);
  u3t_slog_cap(pri_l, u3i_string(cap_c), u3i_string(r.s));
}


/* u3t_render_bytecode(): collect the bytecode data
** for the expression wrapped in the calling hint
** and
*
u3_noun
u3t_render_bytecode(c3_y* pog, c3_w her_w)
{
  //NOTE: this is basically the slog_byecode() function in nock.c
}
*/

