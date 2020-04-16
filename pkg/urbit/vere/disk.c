/* vere/disk.c
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
#include "vere/vere.h"

struct _cd_read {
  c3_d             eve_d;
  c3_d             len_d;
  struct _u3_fact* ent_u;               //  queue entry
  struct _u3_fact* ext_u;               //  queue exit
  struct _u3_disk* log_u;
};

typedef struct _u3_db_batch {
  c3_d             eve_d;               //  first event
  c3_d             len_d;               //  number of events
  void**           byt_p;               //  array of bytes
  size_t*          siz_i;               //  array of lengths
} u3_db_batch;

#undef VERBOSE_DISK

static void
_disk_commit(u3_disk* log_u);

/* u3_disk_init(): load or create pier and log.
*/
u3_disk*
u3_disk_init(c3_c* pax_c, u3_disk_cb cb_u)
{
  u3_disk* log_u = c3_calloc(sizeof(*log_u));
  log_u->liv_o = c3n;
  log_u->hol_o = c3n;
  log_u->cb_u  = cb_u;

  uv_timer_init(u3L, &log_u->tim_u);

  //  create/load pier directory
  //
  {
    if ( 0 == (log_u->dir_u = u3_foil_folder(pax_c)) ) {
      fprintf(stderr, "disk: failed to load pier at %s", pax_c);
      c3_free(log_u);
      return 0;
    }
  }

  //  create/load $pier/.urb
  //
  {
    c3_c* urb_c = c3_malloc(6 + strlen(pax_c));

    strcpy(urb_c, pax_c);
    strcat(urb_c, "/.urb");

    if ( 0 == (log_u->urb_u = u3_foil_folder(urb_c)) ) {
      fprintf(stderr, "disk: failed to load /.urb in %s", pax_c);
      c3_free(urb_c);
      c3_free(log_u);
      return 0;
    }
    c3_free(urb_c);
  }

  //  create/load $pier/.urb/put and $pier/.urb/get
  //
  {
    c3_c* dir_c = c3_malloc(10 + strlen(pax_c));

    strcpy(dir_c, pax_c);
    strcat(dir_c, "/.urb/put");
    mkdir(dir_c, 0700);

    strcpy(dir_c, pax_c);
    strcat(dir_c, "/.urb/get");
    mkdir(dir_c, 0700);

    c3_free(dir_c);
  }

  //  create/load $pier/.urb/log, initialize db
  //
  {
    c3_c* log_c = c3_malloc(10 + strlen(pax_c));

    strcpy(log_c, pax_c);
    strcat(log_c, "/.urb/log");

    if ( 0 == (log_u->com_u = u3_foil_folder(log_c)) ) {
      fprintf(stderr, "disk: failed to load /.urb/log in %s", pax_c);
      c3_free(log_c);
      c3_free(log_u);
      return 0;
    }

    if ( 0 == (log_u->db_u = u3_lmdb_init(log_c)) ) {
      fprintf(stderr, "disk: failed to initialize database");
      c3_free(log_c);
      c3_free(log_u);
      return 0;
    }

    c3_free(log_c);
  }

  //  get the latest event number from the db
  //
  {
    log_u->dun_d = 0;

    if ( c3n == u3_lmdb_get_latest_event_number(log_u->db_u, &log_u->dun_d) ) {
      fprintf(stderr, "disk: failed to load latest event from database");
      c3_free(log_u);
      return 0;
    }

    log_u->sen_d = log_u->dun_d;
  }

  log_u->liv_o = c3y;

  return log_u;
}

c3_o
u3_disk_read_header(u3_disk* log_u, c3_d* who_d, c3_o* fak_o, c3_w* lif_w)
{
  u3_noun who, fak, lif;

  if ( c3n == u3_lmdb_read_identity(log_u->db_u, &who, &fak, &lif) ) {
    return c3n;
  }

  if (  (c3n == u3a_is_cat(lif))
     || !((c3y == fak ) || (c3n == fak )) )
  {
    u3z(who); u3z(fak); u3z(lif);
    return c3n;
  }

  if ( who_d ) {
    u3r_chubs(0, 2, who_d, who);
  }

  if ( fak_o ) {
    *fak_o = fak;
  }

  if ( lif_w ) {
    *lif_w = lif;
  }

  u3z(who);

  return c3y;
}

c3_o
u3_disk_write_header(u3_disk* log_u, c3_d who_d[2], c3_o fak_o, c3_w lif_w)
{
  c3_assert( c3y == u3a_is_cat(lif_w) );
  u3_noun who = u3i_chubs(2, who_d);
  return u3_lmdb_write_identity(log_u->db_u, who, fak_o, lif_w);
}


/* _disk_commit_done(): commit complete.
 */
static void
_disk_commit_done(c3_o ret_o, void* vod_p, c3_d eve_d, c3_d len_d)
{
  u3_disk* log_u = vod_p;

  if ( c3n == ret_o ) {
    log_u->cb_u.write_bail_f(log_u->cb_u.vod_p, eve_d + (len_d - 1ULL));

#ifdef VERBOSE_DISK
    if ( 1ULL == len_d ) {
      fprintf(stderr, "disk: (%" PRIu64 "): commit: failed\r\n", eve_d);
    }
    else {
      fprintf(stderr, "disk: (%" PRIu64 "-%" PRIu64 "): commit: failed\r\n",
                      eve_d,
                      eve_d + (len_d - 1ULL));
    }
#endif
  }
  else {
    log_u->dun_d = eve_d + (len_d - 1ULL);
    log_u->cb_u.write_done_f(log_u->cb_u.vod_p, log_u->dun_d);

#ifdef VERBOSE_DISK
    if ( 1ULL == len_d ) {
      fprintf(stderr, "disk: (%" PRIu64 "): commit: complete\r\n", eve_d);
    }
    else {
      fprintf(stderr, "disk: (%" PRIu64 "-%" PRIu64 "): commit: complete\r\n",
                      eve_d,
                      eve_d + (len_d - 1ULL));
    }
#endif
  }

  {
    u3_fact* tac_u = log_u->put_u.ext_u;
    
    while ( tac_u && (tac_u->eve_d <= log_u->dun_d) ) {
      log_u->put_u.ext_u = tac_u->nex_u;
      u3z(tac_u->job);
      c3_free(tac_u);
      tac_u = log_u->put_u.ext_u;
    }
  }

  if ( !log_u->put_u.ext_u ) {
    log_u->put_u.ent_u = 0;
  }

  log_u->hol_o = c3n;
  _disk_commit(log_u);
}

static void
_disk_commit(u3_disk* log_u)
{
  if (  (c3n == log_u->hol_o)
     && (log_u->sen_d > log_u->dun_d) )
  {
    c3_d len_d = log_u->sen_d - log_u->dun_d;
    u3_fact* tac_u = log_u->put_u.ext_u;

    c3_assert( (1ULL + log_u->dun_d) == tac_u->eve_d );
    c3_assert( log_u->sen_d == log_u->put_u.ent_u->eve_d );

    u3_db_batch* bat_u = c3_malloc(sizeof(*bat_u));
    bat_u->eve_d = tac_u->eve_d;
    bat_u->len_d = len_d;
    bat_u->byt_p = c3_malloc(len_d * sizeof(void*));
    bat_u->siz_i = c3_malloc(len_d * sizeof(size_t));

    for ( c3_d i_d = 0ULL; i_d < len_d; ++i_d) {
      c3_assert( (bat_u->eve_d + i_d) == tac_u->eve_d );

      u3_atom mat = u3ke_jam(u3nc(tac_u->bug_l, u3k(tac_u->job)));
      c3_w  len_w = u3r_met(3, mat);
      c3_y* dat_y = c3_malloc(len_w);
      u3r_bytes(0, len_w, dat_y, mat);

      bat_u->byt_p[i_d] = dat_y;
      bat_u->siz_i[i_d] = len_w;

      tac_u = tac_u->nex_u;
      u3z(mat);
    }

#ifdef VERBOSE_DISK
    if ( 1ULL == len_d ) {
      fprintf(stderr, "disk: (%" PRIu64 "): commit: request\r\n",
                      bat_u->eve_d);
    }
    else {
      fprintf(stderr, "disk: (%" PRIu64 "-%" PRIu64 "): commit: request\r\n",
                      bat_u->eve_d,
                      (bat_u->eve_d + len_d - 1ULL));
    }
#endif

    u3_lmdb_write_event(log_u->db_u, (u3_pier*)log_u,
                        (struct u3_lmdb_write_request*)bat_u,
                        (void(*)(c3_o, u3_pier*, c3_d, c3_d))_disk_commit_done);

    log_u->hol_o = c3y;
  }
}

/* u3_disk_plan():
*/
void
u3_disk_plan(u3_disk* log_u,
             c3_d     eve_d,
             c3_l     bug_l,
             c3_l     mug_l,
             u3_noun  job)
{
  u3_fact* tac_u = c3_malloc(sizeof(*tac_u));
  tac_u->bug_l = bug_l;
  tac_u->mug_l = mug_l;
  tac_u->eve_d = eve_d;
  tac_u->nex_u = 0;
  tac_u->job   = job;

  c3_assert( (1ULL + log_u->sen_d) == eve_d );
  log_u->sen_d++;
  
  if ( !log_u->put_u.ent_u ) {
    c3_assert( !log_u->put_u.ext_u );
    log_u->put_u.ent_u = log_u->put_u.ext_u = tac_u;
  }
  else {
    log_u->put_u.ent_u->nex_u = tac_u;
    log_u->put_u.ent_u = tac_u;
  }

  _disk_commit(log_u);
}

/* u3_disk_boot_plan():
*/
void
u3_disk_boot_plan(u3_disk* log_u, u3_noun job)
{
  u3_fact* tac_u = c3_malloc(sizeof(*tac_u));
  tac_u->mug_l = 0; //u3r_mug(job); XX
  tac_u->eve_d = ++log_u->sen_d;
  tac_u->nex_u = 0;
  tac_u->job   = job;

  if ( !log_u->put_u.ent_u ) {
    c3_assert( !log_u->put_u.ext_u );
    c3_assert( 1ULL == log_u->sen_d );

    tac_u->bug_l = 0;  // XX
    log_u->put_u.ent_u = log_u->put_u.ext_u = tac_u;
  }
  else {
    tac_u->bug_l = log_u->put_u.ent_u->mug_l;  // XX
    log_u->put_u.ent_u->nex_u = tac_u;
    log_u->put_u.ent_u = tac_u;
  }

#ifdef VERBOSE_DISK
  fprintf(stderr, "disk: (%" PRIu64 "): db boot plan\r\n", tac_u->eve_d);
#endif

  _disk_commit(log_u);
}

static void
_disk_read_done_cb(uv_timer_t* tim_u)
{
  struct _cd_read* red_u = tim_u->data;
  u3_disk* log_u = red_u->log_u;
  u3_play  pay_u = {
    .ent_u = red_u->ent_u,
    .ext_u = red_u->ext_u
  };

  c3_assert( red_u->ent_u );
  c3_assert( red_u->ext_u );

  log_u->cb_u.read_done_f(log_u->cb_u.vod_p, pay_u);
  c3_free(red_u);
}

static c3_o
_disk_read_one_cb(void* vod_p, c3_d eve_d, u3_atom mat)
{
  struct _cd_read* red_u = vod_p;
  u3_disk* log_u = red_u->log_u;
  u3_fact* tac_u = c3_calloc(sizeof(*tac_u));
  tac_u->eve_d = eve_d;

  {
    //  xx soft?
    //
    u3_noun dat = u3ke_cue(mat);
    u3_noun mug, job;

    if (  (c3n == u3r_cell(dat, &mug, &job))
       || (c3n == u3r_safe_word(mug, &tac_u->bug_l)) ) // XX
    {
      c3_free(tac_u);
      //  XX dispose get_u;
      log_u->cb_u.read_bail_f(log_u->cb_u.vod_p, eve_d);
      return c3n;
    }

    tac_u->job = u3k(job);
    u3z(dat);
  }

  if ( !red_u->ent_u ) {
    c3_assert( !red_u->ext_u );

    c3_assert( red_u->eve_d == eve_d );
    // tac_u->mug_l = 0;  // XX
    red_u->ent_u = red_u->ext_u = tac_u;
  }
  else {
    c3_assert( (1ULL + red_u->ent_u->eve_d) == eve_d );
    // log_u->get_u.ent_u->mug_l = tac_u->bug_l; // XX
    red_u->ent_u->nex_u = tac_u;
    red_u->ent_u = tac_u;
  }

  return c3y;
}

static void
_disk_read_start_cb(uv_timer_t* tim_u)
{
  struct _cd_read* red_u = tim_u->data;
  u3_disk* log_u = red_u->log_u;

  uv_timer_start(&log_u->tim_u, _disk_read_done_cb, 0, 0);

  if ( c3n == u3_lmdb_read_events(log_u->db_u,
                                  red_u->eve_d,
                                  red_u->len_d,
                                  red_u,
                                  _disk_read_one_cb) )
  {
    log_u->cb_u.read_bail_f(log_u->cb_u.vod_p, red_u->eve_d);
  }
}

void
u3_disk_read(u3_disk* log_u, c3_d eve_d, c3_d len_d)
{
  struct _cd_read* red_u = c3_malloc(sizeof(*red_u));
  red_u->log_u = log_u;
  red_u->eve_d = eve_d;
  red_u->len_d = len_d;
  red_u->ent_u = red_u->ext_u = 0;

  log_u->tim_u.data = red_u;  
  uv_timer_start(&log_u->tim_u, _disk_read_start_cb, 0, 0);
}

/* u3_disk_exit(): close the log.
*/
void
u3_disk_exit(u3_disk* log_u)
{
  u3_lmdb_shutdown(log_u->db_u);
  //  XX dispose
  //
}
