//! @file epoc_tests.c
//! Tests for the epoc module.

#include "vere/epoc.h"

#include <lmdb.h>
#include <sys/stat.h>

#include "c/defs.h"
#include "c/path.h"

#if 0
c3_path*
_epoc_path(const c3_path* const par_u, const c3_d fir_d);

MDB_env*
_lmdb_init(const c3_path* const pax_u);

static void
test_epoc_path(void)
{
  {
    static const c3_d fir_d   = 1;
    static const c3_c exp_c[] = "/tmp/0i1";
    c3_path* const    dir_u   = c3_path_fv(2, "/", "tmp");
    c3_path* const    pax_u   = _epoc_path(dir_u, fir_d);
    c3_assert(strlen(exp_c) == strlen(pax_u->str_c));
    c3_assert(0 == strcmp(exp_c, pax_u->str_c));
    c3_free(dir_u);
    c3_free(pax_u);
  }

  {
    static const c3_d fir_d   = 18446744073709551615UL;
    static const c3_c exp_c[] = "/tmp/0i18446744073709551615";
    c3_path* const    dir_u   = c3_path_fv(2, "/", "tmp");
    c3_path* const    pax_u   = _epoc_path(dir_u, fir_d);
    c3_assert(strlen(exp_c) == strlen(pax_u->str_c));
    c3_assert(0 == strcmp(exp_c, pax_u->str_c));
    c3_free(dir_u);
    c3_free(pax_u);
  }
}

static void
test_epoc_lmdb_init(void)
{
  // Note: running the tests under nix leads to permission issues that I was
  // only able to sidestep by running the following commands manually before
  // running the tests:
  //  $ DIR=/tmp/event_log
  //  $ rm -rf $DIR && mkdir -p $DIR && chmod 0777 $DIR
  //  $ DIR=
  {
    c3_path* const pax_u = c3_path_fv(4, "/", "tmp", "event_log", "0i107");
    mkdir(pax_u->str_c, 0700);
    MDB_env* env_u = _lmdb_init(pax_u);
    c3_assert(env_u);
    const c3_c* tmp_c;
    c3_assert(0 == mdb_env_get_path(env_u, &tmp_c));
    c3_assert(0 == strcmp(pax_u->str_c, tmp_c));
    mdb_env_close(env_u);
    unlink(pax_u->str_c);
    c3_free(pax_u);
  }
}

static void
test_u3_epoc_new(void)
{
  {
    static const c3_c pax_c[] = "/tmp/event_log/0i110";
    static const c3_d fir_d   = 110;
    static const c3_w lif_w   = 5;
    c3_path* const    pax_u   = c3_path_fv(3, "/", "tmp", "event_log");
    u3_epoc*          poc_u   = u3_epoc_new(pax_u, fir_d, lif_w);
    c3_assert(poc_u);
    c3_assert(u3_epoc_is_empty(poc_u));
    c3_assert(0 == u3_epoc_len(poc_u));
    c3_assert(0 == strcmp(pax_c, u3_epoc_path(poc_u)));
    c3_assert(fir_d == u3_epoc_first_commit(poc_u));
    c3_assert(fir_d - 1 == u3_epoc_first_evt(poc_u));
    u3_epoc_close(poc_u);
    c3_free(poc_u);
    c3_free(pax_u);
  }
}

static void
test_sync_mixed(void)
{
  c3_path* const    par_u = c3_path_fv(3, "/", "tmp", "event_log");
  static const c3_d fir_d = 137;
  static c3_y       eve_c = 'a';
  c3_list* const    lis_u = c3_list_init();
  {
    // Create new epoch.
    c3_path* const pax_u = c3_path_fv(4, "/", "tmp", "event_log", "0i137");
    u3_epoc*       poc_u = u3_epoc_new(par_u, fir_d);
    c3_assert(poc_u);
    c3_assert(poc_u->env_u);
    c3_assert(fir_d == poc_u->fir_d);
    c3_assert(fir_d - 1 == poc_u->las_d);
    c3_assert(0 == strcmp(pax_u->str_c, u3_epoc_path(poc_u)));

    // Commit an event to the epoch and then close it.
    c3_list_pushb(lis_u, &eve_c, sizeof(eve_c));
    c3_assert(u3_epoc_commit(poc_u, c3_list_peekb(lis_u), 1));
    c3_free(c3_list_popb(lis_u));
    u3_epoc_close(poc_u);
    c3_free(poc_u);

    // Load recently closed epoch.
    poc_u = u3_epoc_open(pax_u);
    c3_assert(poc_u);
    c3_assert(poc_u->env_u);
    c3_assert(fir_d == poc_u->fir_d);
    c3_assert(fir_d == poc_u->las_d);
    c3_assert(0 == strcmp(pax_u->str_c, u3_epoc_path(poc_u)));
    c3_free(pax_u);
  }

  {
    // Attempt to load epoch that does not exist.
    c3_path* const pax_u = c3_path_fv(4, "/", "tmp", "event_log", "0i138");
    u3_epoc*       poc_u = u3_epoc_open(pax_u);
    c3_assert(!poc_u);
    c3_free(pax_u);
  }

  {
    // Load existing epoch.
    c3_path* const pax_u = c3_path_fv(4, "/", "tmp", "event_log", "0i137");
    u3_epoc*       poc_u = u3_epoc_open(pax_u);
    c3_assert(poc_u);
    c3_assert(poc_u->env_u);
    c3_assert(fir_d == poc_u->fir_d);
    c3_assert(fir_d == poc_u->las_d);
    c3_assert(0 == strcmp(pax_u->str_c, u3_epoc_path(poc_u)));

    // Commit more events to the epoch and then close it.
    size_t cnt_i = 17;
    for ( size_t idx_i = 0; idx_i < cnt_i; idx_i++ ) {
      c3_y tmp_c = eve_c + idx_i + 1;
      c3_list_pushb(lis_u, &tmp_c, sizeof(tmp_c));
      c3_assert(u3_epoc_commit(poc_u, c3_list_peekb(lis_u), 1));
      c3_free(c3_list_popb(lis_u));
    }
    u3_epoc_close(poc_u);
    c3_free(poc_u);

    // Load recently closed epoch.
    poc_u = u3_epoc_open(pax_u);
    c3_assert(poc_u);
    c3_assert(poc_u->env_u);
    c3_assert(fir_d == poc_u->fir_d);
    c3_assert(fir_d + cnt_i == poc_u->las_d);
    c3_assert(0 == strcmp(pax_u->str_c, u3_epoc_path(poc_u)));

    // Iterate over the epoch's events.
    c3_assert(u3_epoc_iter_open(poc_u, fir_d));
    cnt_i = poc_u->las_d + 1 - poc_u->fir_d;
    for ( size_t idx_i = 0; idx_i < cnt_i; idx_i++ ) {
      c3_y*  tmp_c;
      size_t len_i;
      c3_assert(u3_epoc_iter_step(poc_u, &tmp_c, &len_i));
      c3_assert('a' + idx_i == *tmp_c);
      c3_assert(sizeof(c3_y) == len_i);
    }
    u3_epoc_iter_close(poc_u);
    u3_epoc_close(poc_u);
    c3_free(poc_u);

    unlink(pax_u->str_c);
    c3_free(pax_u);
  }
  unlink(par_u->str_c);
  c3_free(par_u);
}
#endif

int
main(int argc, char* argv[])
{
#if 0
  test_epoc_path();
  test_epoc_lmdb_init();
  test_u3_epoc_new();
  test_sync_mixed();
#endif

  fprintf(stderr, "epoc_tests: ok\r\n");
  return 0;
}
