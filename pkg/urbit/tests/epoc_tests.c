//! @file epoc_tests.c
//!
//! Tests for the epoc module.
//! @note `#define U3_EPOC_TEST` must be included at the top of `epoc.h` in
//!       order to use these tests. If this step is omitted, the tests will
//!       segfault because the `epoc` module relies on the snapshotting system
//!       by default, which is too complex to set up for these tests.

#include "vere/epoc.h"

#include <sys/stat.h>

#include "c/defs.h"
#include "c/path.h"

#ifdef U3_EPOC_TEST

static const c3_c dir_c[] = "/tmp/event_log";

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
    c3_assert(0 == strcmp(pax_c, u3_epoc_path_str(poc_u)));
    u3_epoc_close(poc_u);
    c3_free(poc_u);
    c3_path_free(pax_u);
  }
}

static void
test_sync_mixed(void)
{
  c3_path* const    par_u = c3_path_fv(3, "/", "tmp", "event_log");
  static const c3_d fir_d = 137;
  static c3_y       eve_c = 'a';
  static const c3_w lif_w = 5;
  c3_list* const    lis_u = c3_list_init();
  {
    // Create new epoch.
    c3_path* const pax_u = c3_path_fv(4, "/", "tmp", "event_log", "0i137");
    u3_epoc*       poc_u = u3_epoc_new(par_u, fir_d, lif_w);
    c3_assert(poc_u);
    c3_assert(u3_epoc_is_empty(poc_u));
    c3_assert(0 == u3_epoc_len(poc_u));
    c3_assert(c3_path_eq(pax_u, u3_epoc_path(poc_u)));

    // Commit an event to the epoch and then close it.
    c3_list_pushb(lis_u, &eve_c, sizeof(eve_c));
    c3_assert(u3_epoc_commit(poc_u, c3_list_peekb(lis_u), 1));
    c3_free(c3_list_popb(lis_u));
    u3_epoc_close(poc_u);
    c3_free(poc_u);

    // Load recently closed epoch.
    poc_u = u3_epoc_open(pax_u, NULL);
    c3_assert(poc_u);
    c3_assert(!u3_epoc_is_empty(poc_u));
    c3_assert(1 == u3_epoc_len(poc_u));
    c3_assert(fir_d == u3_epoc_first_commit(poc_u));
    c3_assert(fir_d == u3_epoc_last_commit(poc_u));
    c3_assert(c3_path_eq(pax_u, u3_epoc_path(poc_u)));

    c3_path_free(pax_u);
  }

  {
    // Attempt to load epoch that does not exist.
    c3_path* const pax_u = c3_path_fv(4, "/", "tmp", "event_log", "0i138");
    u3_epoc*       poc_u = u3_epoc_open(pax_u, NULL);
    c3_assert(!poc_u);
    c3_path_free(pax_u);
  }

  {
    // Load existing epoch.
    c3_path* const pax_u = c3_path_fv(4, "/", "tmp", "event_log", "0i137");
    u3_epoc*       poc_u = u3_epoc_open(pax_u, NULL);
    c3_assert(poc_u);
    c3_assert(!u3_epoc_is_empty(poc_u));
    c3_assert(1 == u3_epoc_len(poc_u));
    c3_assert(fir_d == u3_epoc_first_commit(poc_u));
    c3_assert(fir_d == u3_epoc_last_commit(poc_u));
    c3_assert(c3_path_eq(pax_u, u3_epoc_path(poc_u)));

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
    poc_u = u3_epoc_open(pax_u, NULL);
    c3_assert(poc_u);
    c3_assert(cnt_i + 1 == u3_epoc_len(poc_u));
    c3_assert(fir_d == u3_epoc_first_commit(poc_u));
    c3_assert(fir_d + cnt_i == u3_epoc_last_commit(poc_u));
    c3_assert(c3_path_eq(pax_u, u3_epoc_path(poc_u)));

    // Iterate over the epoch's events.
    c3_assert(u3_epoc_iter_open(poc_u, fir_d));
    for ( size_t idx_i = 0; idx_i < u3_epoc_len(poc_u); idx_i++ ) {
      c3_y*  tmp_c;
      size_t len_i;
      c3_assert(u3_epoc_iter_step(poc_u, &tmp_c, &len_i));
      c3_assert('a' + idx_i == *tmp_c);
      c3_assert(sizeof(c3_y) == len_i);
    }
    u3_epoc_iter_close(poc_u);
    u3_epoc_close(poc_u);
    c3_free(poc_u);

    unlink(c3_path_str(pax_u));
    c3_path_free(pax_u);
  }
  unlink(c3_path_str(par_u));
  c3_path_free(par_u);
}
#endif /* ifdef U3_EPOC_TEST */

int
main(int argc, char* argv[])
{
#ifdef U3_EPOC_TEST
  mkdir(dir_c, 0777);
  test_u3_epoc_new();
  test_sync_mixed();
  // There's no straightforward way to recursively remove a directory tree from
  // C, so `rm -rf <dir_c>` will need to be run from the command line to remove
  // the artifacts from a test run.
#endif /* ifdef U3_EPOC_TEST */

  fprintf(stderr, "epoc_tests: ok\r\n");
  return 0;
}
