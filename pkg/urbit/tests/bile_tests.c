//! @file bile_tests.c

#include "c/bile.h"

#include "c/defs.h"
#include "c/portable.h"
#include "c/types.h"

static inline void
_cleanup(c3_path* const pax_u, c3_bile* bil_u)
{
  c3_free(pax_u);
  unlink(bil_u->pax_u->str_c);
  c3_bile_close(bil_u);
  c3_free(bil_u);
}

static void
_test_bile_open(void)
{
  {
    c3_path* const pax_u = c3_path_fv(3, "/", "tmp", "giraffe.bin");
    c3_bile*       bil_u = c3_bile_open(pax_u);
    c3_assert(bil_u);
    c3_assert(0 == bil_u->len_i);
    c3_assert(c3_path_eq(pax_u, bil_u->pax_u));
    _cleanup(pax_u, bil_u);
  }
}

static void
_test_bile_put_get_raw(void)
{
  // Put once, get once.
  {
    c3_path* const pax_u = c3_path_fv(3, "/", "tmp", "elephant.bin");
    c3_bile*       bil_u = c3_bile_open(pax_u);
    c3_assert(bil_u);
    c3_assert(0 == bil_u->len_i);
    c3_assert(c3_path_eq(pax_u, bil_u->pax_u));

    static c3_c inn_c[] = "nile river";
    c3_assert(c3_bile_put_raw(bil_u, inn_c, sizeof(inn_c)));

    c3_assert(sizeof(inn_c) == bil_u->len_i);

    c3_c out_c[bil_u->len_i];
    c3_assert(c3_bile_get_raw(bil_u, out_c, sizeof(out_c)));
    c3_assert(sizeof(inn_c) == bil_u->len_i);
    c3_assert(0 == strcmp(inn_c, out_c));

    _cleanup(pax_u, bil_u);
  }

  // Put three times, get three times.
  {
    c3_path* const pax_u = c3_path_fv(3, "/", "tmp", "gazelle.bin");
    c3_bile*       bil_u = c3_bile_open(pax_u);
    c3_assert(bil_u);
    c3_assert(0 == bil_u->len_i);
    c3_assert(c3_path_eq(pax_u, bil_u->pax_u));

    static c3_c in1_c[] = "zambezi river";
    c3_assert(c3_bile_put_raw(bil_u, in1_c, sizeof(in1_c)));
    c3_assert(sizeof(in1_c) == bil_u->len_i);

    static c3_c in2_c[] = "lake victoria";
    c3_assert(c3_bile_put_raw(bil_u, in2_c, sizeof(in2_c)));
    c3_assert(sizeof(in1_c) + sizeof(in2_c) == bil_u->len_i);

    static c3_c in3_c[] = "great rift valley";
    c3_assert(c3_bile_put_raw(bil_u, in3_c, sizeof(in3_c)));
    c3_assert(sizeof(in1_c) + sizeof(in2_c) + sizeof(in3_c) == bil_u->len_i);

    c3_c ou1_c[sizeof(in1_c)];
    c3_assert(c3_bile_get_raw(bil_u, ou1_c, sizeof(ou1_c)));
    c3_assert(0 == strcmp(in1_c, ou1_c));

    c3_c ou2_c[sizeof(in2_c)];
    c3_assert(c3_bile_get_raw(bil_u, ou2_c, sizeof(ou2_c)));
    c3_assert(0 == strcmp(in2_c, ou2_c));

    c3_c ou3_c[sizeof(in3_c)];
    c3_assert(c3_bile_get_raw(bil_u, ou3_c, sizeof(ou3_c)));
    c3_assert(0 == strcmp(in3_c, ou3_c));

    _cleanup(pax_u, bil_u);
  }

  // Put once, close, get once.
  {
    c3_path* const pax_u = c3_path_fv(3, "/", "tmp", "hippo.bin");
    c3_bile*       bil_u = c3_bile_open(pax_u);
    c3_assert(bil_u);
    c3_assert(0 == bil_u->len_i);
    c3_assert(c3_path_eq(pax_u, bil_u->pax_u));

    static c3_c inn_c[] = "bandama river";
    c3_assert(c3_bile_put_raw(bil_u, inn_c, sizeof(inn_c)));

    c3_assert(sizeof(inn_c) == bil_u->len_i);

    c3_bile_close(bil_u);

    c3_assert(bil_u = c3_bile_open(pax_u));
    c3_assert(sizeof(inn_c) == bil_u->len_i);
    c3_assert(c3_path_eq(pax_u, bil_u->pax_u));

    c3_c out_c[sizeof(inn_c)];
    c3_assert(c3_bile_get_raw(bil_u, out_c, sizeof(out_c)));
    c3_assert(sizeof(inn_c) == bil_u->len_i);
    c3_assert(0 == strcmp(inn_c, out_c));

    _cleanup(pax_u, bil_u);
  }

  // Put once, get once, put once, get once.
  {
    c3_path* const pax_u = c3_path_fv(3, "/", "tmp", "crocodile.bin");
    c3_bile*       bil_u = c3_bile_open(pax_u);
    c3_assert(bil_u);
    c3_assert(0 == bil_u->len_i);
    c3_assert(c3_path_eq(pax_u, bil_u->pax_u));

    static c3_c in1_c[] = "okavango river";
    c3_assert(c3_bile_put_raw(bil_u, in1_c, sizeof(in1_c)));
    c3_assert(sizeof(in1_c) == bil_u->len_i);

    c3_c ou1_c[sizeof(in1_c)];
    c3_assert(c3_bile_get_raw(bil_u, ou1_c, sizeof(ou1_c)));
    c3_assert(0 == strcmp(in1_c, ou1_c));

    static c3_c in2_c[] = "volta river";
    c3_assert(c3_bile_put_raw(bil_u, in2_c, sizeof(in2_c)));
    c3_assert(sizeof(in1_c) + sizeof(in2_c) == bil_u->len_i);

    c3_assert(c3_bile_get_raw(bil_u, ou1_c, sizeof(ou1_c)));
    c3_assert(0 == strcmp(in1_c, ou1_c));

    c3_c ou2_c[sizeof(in2_c)];
    c3_assert(c3_bile_get_raw(bil_u, ou2_c, sizeof(ou2_c)));
    c3_assert(0 == strcmp(in2_c, ou2_c));

    _cleanup(pax_u, bil_u);
  }
}

int
main(int argc, char* argv[])
{
  _test_bile_open();
  _test_bile_put_get_raw();

  fprintf(stderr, "test_bile: ok\r\n");

  return 0;
}
