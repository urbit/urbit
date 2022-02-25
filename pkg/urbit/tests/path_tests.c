//! @file path_tests.c

#include "c/path.h"

#include "c/defs.h"
#include "c/portable.h"
#include "c/types.h"

static void
_test_path_fv(void)
{
  // Empty path.
  {
    c3_path* pax_u;
    c3_assert(pax_u = c3_path_fv(0));
    c3_assert(0 == strcmp("", pax_u->str_c));
    c3_free(pax_u);
  }

  // Valid relative canonical path with one component.
  {
    static const c3_c* const exp_c = "fanfun-mocbud";
    c3_path*                 pax_u;
    c3_assert(pax_u = c3_path_fv(1, "fanfun-mocbud"));
    c3_assert(0 == strcmp(exp_c, pax_u->str_c));
    c3_free(pax_u);
  }

  // Valid absolute canonical path with one component.
  {
    static const c3_c* const exp_c = "/";
    c3_path*                 pax_u;
    c3_assert(pax_u = c3_path_fv(1, "/"));
    c3_assert(0 == strcmp(exp_c, pax_u->str_c));
    c3_free(pax_u);
  }

  // Valid relative canonical path with two components.
  {
    static const c3_c* const exp_c = "~/master-morzod";
    c3_path*                 pax_u;
    c3_assert(pax_u = c3_path_fv(2, "~", "master-morzod"));
    c3_assert(0 == strcmp(exp_c, pax_u->str_c));
    c3_free(pax_u);
  }

  // Valid absolute canonical path with two components.
  {
    static const c3_c* const exp_c = "/silsyn-wathep";
    c3_path*                 pax_u;
    c3_assert(pax_u = c3_path_fv(2, "/", "silsyn-wathep"));
    c3_assert(0 == strcmp(exp_c, pax_u->str_c));
    c3_free(pax_u);
  }

  // Valid relative canonical path with more than two components.
  {
    static const c3_c* const exp_c
      = "a/really/long/relative/path/with_underscores/and-dashes";
    c3_path* pax_u;
    c3_assert(pax_u = c3_path_fv(7,
                                 "a",
                                 "really",
                                 "long",
                                 "relative",
                                 "path",
                                 "with_underscores",
                                 "and-dashes"));
    c3_assert(0 == strcmp(exp_c, pax_u->str_c));
    c3_free(pax_u);
  }

  // Valid absolute canonical path with more than two components.
  {
    static const c3_c* const exp_c
      = "/a/really/long/ABSOLUTE/path/with_underscores/and-dashes";
    c3_path* pax_u;
    c3_assert(pax_u = c3_path_fv(8,
                                 "/",
                                 "a",
                                 "really",
                                 "long",
                                 "ABSOLUTE",
                                 "path",
                                 "with_underscores",
                                 "and-dashes"));
    c3_assert(0 == strcmp(exp_c, pax_u->str_c));
    c3_free(pax_u);
  }

  // Valid absolute non-canonical path with three components.
  {
    static const c3_c* const exp_c = "/";
    c3_path*                 pax_u;
    c3_assert(pax_u = c3_path_fv(3, "/", "tmp", ".."));
    c3_assert(0 == strcmp(exp_c, pax_u->str_c));
    c3_free(pax_u);
  }

  // Valid relative non-canonical path with five components.
  {
    static const c3_c* const exp_c = "fanfun-mocbud";
    c3_path*                 pax_u;
    c3_assert(pax_u
              = c3_path_fv(5, "fanfun-mocbud", ".urb", "log", "..", ".."));
    c3_assert(0 == strcmp(exp_c, pax_u->str_c));
    c3_free(pax_u);
  }

  // Valid absolute non-canonical path with two components.
  {
    static const c3_c* const exp_c = "/";
    c3_path*                 pax_u;
    c3_assert(pax_u = c3_path_fv(2, "/", ".."));
    c3_assert(0 == strcmp(exp_c, pax_u->str_c));
    c3_free(pax_u);
  }
}

static void
_test_path_push_pop(void)
{
  // Absolute path.
  {
    c3_path* pax_u;
    c3_assert(pax_u = c3_path_fv(1, "/"));
    c3_assert(0 == strcmp("/", pax_u->str_c));

    c3_path_push(pax_u, "fanfun");
    c3_assert(0 == strcmp("/fanfun", pax_u->str_c));

    c3_path_push(pax_u, "mocbud");
    c3_assert(0 == strcmp("/fanfun/mocbud", pax_u->str_c));

    c3_path_push(pax_u, "some-moon-name");
    c3_assert(0 == strcmp("/fanfun/mocbud/some-moon-name", pax_u->str_c));

    c3_path_pop(pax_u);
    c3_assert(0 == strcmp("/fanfun/mocbud", pax_u->str_c));

    c3_path_pop(pax_u);
    c3_assert(0 == strcmp("/fanfun", pax_u->str_c));

    c3_path_pop(pax_u);
    c3_assert(0 == strcmp("/", pax_u->str_c));

    c3_path_pop(pax_u);
    c3_assert(0 == strcmp("", pax_u->str_c));
    c3_assert(0 == pax_u->len_i);

    c3_path_pop(pax_u);
    c3_path_pop(pax_u);
    c3_path_pop(pax_u);

    c3_free(pax_u);
  }

  // Push special components onto relative path.
  {
    c3_path* pax_u;
    c3_assert(pax_u = c3_path_fv(2, "~", "fanfun-mocbud"));

    c3_path_push(pax_u, "its-snack-time");
    c3_assert(0 == strcmp("~/fanfun-mocbud/its-snack-time", pax_u->str_c));

    c3_path_push(pax_u, ".");
    c3_assert(0 == strcmp("~/fanfun-mocbud/its-snack-time", pax_u->str_c));

    c3_path_push(pax_u, "");
    c3_assert(0 == strcmp("~/fanfun-mocbud/its-snack-time", pax_u->str_c));

    c3_path_push(pax_u, NULL);
    c3_assert(0 == strcmp("~/fanfun-mocbud/its-snack-time", pax_u->str_c));

    c3_path_push(pax_u, "..");
    c3_assert(0 == strcmp("~/fanfun-mocbud", pax_u->str_c));

    c3_path_push(pax_u, "..");
    c3_assert(0 == strcmp("~", pax_u->str_c));

    c3_path_push(pax_u, "..");
    c3_assert(0 == strcmp("", pax_u->str_c));

    c3_path_push(pax_u, "/");
    c3_assert(0 == strcmp("/", pax_u->str_c));

    c3_path_push(pax_u, "..");
    c3_assert(0 == strcmp("/", pax_u->str_c));

    c3_free(pax_u);
  }

  // Start with empty path.
  {
    c3_path* pax_u;
    c3_assert(pax_u = c3_path_fv(0));

    c3_path_push(pax_u, "a");
    c3_assert(0 == strcmp("a", pax_u->str_c));

    c3_path_push(pax_u, "b");
    c3_assert(0 == strcmp("a/b", pax_u->str_c));

    c3_path_push(pax_u, "c");
    c3_assert(0 == strcmp("a/b/c", pax_u->str_c));

    c3_path_push(pax_u, "d");
    c3_assert(0 == strcmp("a/b/c/d", pax_u->str_c));

    c3_path_push(pax_u, "e");
    c3_assert(0 == strcmp("a/b/c/d/e", pax_u->str_c));

    c3_path_push(pax_u, "f");
    c3_assert(0 == strcmp("a/b/c/d/e/f", pax_u->str_c));

    c3_path_pop(pax_u);
    c3_path_pop(pax_u);
    c3_path_pop(pax_u);
    c3_path_pop(pax_u);
    c3_path_pop(pax_u);
    c3_path_pop(pax_u);

    c3_assert(0 == strcmp("", pax_u->str_c));

    c3_free(pax_u);
  }
}

int
main(int argc, char* argv[])
{
  _test_path_fv();
  _test_path_push_pop();

  fprintf(stderr, "test_path: ok\r\n");

  return 0;
}
