/// @file prim_tests.c

#include "c/prim.h"

static void
_prim_put_get(void)
{
  { // Test string.
    c3_path* const path_u = c3_path_fv(3, "/", "tmp", "test.prim");

    c3_c* data_c = "Did you try switching it to wumbo?";
    c3_assert(c3_prim_put(path_u, c3_prim_str, &data_c));

    data_c = NULL;
    c3_assert(c3_prim_get(path_u, c3_prim_str, &data_c));
    c3_assert(strcmp(data_c, "Did you try switching it to wumbo?") == 0);

    c3_path_free(path_u);
  }

  { // Test unsigned 8-bit integer.
    c3_path* const path_u = c3_path_fv(3, "/", "tmp", "test.prim");

    c3_y data_y = 107;
    c3_assert(c3_prim_put(path_u, c3_prim_uint8, &data_y));

    data_y = 0;
    c3_assert(c3_prim_get(path_u, c3_prim_uint8, &data_y));
    c3_assert(data_y == 107);

    c3_path_free(path_u);
  }

  { // Test unsigned 16-bit integer.
    c3_path* const path_u = c3_path_fv(3, "/", "tmp", "test.prim");

    c3_s data_s = 10000;
    c3_assert(c3_prim_put(path_u, c3_prim_uint16, &data_s));

    data_s = 0;
    c3_assert(c3_prim_get(path_u, c3_prim_uint16, &data_s));
    c3_assert(data_s == 10000);

    c3_path_free(path_u);
  }

  { // Test unsigned 32-bit integer.
    c3_path* const path_u = c3_path_fv(3, "/", "tmp", "test.prim");

    c3_w data_w = 4000000000;
    c3_assert(c3_prim_put(path_u, c3_prim_uint32, &data_w));

    data_w = 0;
    c3_assert(c3_prim_get(path_u, c3_prim_uint32, &data_w));
    c3_assert(data_w == 4000000000);

    c3_path_free(path_u);
  }

  { // Test unsigned 64-bit integer.
    c3_path* const path_u = c3_path_fv(3, "/", "tmp", "test.prim");

    c3_d data_d = 18446744073709551615UL;
    c3_assert(c3_prim_put(path_u, c3_prim_uint64, &data_d));

    data_d = 0;
    c3_assert(c3_prim_get(path_u, c3_prim_uint64, &data_d));
    c3_assert(data_d == 18446744073709551615UL);

    c3_path_free(path_u);
  }

  { // Test signed 8-bit integer.
    c3_path* const path_u = c3_path_fv(3, "/", "tmp", "test.prim");

    c3_ys data_ys = -97;
    c3_assert(c3_prim_put(path_u, c3_prim_int8, &data_ys));

    data_ys = 0;
    c3_assert(c3_prim_get(path_u, c3_prim_int8, &data_ys));
    c3_assert(data_ys == -97);

    c3_path_free(path_u);
  }

  { // Test signed 16-bit integer.
    c3_path* const path_u = c3_path_fv(3, "/", "tmp", "test.prim");

    c3_ss data_ss = -1223;
    c3_assert(c3_prim_put(path_u, c3_prim_int16, &data_ss));

    data_ss = 0;
    c3_assert(c3_prim_get(path_u, c3_prim_int16, &data_ss));
    c3_assert(data_ss == -1223);

    c3_path_free(path_u);
  }

  { // Test signed 32-bit integer.
    c3_path* const path_u = c3_path_fv(3, "/", "tmp", "test.prim");

    c3_ws data_ws = -70003;
    c3_assert(c3_prim_put(path_u, c3_prim_int32, &data_ws));

    data_ws = 0;
    c3_assert(c3_prim_get(path_u, c3_prim_int32, &data_ws));
    c3_assert(data_ws == -70003);

    c3_path_free(path_u);
  }

  { // Test signed 64-bit integer.
    c3_path* const path_u = c3_path_fv(3, "/", "tmp", "test.prim");

    c3_ds data_ds = -7123456789;
    c3_assert(c3_prim_put(path_u, c3_prim_int64, &data_ds));

    data_ds = 0;
    c3_assert(c3_prim_get(path_u, c3_prim_int64, &data_ds));
    c3_assert(data_ds == -7123456789);

    c3_path_free(path_u);
  }
}

int
main(int argc, char* argv[])
{
  _prim_put_get();

  fprintf(stderr, "test_prim: ok\r\n");

  return 0;
}
