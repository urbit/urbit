#include "all.h"
#include "ur/hashcons.h"

static c3_i
_test_jam(const c3_c* cap_c,
          ur_root_t*  rot_u,
          ur_nref       ref,
          c3_w        len_w,
          const c3_y* res_y)
{
  c3_d  i_d, len_d;
  c3_y* out_y;
  c3_i  ret_i;

  ur_jam(rot_u, ref, &len_d, &out_y);

  if ( 0 != memcmp(out_y, res_y, len_w) ) {
    fprintf(stderr, "\033[31m%s fail\033[0m\r\n", cap_c);

    fprintf(stderr, "  actual: { ");
    for ( i_d = 0; i_d < len_d; i_d++ ) {
      fprintf(stderr, "0x%x, ", out_y[i_d]);
    }
    fprintf(stderr, "}\r\n");
    fprintf(stderr, "  expect: { ");
    for ( i_d = 0; i_d < len_w; i_d++ ) {
      fprintf(stderr, "0x%x, ", res_y[i_d]);
    }
    fprintf(stderr, "}\r\n");

    ret_i = 0;
  }
  else {
    ret_i = 1;
  }

  c3_free(out_y);

  return ret_i;
}

static c3_i
_test_ur(void)
{
  ur_root_t* rot_u = ur_hcon_init();
  c3_d  i_d, len_d;
  c3_y* byt_y;
  c3_i  res_i = 1;

#     define nc(a, b)     ur_cons(rot_u, a, b)
#     define nt(a, b, c)  nc(a, nc(b, c))

  {
    c3_c* cap_c    = "jam 0";
    c3_y  res_y[1] = { 0x2 };
    ur_nref ref    = 0;
    res_i &= _test_jam(cap_c, rot_u, ref, sizeof(res_y), res_y);
  }

  {
    c3_c* cap_c    = "jam 1";
    c3_y  res_y[1] = { 0xc };
    ur_nref ref    = 1;
    res_i &= _test_jam(cap_c, rot_u, ref, sizeof(res_y), res_y);
  }
  
  {
    c3_c* cap_c    = "jam 2";
    c3_y  res_y[1] = { 0x48 };
    ur_nref ref    = 2;
    res_i &= _test_jam(cap_c, rot_u, ref, sizeof(res_y), res_y);
  }

  {
    c3_c*  cap_c   = "jam %fast";
    c3_y  res_y[6] = { 0xc0, 0x37, 0xb, 0x9b, 0xa3, 0x3 };
    ur_nref  ref   = c3__fast;
    res_i &= _test_jam(cap_c, rot_u, ref, sizeof(res_y), res_y);
  }

  {
    c3_c* cap_c    = "jam %full";
    c3_y  res_y[6] = { 0xc0, 0x37, 0xab, 0x63, 0x63, 0x3 };
    ur_nref ref    = c3__full;
    res_i &= _test_jam(cap_c, rot_u, ref, sizeof(res_y), res_y);
  }

  {
    c3_c* cap_c    = "jam [0 0]";
    c3_y  res_y[1] = { 0x29 };
    ur_nref ref    = nc(0, 0);
    res_i &= _test_jam(cap_c, rot_u, ref, sizeof(res_y), res_y);
  }

  {
    c3_c* cap_c    = "jam [1 1]";
    c3_y  res_y[2] = { 0x31, 0x3 };
    ur_nref ref    = nc(1, 1);
    res_i &= _test_jam(cap_c, rot_u, ref, sizeof(res_y), res_y);
  }

  {
    c3_c* cap_c    = "jam [2 3]";
    c3_y  res_y[2] = { 0x21, 0xd1 };
    ur_nref ref    = nc(2, 3);
    res_i &= _test_jam(cap_c, rot_u, ref, sizeof(res_y), res_y);
  }

  {
    c3_c* cap_c     = "jam [%fast %full]";
    c3_y  res_y[11] = { 0x1, 0xdf, 0x2c, 0x6c, 0x8e, 0xe, 0x7c, 0xb3, 0x3a, 0x36, 0x36 }; 
    ur_nref ref     = nc(c3__fast, c3__full);
    res_i &= _test_jam(cap_c, rot_u, ref, sizeof(res_y), res_y);
  }

  {
    c3_c* cap_c    = "jam [1 1 1]";
    c3_y  res_y[2] = {  0x71, 0xcc };
    ur_nref ref    = nc(1, nc(1, 1));
    res_i &= _test_jam(cap_c, rot_u, ref, sizeof(res_y), res_y);
  }

  {
    c3_c* cap_c     = "jam [%fast %full %fast]";
    c3_y  res_y[12] = { 0x1, 0xdf, 0x2c, 0x6c, 0x8e, 0x1e, 0xf0, 0xcd, 0xea, 0xd8, 0xd8, 0x93 };
    ur_nref ref     = nc(c3__fast, nc(c3__full, c3__fast));
    res_i &= _test_jam(cap_c, rot_u, ref, sizeof(res_y), res_y);
  }

  {
    c3_c* cap_c    = "jam [[0 0] [[0 0] 1 1] 1 1]";
    c3_y  res_y[6] = { 0xa5, 0x35, 0x19, 0xf3, 0x18, 0x5 };
    ur_nref ref    = nc(nc(0, 0), nc(nc(nc(0, 0), nc(1, 1)), nc(1, 1)));
    res_i &= _test_jam(cap_c, rot_u, ref, sizeof(res_y), res_y);
  }

  {
    c3_c* cap_c    = "jam big";
    c3_y  res_y[14] = { 0x15, 0x17, 0xb2, 0xd0, 0x85, 0x59, 0xb8, 0x61, 0x87, 0x5f, 0x10, 0x54, 0x55, 0x5 };
    ur_nref ref    = nc(nc(nc(1, nc(nc(2, nc(nc(3, nc(nc(4, nc(nt(5, 6, nc(7, nc(nc(8, 0), 0))), 0)), 0)), 0)), 0)), 0), 0);
    res_i &= _test_jam(cap_c, rot_u, ref, sizeof(res_y), res_y);
  }

  return res_i;
}

int
main(int argc, char* argv[])
{
  if ( !_test_ur() ) {
    fprintf(stderr, "ur test failed\r\n");
    return 1;
  }

  fprintf(stderr, "ur ok\n");
  return 0;
}
