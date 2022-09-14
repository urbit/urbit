#include "all.h"

#define TRUE 1
#define FALSE 0

/* _setup(): prepare for tests.
*/
static void
_setup(void)
{
  u3m_init();
  u3m_pave(c3y);
}

/* _test_u3r_chop: "extract bit slices from atom"
*/
static c3_i
_test_u3r_chop()
{
  c3_i  ret_i = 1;
  c3_w  dst_w = 0;
  u3_atom src = 0b11011;

  //  bloq 0
  //
  {
    //  read 1 bit from pos=0 (far right)
    //
    dst_w = 0;
    u3r_chop(0, 0, 1, 0, &dst_w, src);
    if ( 0x1 != dst_w ) {
      fprintf(stderr, "test: u3r_chop: bloq 0, 0\r\n");
      ret_i = 0;
    }

    //  read 1 bit from pos=1
    //
    dst_w = 0;
    u3r_chop(0, 1, 1, 0, &dst_w, src);
    if ( 0x1 != dst_w ) {
      fprintf(stderr, "test: u3r_chop: bloq 0, 1\r\n");
      ret_i = 0;
    }

    //  read 1 bit from pos=2
    //
    dst_w = 0;
    u3r_chop(0, 2, 1, 0, &dst_w, src);
    if ( 0x0 != dst_w ) {
      fprintf(stderr, "test: u3r_chop: bloq 0, 2\r\n");
      ret_i = 0;
    }

    //  read 4 x 1 bit bloq from pos=0
    //
    dst_w = 0;
    u3r_chop(0, 0, 4, 0, &dst_w, src);
    if ( 0b1011 != dst_w ) {
      fprintf(stderr, "test: u3r_chop: bloq 0, 3\r\n");
      ret_i = 0;
    }

    //  read 4 x 1 bit bloq from pos=0 into offset 1
    //
    dst_w = 0;
    u3r_chop(0, 0, 4, 1, &dst_w, src);
    if ( 0b10110 != dst_w ) {
      fprintf(stderr, "test: u3r_chop: bloq 0, 4\r\n");
      ret_i = 0;
    }
  }

  //  bloq 1
  //
  {
    //  read 2 bit from pos=0 (far right)
    //
    dst_w = 0;
    u3r_chop(1, 0, 1, 0, &dst_w, src);
    if ( 0b11 != dst_w ) {
      fprintf(stderr, "test: u3r_chop: bloq 1, 0\r\n");
      ret_i = 0;
    }

    //  read 2 bit from pos=1
    //
    dst_w = 0;
    u3r_chop(1, 1, 1, 0, &dst_w, src);
    if ( 0b10 != dst_w ) {
      fprintf(stderr, "test: u3r_chop: bloq 1, 1\r\n");
      ret_i = 0;
    }

    // read 2 bit from pos=2 (2 bloq over)
    dst_w = 0;
    u3r_chop(1, 2, 1, 0, &dst_w, src);
    if ( 0b01 != dst_w ) {
      fprintf(stderr, "test: u3r_chop: bloq 1, 2\r\n");
      ret_i = 0;
    }
  }

  //  bloq 3
  {
    dst_w = 0;
    u3r_chop(3, 0, 1, 0, &dst_w, src);
    if ( 0b11011 != dst_w ) {
      fprintf(stderr, "test: u3r_chop: bloq 3, 0\r\n");
      ret_i = 0;
    }
  }

  //  read 1,8,16 bit bloqs from an indirect atom
  //
  {
    src = u3i_string("abcdefghij");

    //  1 bit pos=0 (far right)
    //
    dst_w = 0;
    u3r_chop(0, 0, 1, 0, &dst_w, src);
    if ( 0b1 != dst_w ) {
      fprintf(stderr, "test: u3r_chop: indirect 0\r\n");
      ret_i = 0;
    }

    //  8 bits pos=0
    //
    dst_w = 0;
    u3r_chop(0, 0, 8, 0, &dst_w, src);
    if ( 0b1100001 != dst_w ) {
      fprintf(stderr, "test: u3r_chop: indirect 1\r\n");
      ret_i = 0;
    }

    //  1 byte pos=0
    //
    dst_w = 0;
    u3r_chop(3, 0, 1, 0, &dst_w, src);
    if ( 0b1100001 != dst_w ) {
      fprintf(stderr, "test: u3r_chop: indirect 2\r\n");
      ret_i = 0;
    }

    //  1 short pos=0
    //
    dst_w = 0;
    u3r_chop(4, 0, 1, 0, &dst_w, src);
    if ( 0b0110001001100001 != dst_w ) {
      fprintf(stderr, "test: u3r_chop: indirect 3\r\n");
      ret_i = 0;
    }

    u3z(src);
  }

  // read lots of bits from a direct noun which holds 64 bits of data
  // makes sure that we handle top 32 / bottom 32 correctly
  {
    c3_y inp_y[8] = { 0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7 };
    src = u3i_bytes(8, inp_y);

    c3_w dst_w[2] = {0};
    u3r_chop(0, 0, 63, 0, dst_w, src);
    if ( (0x3020100 != dst_w[0]) || (0x7060504 != dst_w[1]) ) {
      fprintf(stderr, "test: u3r_chop: indirect 4\r\n");
      ret_i = 0;
    }

    u3z(src);
  }

  // as above (read lots of bits from a direct noun which holds 64 bits of data
  // makes sure that we handle top 32 / bottom 32 correctly)
  // but with a bit more nuance
  {
    c3_y inp_y[8] = { 0x0, 0x0, 0x0, 0xaa, 0xff, 0x0, 0x0, 0x0 };
    src = u3i_bytes(8, (c3_y*)inp_y);

    dst_w = 0;
    u3r_chop(0, 24, 16, 0, &dst_w, src);
    if ( 0b1111111110101010 != dst_w ) {
      fprintf(stderr, "test: u3r_chop: indirect 5\r\n");
      ret_i = 0;
    }

    u3z(src);
  }

  return ret_i;
}

/* _test_chop_slow(): "golden master" for chop tests (formerly u3r_chop())
*/
void
_test_chop_slow(c3_g    met_g,
                c3_w    fum_w,
                c3_w    wid_w,
                c3_w    tou_w,
                c3_w*   dst_w,
                c3_w    len_w,
                c3_w*   buf_w)
{
  c3_w  i_w;

  if ( met_g < 5 ) {
    c3_w san_w = (1 << met_g);
    c3_w mek_w = ((1 << san_w) - 1);
    c3_w baf_w = (fum_w << met_g);
    c3_w bat_w = (tou_w << met_g);

    // XX: efficiency: poor.  Iterate by words.
    //
    for ( i_w = 0; i_w < wid_w; i_w++ ) {
      c3_w waf_w = (baf_w >> 5);
      c3_g raf_g = (baf_w & 31);
      c3_w wat_w = (bat_w >> 5);
      c3_g rat_g = (bat_w & 31);
      c3_w hop_w;

      hop_w = (waf_w >= len_w) ? 0 : buf_w[waf_w];
      hop_w = (hop_w >> raf_g) & mek_w;

      dst_w[wat_w] ^= (hop_w << rat_g);

      baf_w += san_w;
      bat_w += san_w;
    }
  }
  else {
    c3_g hut_g = (met_g - 5);
    c3_w san_w = (1 << hut_g);
    c3_w j_w;

    for ( i_w = 0; i_w < wid_w; i_w++ ) {
      c3_w wuf_w = (fum_w + i_w) << hut_g;
      c3_w wut_w = (tou_w + i_w) << hut_g;

      for ( j_w = 0; j_w < san_w; j_w++ ) {
        dst_w[wut_w + j_w] ^=
            ((wuf_w + j_w) >= len_w)
              ? 0
              : buf_w[wuf_w + j_w];
      }
    }
  }
}

/* _test_chop_smol(): test permuations of chop from bloq 0-4
*/
static c3_i
_test_chop_smol(c3_c* cap_c, c3_y val_y)
{
  c3_i ret_i = 1;
  c3_g met_g;
  c3_w fum_w, wid_w, tou_w;
  c3_w len_w = 34;  //  (rsh [0 5] (mul 2 (mul 34 (bex 4))))
  c3_w src_w[len_w];
  c3_w   a_w[len_w];
  c3_w   b_w[len_w];

  memset(src_w, val_y, len_w << 2);

  for ( met_g = 0; met_g < 5; met_g++ ) {
    for ( fum_w = 0; fum_w <= len_w; fum_w++ ) {
      for ( wid_w = 0; wid_w <= len_w; wid_w++ ) {
        for ( tou_w = 0; tou_w <= len_w; tou_w++ ) {
          memset(a_w, 0, len_w << 2);
          memset(b_w, 0, len_w << 2);
          u3r_chop_words(met_g, fum_w, wid_w, tou_w, a_w, len_w, src_w);
          _test_chop_slow(met_g, fum_w, wid_w, tou_w, b_w, len_w, src_w);

          if ( 0 != memcmp(a_w, b_w, len_w << 2) ) {
            c3_g sif_g = 5 - met_g;
            c3_w mas_w = (1 << met_g) - 1;
            c3_w out_w = tou_w >> sif_g;
            c3_w max_w = out_w + !!(fum_w & mas_w)
                       + (wid_w >> sif_g) + !!(wid_w & mas_w);

            fprintf(stderr, "%s (0x%x): met_g=%u fum_w=%u wid_w=%u tou_w=%u\r\n",
                            cap_c, val_y,
                            met_g, fum_w, wid_w, tou_w);


            fprintf(stderr, "%u-%u: ", out_w, max_w - 1);
            for ( ; out_w < max_w; out_w++ ) {
              fprintf(stderr, "[0x%x 0x%x] ", a_w[out_w], b_w[out_w]);
            }
            fprintf(stderr, "\r\n");
          }
        }
      }
    }
  }

  return ret_i;
}

/* _test_chop_huge(): test permuations of chop from bloq 5+
*/
static c3_i
_test_chop_huge(c3_c* cap_c, c3_y val_y)
{
  c3_i ret_i = 1;
  c3_g met_g;
  c3_w fum_w, wid_w, tou_w;
  c3_w len_w = 192;  //   (rsh [0 5] (mul 2 (mul 3 (bex 10))))
  c3_w src_w[len_w];
  c3_w   a_w[len_w];
  c3_w   b_w[len_w];

  memset(src_w, val_y, len_w << 2);

  for ( met_g = 5; met_g <= 10; met_g++ ) {
    for ( fum_w = 0; fum_w <= 3; fum_w++ ) {
      for ( wid_w = 0; wid_w <= 2; wid_w++ ) {
        for ( tou_w = 0; tou_w <= 1; tou_w++ ) {
          memset(a_w, 0, len_w << 2);
          memset(b_w, 0, len_w << 2);
          u3r_chop_words(met_g, fum_w, wid_w, tou_w, a_w, len_w, src_w);
          _test_chop_slow(met_g, fum_w, wid_w, tou_w, b_w, len_w, src_w);

          if ( 0 != memcmp(a_w, b_w, len_w << 2) ) {
            c3_g sif_g = met_g - 5;
            c3_w mas_w = (1 << met_g) - 1;
            c3_w out_w = tou_w << sif_g;
            c3_w max_w = out_w + !!(fum_w & mas_w)
                       + (wid_w << sif_g) + !!(wid_w & mas_w);

            fprintf(stderr, "%s (0x%x): met_g=%u fum_w=%u wid_w=%u tou_w=%u\r\n",
                            cap_c, val_y,
                            met_g, fum_w, wid_w, tou_w);


            fprintf(stderr, "%u-%u: ", out_w, max_w - 1);
            for ( ; out_w < max_w; out_w++ ) {
              fprintf(stderr, "[0x%x 0x%x] ", a_w[out_w], b_w[out_w]);
            }
            fprintf(stderr, "\r\n");
          }
        }
      }
    }
  }

  return ret_i;
}

/* _test_u3r_chop(): bit slice XOR
*/
static c3_i
_test_chop()
{
  return _test_u3r_chop()
       & _test_chop_smol("chop smol zeros", 0x0)
       & _test_chop_smol("chop smol ones", 0xff)
       & _test_chop_smol("chop smol alt 1", 0xaa)
       & _test_chop_smol("chop smol alt 2", 0x55)
       & _test_chop_huge("chop huge zeros", 0x0)
       & _test_chop_huge("chop huge ones", 0xff)
       & _test_chop_huge("chop huge alt 1", 0xaa)
       & _test_chop_huge("chop huge alt 2", 0x55);
}

/* _util_rand_string(): dynamically allocated len_w random string
*/
static c3_y*
_util_rand_string(c3_w len_w)
{
  c3_c* choice_c =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
  c3_w choice_len_w = strlen(choice_c);

  c3_y* out_y = c3_malloc(len_w + 1);

  c3_w i_w;
  for (i_w = 0; i_w < len_w; i_w ++){
    out_y[i_w] = choice_c[ (c3_w) rand() % choice_len_w ];
  }
  out_y[i_w] = 0;

  return out_y;
}

/* _test_noun_bits_helper():
*/
static void
_test_noun_bits_helper(u3_noun a, int direct_o,
                                  int indirect_o,
                                  int indirect_atom_o,
                                  int cell_o)
{
#if 0
  printf("=========== %u\n", a);
  printf("    31 bit  %u\n", a & ( ((c3_w)1) << 31));
  printf("    30 bit  %u\n", a & ( ((c3_w)1) << 30));
  printf("    dir     %x\n", c3y == u3a_is_cat(a));
  printf("    ind     %x\n", c3y == u3a_is_dog(a));
  printf("  i cell    %x\n", c3y == u3a_is_cell(a));
  printf("  i atom    %x\n", c3y == u3a_is_pug(a));
#endif

  if ( direct_o != (c3y == u3a_is_cat(a)) ) {
    printf("*** _test_noun_bits_one() fail: u3a_is_direct\r\n");
  }

  if ( indirect_o != (c3y == u3a_is_dog(a)) ) {
    printf("*** fail-2 u3a_is_indirect %d\r\n", c3n == u3a_is_cat(a));
  }

  if ( cell_o != (c3y == u3a_is_cell(a)) ) {
    printf("*** fail-4 u3a_is_cell\r\n");
  }

  if ( indirect_atom_o != (c3y == u3a_is_pug(a)) ) {
    printf("*** fail-3 u3a_is_indirect_atom\r\n");
  }
}

/* _test_noun_bits_set(): allocate.h level 1a
*/
static void
_test_noun_bits_set()
{
  u3_noun a = 1;

  // flip indirect bit on
  a |= (1 << 31);
  if ( c3n == u3a_is_dog(a) ) {
    printf("*** fail-5a turn indirect bit on\r\n");
  }

  if ( c3y == u3a_is_cell(a) ) {
    printf("*** fail-5b turn indirect bit on\r\n");
  }

  if ( c3n == u3a_is_pug(a) ) {
    printf("*** fail-5c turn indirect bit on\r\n");
  }

  // flip all bits off
  a = u3a_to_off(a);

  if ( c3y == u3a_is_dog(a) ) {
    printf("*** fail-5d turn indirect bit off\r\n");
  }

  if ( c3y == u3a_is_cell(a) ) {
    printf("*** fail-5e turn indirect bit on\r\n");
  }

  if ( c3y == u3a_is_pug(a)) {
    printf("*** fail-5f turn indirect bit on\r\n");
  }

  // flip indirect & cell bit on
  a = u3a_to_pom(a);

  if ( c3n == u3a_is_dog(a) ) {
    printf("*** fail-5g turn indirect bit on\r\n");
  }

  if ( c3n == u3a_is_cell(a) ) {
    printf("*** fail-5h turn indirect bit on\r\n");
  }

  if ( c3y == u3a_is_pug(a) ) {
    printf("*** fail-5i turn indirect bit on\r\n");
  }
}

/* _test_noun_bits_read(): allocate.h level 1
*/
static void
_test_noun_bits_read()
{

  u3_noun a = (u3_noun)0x1;     // direct atom
  u3_noun b = u3a_to_pug(0x2);  // indirect atom
  u3_noun c = u3a_to_pom(0x3);  // indirect cell

                         // direct  indirect  indirect-atom  indirect-cell
                         //----------------------------------------
  _test_noun_bits_helper(a, TRUE,   FALSE,    FALSE,         FALSE);
  _test_noun_bits_helper(b, FALSE,  TRUE,     TRUE,          FALSE);
  _test_noun_bits_helper(c, FALSE,  TRUE,     FALSE,         TRUE);
}

/* _test_imprison(): test basic data into / out of nouns
** insert and retrieve bytes with u3i_bytes()/u3r_bytes()
*/
static void
_test_imprison()
{
  c3_c* input_c =  "abcdefghij";
  c3_w out_len_w = 300;
  c3_y * output_y = c3_malloc(out_len_w);
  u3_noun a;

  // size 1, direct
  a = u3i_bytes(1, (c3_y*)input_c);
  memset(output_y, 0, out_len_w);
  u3r_bytes(0, 1, output_y, a);
  if (0 != memcmp(output_y, "a", 1)) {
    printf("*** _test_imprison: fail-1\n");
  }

  // size 2, direct
  a = u3i_bytes(2, (c3_y*)input_c);
  memset(output_y, 0, out_len_w);
  u3r_bytes(0, 2, output_y, a);
  if (0 != memcmp(output_y, "ab", 2)) {
    printf("*** _test_imprison: fail-2\n");
  }

  // size 6, direct (taken from an actual issue)
  {
    c3_y data_y[] = { 0x1, 0x1f, 0x8e, 0x2d, 0x2c, 0x2f };
    a = u3i_bytes(6, data_y);
    memset(output_y, 0, out_len_w);
    u3r_bytes(0, 6, output_y, a);
    int ret;
    ret = memcmp(output_y, data_y, 6);
    if (0 != ret) {
      printf("*** _test_imprison: fail-2.5 %x\n", ret);
      printf("    %x %x %x %x %x %x\n", output_y[0],
                                        output_y[1],
                                        output_y[2],
                                        output_y[3],
                                        output_y[4],
                                        output_y[5]);
    }
  }

  // size 8, direct
  a = u3i_bytes(8, (c3_y*)input_c);
  memset(output_y, 0, out_len_w);
  u3r_bytes(0, 8, output_y, a);
  if (0 != memcmp(output_y, "abcdefgh", 8)) {
    printf("*** _test_imprison: fail-3\n");
  }

  // size 10, indirect
  a = u3i_bytes(10, (c3_y*)input_c);
  memset(output_y, 0, out_len_w);
  u3r_bytes(0, 10, output_y, a);
  if (0 != memcmp(output_y, "abcdefghij", 10)) {
    printf("*** _test_imprison: fail-4\n");
  }

  // size 200, indirect
  c3_y * rand_y = _util_rand_string(200);
  a = u3i_bytes(200, rand_y);
  memset(output_y, 0, out_len_w);
  u3r_bytes(0, 200, output_y, a);
  if (0 != memcmp(output_y, rand_y, 200)) {
    printf("*** _test_imprison: fail-5\n");
  }

  c3_free(rand_y);
  c3_free(output_y);
}

/* _test_cells(): build and inspect cells: u3i_cell(), u3h(), u3t()
*/
static void
_test_cells()
{
  // very simple cell
  {
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = u3i_cell(a, b);

    u3_noun a2 = u3h(c);
    if (a2 != a){
      printf("*** _test_cells: fail-1\n");
    }

    u3_noun b2 = u3t(c);
    if (b2 != b){
      printf("*** _test_cells: fail-2\n");
    }
  }

  // very simple cell with indirect atoms
  {
    c3_w out_len_w = 200;
    c3_y * rand_a = _util_rand_string(out_len_w);
    c3_y * rand_b = _util_rand_string(out_len_w);

    u3_noun a = u3i_bytes(200, rand_a);
    u3_noun b = u3i_bytes(200, rand_b);
    u3_noun c = u3i_cell(a, b);

#if 0
    printf("a = %x\n", a);
    printf("b = %x\n", b);
    printf("c = %x\n", c);
    printf("a_rand = %s\n", rand_a);
    printf("b_rand = %s\n", rand_b);
#endif

    u3_noun a2 = u3h(c);
    c3_y * output_y = c3_malloc(out_len_w + 1);
    memset(output_y, 0, out_len_w + 1);
    u3r_bytes(0, out_len_w, output_y, a);

    if (0 != memcmp(output_y, rand_a, out_len_w)) {
      printf("*** _test_imprison: fail-3\n");
    }

    u3_noun b2 = u3h(c);
    memset(output_y, 0, out_len_w + 1);
    u3r_bytes(0, out_len_w, output_y, b);

    if (0 != memcmp(output_y, rand_b, out_len_w)) {
      printf("*** _test_imprison: fail-4\n");
    }

    c3_free(output_y);
    c3_free(rand_a);
    c3_free(rand_b);
  }

  // medium complicated cell
  //    q
  //   / \
  //  a1  r
  //     / \
  //    b2  s
  //       / \
  //      c3  d4
  {
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = (u3_noun) 0x3;
    u3_noun d = (u3_noun) 0x4;

    u3_noun s = u3i_cell(c, d);
    u3_noun r = u3i_cell(b, s);
    u3_noun q = u3i_cell(a, r);

    u3_noun a2 = u3h(q);
    u3_noun r2 = u3t(q);
    if (a2 != a){
      printf("*** _test_cells: complicated a\n");
    }

    u3_noun b2 = u3h(r2);
    u3_noun s2 = u3t(r2);
    if (b2 != b){
      printf("*** _test_cells: complicated b\n");
    }


    u3_noun c2 = u3h(s2);
    u3_noun d2 = u3t(s2);
    if (c2 != c){
      printf("*** _test_cells: complicated c\n");
    }

    if (d2 != d){
      printf("*** _test_cells: complicated d\n");
    }

    a2 = 0;
    u3r_mean(q, 2, &a2, 0);
    if (a2 != a){
      printf("*** _test_cells: complicated (via u3r_mean) a\n");
    }
  }

  // trel
  {
    //     q
    //    / \
    //   a   ?
    //      / \
    //     b   c

    // Produce the triple `[a b c]`.
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = (u3_noun) 0x3;

    u3_noun trel = u3i_trel(a, b, c);

    u3_noun a2 = u3h(trel);
    u3_noun b2 = u3h(u3t(trel));
    u3_noun c2 = u3t(u3t(trel));

    if (a2 != a){
      printf("*** trel: 1 a\n");
    }
    if (b2 != b){
      printf("*** trel: 2 a\n");
    }
    if (c2 != c){
      printf("*** trel: 3 a\n");
    }
  }

  // qual
  {
    //     q
    //    / \
    //   a   ?
    //      / \
    //     b   ?
    //        /  \
    //       c   d
    //

    // Produce the triple `[a b c]`.
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = (u3_noun) 0x3;
    u3_noun d = (u3_noun) 0x4;

    u3_noun qual = u3i_qual(a, b, c, d);

    u3_noun a2 = u3h(qual);
    u3_noun b2 = u3h(u3t(qual));
    u3_noun c2 = u3h(u3t(u3t(qual)));
    u3_noun d2 = u3t(u3t(u3t(qual)));

    if (a2 != a){
      printf("*** qual: 1 \n");
    }
    if (b2 != b){
      printf("*** qual: 2 \n");
    }
    if (c2 != c){
      printf("*** qual: 3 \n");
    }
    if (d2 != d){
      printf("*** qual: 4 \n");
    }
  }
}

/* _test_cells_complex(): build cells with more complex methods
*/
static void
_test_cells_complex()
{
  // trel
  {
    //     q
    //    / \
    //   a   ?
    //      / \
    //     b   c

    // Produce the triple `[a b c]`.
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = (u3_noun) 0x3;

    u3_noun q = u3i_trel(a, b, c);

    u3_noun a2 = 0;
    u3_noun b2 = 0;
    u3_noun c2 = 0;

    u3x_trel(q, &a2, &b2, &c2);

    if (a2 != a){
      printf("*** _test_cells_complex: trel() 1 a\n");
    }
    if (b2 != b){
      printf("*** _test_cells_complex: trel() 2 a\n");
    }
    if (c2 != c){
      printf("*** _test_cells_complex: trel() 3 a\n");
    }
  }

  // qual
  {
    //     q
    //    / \
    //   a   ?
    //      / \
    //     b   z
    //        / \
    //       c   d

    // Produce the qual `[a b c d]`.
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = (u3_noun) 0x3;
    u3_noun d = (u3_noun) 0x4;

    u3_noun z = u3i_cell(c, d);
    u3_noun q = u3i_trel(a, b, z);

    u3_noun a2 = 0;
    u3_noun b2 = 0;
    u3_noun c2 = 0;
    u3_noun d2 = 0;

    u3x_qual(q, &a2, &b2, &c2, &d2);

    if (a2 != a){
      printf("*** _test_cells_complex: qual() a\n");
    }
    if (b2 != b){
      printf("*** _test_cells_complex: qual() b\n");
    }
    if (c2 != c){
      printf("*** _test_cells_complex: qual() c\n");
    }
    if (d2 != d){
      printf("*** _test_cells_complex: qual() d\n");
    }
  }

  // quil
  {
    //     q
    //    / \
    //   a   ?
    //      / \
    //     b   z
    //        / \
    //       c   ?
    //          / \
    //         d   e

    // Produce `[a b c d e]`.
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = (u3_noun) 0x3;
    u3_noun d = (u3_noun) 0x4;
    u3_noun e = (u3_noun) 0x5;

    u3_noun z = u3i_trel(c, d, e);
    u3_noun q = u3i_trel(a, b, z);

    u3_noun a2 = 0;
    u3_noun b2 = 0;
    u3_noun c2 = 0;
    u3_noun d2 = 0;
    u3_noun e2 = 0;

    u3x_quil(q, &a2, &b2, &c2, &d2, &e2);

    if (a2 != a){
      printf("*** _test_cells_complex: quil() a\n");
    }
    if (b2 != b){
      printf("*** _test_cells_complex: quil() b\n");
    }
    if (c2 != c){
      printf("*** _test_cells_complex: quil() c\n");
    }
    if (d2 != d){
      printf("*** _test_cells_complex: quil() d\n");
    }
    if (e2 != e){
      printf("*** _test_cells_complex: quil() e\n");
    }
  }

  // hext
  {
    //     q
    //    / \
    //   a   ?
    //      / \
    //     b   z
    //        / \
    //       c   ?
    //          / \
    //         d   .
    //            / \
    //           e   f
    //
    // Produce `[a b c d e f]`.
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = (u3_noun) 0x3;
    u3_noun d = (u3_noun) 0x4;
    u3_noun e = (u3_noun) 0x5;
    u3_noun f = (u3_noun) 0x6;

    u3_noun z = u3i_trel(d, e, f);
    u3_noun q = u3i_qual(a, b, c, z);

    u3_noun a2 = 0;
    u3_noun b2 = 0;
    u3_noun c2 = 0;
    u3_noun d2 = 0;
    u3_noun e2 = 0;
    u3_noun f2 = 0;

    u3x_hext(q, &a2, &b2, &c2, &d2, &e2, &f2);

    if (a2 != a){
      printf("*** _test_cells_complex: hext() a\n");
    }
    if (b2 != b){
      printf("*** _test_cells_complex: hext() b\n");
    }
    if (c2 != c){
      printf("*** _test_cells_complex: hext() c\n");
    }
    if (d2 != d){
      printf("*** _test_cells_complex: hext() d\n");
    }
    if (e2 != e){
      printf("*** _test_cells_complex: hext() e - e2 = %i\n", e2);
    }
    if (f2 != f){
      printf("*** _test_cells_complex: hext() f - f2 = %i\n", f2);
    }
  }
}

/* _test_imprison_complex(): more complicated into/out-of nouns
*/
static void
_test_imprison_complex()
{
  // vint
  {
    u3_noun a = 1;

    u3_noun b= u3i_vint(a);
    if (2 != b){
      printf("*** vint 1\n");
    }

    u3_noun c = u3i_vint(b);
    if (3 != c){
      printf("*** vint 2\n");
    }

    //  XX disabled, 64-bit
    //
#if 0
    {
      c3_d d = 1ULL << 50;
      a = u3i_chubs(1, &d);
      b = u3i_vint(a);

      if ((a + 1) != b){
        printf("*** vint 3\n");
      }
    }
#endif
  }

    // bytes
  {
    c3_y in_y[10] = { 10, 20, 0xff};
    u3_noun a = u3i_bytes(3, in_y);

    c3_w out_a = u3r_byte(0, a);
    if (10 != out_a ){
      printf("*** u3r_byte 1\n");
    }

    c3_w out_b = u3r_byte(1, a);
    if (20 != out_b ){
      printf("*** u3r_byte 2\n");
    }

    c3_w out_c = u3r_byte(2, a);
    if (0xff != out_c ){
      printf("*** u3r_byte 3\n");
    }

    c3_y out_y[10];
    memset(out_y, 0, 10 * sizeof(c3_y));
    u3r_bytes(0, 3, out_y, a);

    if (10 != out_y[0] ||
        20 != out_y[1] ||
        0xff != out_y[2] ||
        0 != out_y[3]
        ){
      printf("*** u3r_byte 4\n");
    }
  }

  // words
  {
    c3_w in_w[10] = {10, 20, 0xffffffff};
    u3_noun noun = u3i_words(3, in_w);


    c3_w out_a = u3r_word(0, noun);
    if (10 != out_a ){
      printf("*** u3r_word 1\n");
    }

    c3_w out_b = u3r_word(1, noun);
    if (20 != out_b ){
      printf("*** u3r_word 2\n");
    }

    c3_w out_c = u3r_word(2, noun);
    if (0xffffffff != out_c ){
      printf("*** u3r_word 3\n");
    }

    c3_w out_w[10];
    memset(out_w, 0, 10 * sizeof(c3_w));
    u3r_words(0, 3, out_w, noun);

    if (10 != out_w[0] ||
        20 != out_w[1] ||
        0xffffffff != out_w[2] ||
        0 != out_w[3]
        ){
      printf("*** u3r_word 4\n");
    }
  }

  // chubs
  {
    c3_d in_d[10] = {1, 2, 0xffffffffffffffffULL};

    c3_d out_d[10];

    u3_noun a =  u3i_chubs(1, & in_d[0]);
    memset(out_d, 0, sizeof(c3_d) * 10);
    u3r_chubs(0, 1, out_d, a);
    if (1 != out_d[0] ){
      printf("*** u3r_chubs 1\n");
    }


    u3_noun b =  u3i_chubs(1, & in_d[1]);
    memset(out_d, 0, sizeof(c3_d) * 10);
    u3r_chubs(0, 1, out_d, b);
    if (2 != out_d[0] ){
      printf("*** u3r_chubs 2\n");
    }

    u3_noun c =  u3i_chubs(1, & in_d[2]);
    memset(out_d, 0, sizeof(c3_d) * 10);
    u3r_chubs(0, 1, out_d, c);
    if (0xffffffffffffffffULL != out_d[0] ){
      printf("*** u3r_chubs 3\n");
    }

    u3_noun d =  u3i_chubs(3, in_d);
    memset(out_d, 0, sizeof(c3_d) * 10);
    u3r_chubs(0, 3, out_d, d);
    if (1 != out_d[0] ){
      printf("*** u3r_chubs 4-a\n");
    }
    if (2 != out_d[1] ){
      printf("*** u3r_chubs 4-b\n");
    }
    if (0xffffffffffffffffULL != out_d[2] ){
      printf("*** u3r_chubs 4-c\n");
    }
  }

  // string
  {
    c3_c * in_c = "a";
    u3_noun noun = u3i_string(in_c);
    c3_c* out_c = u3r_string(noun);

    if (0 != strcmp(in_c, out_c)){
      printf("*** u3r_string: in '%s'; out '%s'\n", in_c, out_c);
    }

    c3_free(out_c);
    in_c = "ab";
    noun = u3i_string(in_c);
    out_c = u3r_string(noun);

    if (0 != strcmp(in_c, out_c)){
      printf("*** u3r_string: in '%s'; out '%s'\n", in_c, out_c);
    }

    c3_free(out_c);
    in_c = "abcd";
    noun = u3i_string(in_c);
    out_c = u3r_string(noun);

    if (0 != strcmp(in_c, out_c)){
      printf("*** u3r_string: in '%s'; out '%s'\n", in_c, out_c);
    }

    c3_free(out_c);
    in_c = "this is a test";
    noun = u3i_string(in_c);
    out_c = u3r_string(noun);

    if (0 != strcmp(in_c, out_c)){
      printf("*** u3r_string: in '%s'; out '%s'\n", in_c, out_c);
    }

    c3_free(out_c);
  }

  // tape
  {
    c3_c* in_c = "this is a test";
    u3_noun noun = u3i_tape(in_c);

    c3_y* out_y = u3r_tape(noun);

    if (0 != memcmp(in_c, out_y, strlen(in_c))){
      printf("*** u3r_tape 1\n");
    }

    c3_free(out_y);

    // tape stores each byte in the string as one atom in the tree
    u3_noun lent = u3qb_lent(noun);
    if ( (c3_w)lent != strlen(in_c) ){
      printf("*** u3r_tape 2\n");
    }
  }

  // edit
  {

  //    q
  //   / \
  //  a1  r
  //     / \
  //    b2  s
  //       / \
  //      c3  d4

    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = (u3_noun) 0x3;
    u3_noun d = (u3_noun) 0x4;

    u3_noun s = u3i_cell(c, d);
    u3_noun r = u3i_cell(b, s);
    u3_noun q = u3i_cell(a, r);

    u3_noun axis = 2;
    u3_noun newval = 99;
    u3_noun hacked = u3i_edit(q, axis, newval);

    u3_noun read_1;
    u3r_mean(hacked, axis, &read_1, 0);

    if (newval != read_1){
      printf("*** u3i_edit 1\n");
    }
  }

  // molt
  {

    //    q
    //   / \
    //  a1  r
    //     / \
    //    b2  s
    //       / \
    //      c3  d4

    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x2;
    u3_noun c = (u3_noun) 0x3;
    u3_noun d = (u3_noun) 0x4;

    u3_noun s = u3i_cell(c, d);
    u3_noun r = u3i_cell(b, s);
    u3_noun q = u3i_cell(a, r);

    u3_noun axis_1 = 2;
    u3_noun newval_1 = 99;

    u3_noun axis_2 = 6;
    u3_noun newval_2 = 777;

    u3_noun hacked = u3i_molt(q, axis_1, newval_1, axis_2, newval_2, 0);

    u3_noun read_1;
    u3_noun read_2;
    u3r_mean(hacked, axis_1, &read_1, axis_2, &read_2, 0);

    if (newval_1 != read_1){
      printf("*** u3i_molt 1\n");
    }

    if (newval_2 != read_2){
      printf("*** u3i_molt 2\n");
    }
  }
}

/* _test_sing(): Yes iff (a) and (b) are the same noun.
*/
static void
_test_sing()
{
  // direct noun
  //
  {
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x1;
    u3_noun c = (u3_noun) 0x2;

    if (c3y != u3r_sing(a, a)) {
      printf("*** sing direct: 1 \n");
    }

    if (c3y != u3r_sing(a, b)) {
      printf("*** sing direct: 2 \n");
    }

    if (c3n != u3r_sing(a, c)) {
      printf("*** sing direct: 3 \n");
    }
  }

  // indirect
  //
  {
    c3_c* in_alpha_c =  "abcdefghijklmnopqrstuvwxyz";
    c3_c* in_numer_c =  "0123456789001234567890";


    u3_noun a = u3i_string(in_alpha_c);
    u3_noun b = u3i_string(in_alpha_c);
    u3_noun c = u3i_string(in_numer_c);

    if (c3y != u3r_sing(a, a)) {
      printf("*** sing indirect: 1 \n");
    }

    if (c3y != u3r_sing(a, b)) {
      printf("*** sing indirect: 2 \n");
    }

    if (c3n != u3r_sing(a, c)) {
      printf("*** sing indirect: \n");
    }
  }
}

/* _test_fing(): yes same copy of the same noun (ie, pointer equality)
*/
static void
_test_fing()
{
  // direct noun
  //
  {
    u3_noun a = (u3_noun) 0x1;
    u3_noun b = (u3_noun) 0x1;
    u3_noun c = (u3_noun) 0x2;

    if (c3y != u3r_fing(a, a)) {
      printf("*** fing direct: 1 \n");
    }

    if (c3y != u3r_fing(a, b)) {
      printf("*** fing direct: 2 \n");
    }

    if (c3n != u3r_fing(a, c)) {
      printf("*** fing direct: 3 \n");
    }
  }

  // indirect
  //
  {
    c3_c* in_alpha_c =  "abcdefghijklmnopqrstuvwxyz";
    c3_c* in_numer_c =  "0123456789001234567890";


    u3_noun a = u3i_string(in_alpha_c);
    u3_noun b = u3i_string(in_alpha_c);
    u3_noun c = u3i_string(in_numer_c);

    if (c3y != u3r_fing(a, a)) {
      printf("*** fing indirect: 1 \n");
    }

    if (c3n != u3r_fing(a, b)) {
      printf("*** fing indirect: 2 \n");
    }

    if (c3n != u3r_fing(a, c)) {
      printf("*** fing indirect: \n");
    }
  }
}

/* _test_met(): 'met' = measure / take size
*/
static void
_test_met()
{
  c3_w ret_w;
  u3_atom atom;

  // 1
   {
    atom = 1;

    ret_w = u3r_met(0, atom);
    if (1 != ret_w){
      printf("*** _test_met bit of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(3, atom);
    if (1 != ret_w){
      printf("*** _test_met byte of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(4, atom);
    if (1 != ret_w){
      printf("*** _test_met _w of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(5, atom);
    if (1 != ret_w){
      printf("*** _test_met _d of 1 = %d \n", ret_w);
    }
  }

  // 2 = 0b10
   {
    atom = 2;

    ret_w = u3r_met(0, atom);
    if (2 != ret_w){
      printf("*** _test_met bit of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(3, atom);
    if (1 != ret_w){
      printf("*** _test_met byte of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(5, atom);
    if (1 != ret_w){
      printf("*** _test_met _w of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(6, atom);
    if (1 != ret_w){
      printf("*** _test_met _d of 1 = %d \n", ret_w);
    }
  }

  // 8=0b1000
   {
    atom = 8;

    ret_w = u3r_met(0, atom);
    if (4 != ret_w){
      printf("*** _test_met bit of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(3, atom);
    if (1 != ret_w){
      printf("*** _test_met byte of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(5, atom);
    if (1 != ret_w){
      printf("*** _test_met _w of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(6, atom);
    if (1 != ret_w){
      printf("*** _test_met _d of 1 = %d \n", ret_w);
    }
  }

  // 0xff = 255 =0b 1111 1111
   {
    atom = 0xff;

    ret_w = u3r_met(0, atom);
    if (8 != ret_w){
      printf("*** _test_met bit of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(3, atom);
    if (1 != ret_w){
      printf("*** _test_met byte of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(5, atom);
    if (1 != ret_w){
      printf("*** _test_met _w of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(6, atom);
    if (1 != ret_w){
      printf("*** _test_met _d of 1 = %d \n", ret_w);
    }
  }

  // 0x100 = 256 =0b 0001 1111 1111
   {
    atom = 0x100;

    ret_w = u3r_met(0, atom);
    if (9 != ret_w){
      printf("*** _test_met bit of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(3, atom);
    if (2 != ret_w){
      printf("*** _test_met byte of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(5, atom);
    if (1 != ret_w){
      printf("*** _test_met _w of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(6, atom);
    if (1 != ret_w){
      printf("*** _test_met _d of 1 = %d \n", ret_w);
    }
  }

  //  XX disabled, 64-bit
  //
#if 0
  // 32 bit direct
  // 0x ff ff ff ff
  {
    atom = 0xffffffffULL;

    ret_w = u3r_met(0, atom);
    if (32 != ret_w){
      printf("*** _test_met bit of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(3, atom);
    if (4 != ret_w){
      printf("*** _test_met byte of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(5, atom);
    if (1 != ret_w){
      printf("*** _test_met _w of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(6, atom);
    if (1 != ret_w){
      printf("*** _test_met _d of 1 = %d \n", ret_w);
    }
  }
#endif

  // 4 words x 32 bits each = 128 bits = 16 bytes = 4 words = 2 doubles
  //
  {
    c3_w data_w[4] = { 0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff };
    atom = u3i_words(4, data_w);

    ret_w = u3r_met(0, atom);
    if (128 != ret_w){
      printf("*** _test_met bit of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(3, atom);
    if (16 != ret_w){
      printf("*** _test_met byte of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(5, atom);
    if (4 != ret_w){
      printf("*** _test_met _w of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(6, atom);
    if (2 != ret_w){
      printf("*** _test_met _d of 1 = %d \n", ret_w);
    }
  }

  // 4 words (top word is '1' )
  //
  {
    c3_w data_w[4] = { 0xffffffff, 0xffffffff, 0xffffffff, 1 };
    atom = u3i_words(4, data_w);

    ret_w = u3r_met(0, atom);
    if (97 != ret_w){
      printf("*** _test_met bit of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(3, atom);
    if (13 != ret_w){
      printf("*** _test_met byte of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(5, atom);
    if (4 != ret_w){
      printf("*** _test_met _w of 1 = %d \n", ret_w);
    }

    ret_w = u3r_met(6, atom);
    if (2 != ret_w){
      printf("*** _test_met _d of 1 = %d \n", ret_w);
    }
  }
}

/* _test_u3r_at(): inspect cells at arbitrary axis locations
** [ and utility function u3x_dep() ]
*/
static void
_test_u3r_at()
{
  c3_w a_w = u3x_dep(0);

  if (0xffffffff != a_w) {  printf("*** u3x_dep() \n"); }

  a_w = u3x_dep(1);
  if (0 != a_w) {  printf("*** u3x_dep() \n"); }

  a_w = u3x_dep(0b10);
  if (1 != a_w) {  printf("*** u3x_dep() \n"); }

  a_w = u3x_dep(0b11);
  if (1 != a_w) {  printf("*** u3x_dep() \n"); }

  a_w = u3x_dep(0b100);
  if (2 != a_w) {  printf("*** u3x_dep() \n"); }

  a_w = u3x_dep(0b110);
  if (2 != a_w) {  printf("*** u3x_dep() \n"); }

  a_w = u3x_dep(0b111);
  if (2 != a_w) {  printf("*** u3x_dep() \n"); }

  a_w = u3x_dep( ((c3_w) (((c3_d) 1  << 32) - 1)) );
  if (31 != a_w) {  printf("*** u3x_dep() \n"); }


  //  XX disabled, 64-bit
  //
#if 0
  a_w = u3x_dep_d(0);
  a_w = u3x_dep_d(1);
  a_w = u3x_dep_d(0b10);
  a_w = u3x_dep_d(0b11);
  a_w = u3x_dep_d(0b100);
  a_w = u3x_dep_d( ((c3_w) (((c3_d) 1  << 32) - 1)) );
  a_w = u3x_dep_d( ((c3_w) (((c3_d) 1  << 33) - 1)) );
  a_w = u3x_dep_d( ((c3_d) (((c3_d) 1  << 64) - 1)) );
#endif

  u3_weak ret;

  // addr 1 in atom == atom value
  u3_noun tree = 1;
  ret = u3r_at( 1, tree);
  if (1 != ret) {  printf("*** u3r_at()\n"); }

  // addr 1 in atom == atom value
  tree = 2;
  ret = u3r_at( 1, tree);
  if (2 != ret) {  printf("*** u3r_at \n"); }

  // illegal
  ret = u3r_at( 2, tree);
  if (u3_none != ret) {  printf("*** u3r_at \n"); }


  // simple tree [ 1 2]
  tree = u3i_cell(10, 20);
  ret = u3r_at( 1, tree);
  if (tree != ret) {  printf("*** u3r_at \n"); }

  ret = u3r_at( 2, tree);
  if (10 != ret) {  printf("*** u3r_at \n"); }
  ret = u3r_at( 3, tree);
  if (20 != ret) {  printf("*** u3r_at \n"); }

  // simple tree [ 1 <BIGNUM>]
  c3_w in_w[10] = {10, 20, 0xffffffff};
  u3_noun bignum = u3i_words(3, in_w);

  tree = u3i_cell(99, bignum);
  ret = u3r_at( 2, tree);
  if (99 != ret) {  printf("*** u3r_at \n"); }
  ret = u3r_at( 3, tree);
  if (bignum != ret) {  printf("*** u3r_at \n"); }
}

//  XX disabled, static functions
//
#if 0
void _n_push(c3_ys mov, c3_ys off, u3_noun a);
u3_noun * _n_peek(c3_ys off);
u3_noun* _n_peet(c3_ys mov, c3_ys off);
void _n_pop(c3_ys mov);
u3_noun _n_pep(c3_ys mov, c3_ys off);
void _n_toss(c3_ys mov, c3_ys off);
u3_noun* _n_swap(c3_ys mov, c3_ys off);

/* _test_nvm_stack_inner():
*/
void
_test_nvm_stack_inner(c3_ys mov,  c3_ys off)
{
  u3_noun * peek;

  // push 1, peek, pop
  if(1)  {
    _n_push(mov, off, 0x1122334455667788);
    peek = _n_peek(off);
    if (0x1122334455667788 != *peek) {  printf("*** test_nvm_stack 1\n"); }
    _n_pop(mov);
  }

  // push 2, peek, pop, peek, pop
  if(1)  {
    _n_push(mov, off, 88);
    _n_push(mov, off, 99);
    peek = _n_peek(off);
    if (99 != *peek) {  printf("*** test_nvm_stack 2\n"); }
    _n_pop(mov);
    peek = _n_peek(off);
    if (88 != *peek) {  printf("*** test_nvm_stack 3\n"); }
    _n_pop(mov);
  }

  // 100 x (push, peek, pop)
  {
    int ii;
    for (ii=0; ii <= 100; ii++){
      _n_push(mov, off, (u3_noun) ii);
    }
    for (ii=100; ii >= 0; ii--){
      peek = _n_peek(off);
      if (ii != *peek) {  printf("*** test_nvm_stack 4\n"); }
      _n_pop(mov);
    }
  }

  // peet()
  {
    _n_push(mov, off, 333);
    _n_push(mov, off, 444);

    peek = _n_peet(mov, off);
    if (333 != *peek) {  printf("*** test_nvm_stack 5\n"); }

    _n_push(mov, off, 555);
    peek = _n_peet(mov, off);
    if (444 != *peek) {  printf("*** test_nvm_stack 6\n"); }
  }

  // pep()
  {
    _n_push(mov, off, 777);
    u3_noun ret =    _n_pep(mov, off);
    if (777 != ret) {  printf("*** test_nvm_stack 7\n"); }


    _n_push(mov, off, 777);
    _n_push(mov, off, 888);
    ret =    _n_pep(mov, off);
    if (888 != ret) {  printf("*** test_nvm_stack 8\n"); }
    ret =    _n_pep(mov, off);
    if (777 != ret) {  printf("*** test_nvm_stack 9\n"); }

  }

  // toss
  {
    _n_push(mov, off, 2777);
    _n_push(mov, off, 3888);
    _n_toss(mov, off);
    u3_noun ret =    _n_pep(mov, off);
    if (2777 != ret) {  printf("*** test_nvm_stack 10\n"); }

  }

  // swap
  {
    _n_push(mov, off, 2002);
    _n_push(mov, off, 3003);
    _n_swap(mov, off);
    u3_noun ret =    _n_pep(mov, off);
    if (2002 != ret) {  printf("*** test_nvm_stack 11\n"); }
    ret =    _n_pep(mov, off);
    if (3003 != ret) {  printf("*** test_nvm_stack 12\n"); }

  }
}

/* _test_nvm_stack_south(): test the stack usage in an inner, south road
*/
u3_noun
_test_nvm_stack_south(u3_noun arg)
{
  c3_ys mov = 2;
  c3_ys off = -2;

  _test_nvm_stack_inner(mov, off);

  return u3_nul;
}
#endif

/* _test_nvm_stack(): test the stack usage of the bytecode interpreter
** (growing in both directions: N and S)
*/
static void
_test_nvm_stack()
{
  //  XX disabled, static functions
  //  XX rewrite to use u3a_push/u3a_pop?
  //
#if 0
  // north road
  c3_ys mov = -2;
  c3_ys off = 0;
  _test_nvm_stack_inner(mov, off);

  // south road
  u3m_soft(100, &_test_nvm_stack_south, 0);
#endif
}

static c3_i
_test_noun(void)
{
  c3_i ret_i = 1;

  if ( !_test_chop() ) {
    fprintf(stderr, "test noun: chop failed\r\n");
    ret_i = 0;
  }

  return ret_i;
}

/* main(): run all test cases.
*/
int
main(int argc, char* argv[])
{
   _setup();

  if ( !_test_noun() ) {
    fprintf(stderr, "test noun: failed\r\n");
    exit(1);
  }

  //  GC
  //
  u3m_grab(u3_none);

  //  XX the following tests leak memory
  //  fix and move to _test_noun()
  //
  _test_noun_bits_set();
  _test_noun_bits_read();
  _test_imprison();
  _test_imprison_complex();
  _test_sing();
  _test_fing();
  _test_met();
  _test_cells();
  _test_cells_complex();
  _test_u3r_at();
  _test_nvm_stack();

  fprintf(stderr, "test_noun: ok\n");

  return 0;
}
