/* include/c/defs.h
**
** This file is in the public domain.
*/
  /** Random useful C macros.
  **/
    /* Assert.  Good to capture.
    */
// #     define c3_assert(x)   assert(x)
#     define c3_assert(x)  ( (x) ? 0 : c3_cooked(), assert(x) )

    /* Stub.
    */
#     define c3_stub       (assert(!"stub"), 0)

    /* Size in words.
    */
#     define c3_wiseof(x)  (((sizeof (x)) + 3) >> 2)

    /* Bits in byte.
    */
#ifdef C3_GLOBAL
      c3_y Bts_y[] = {
        0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4,
        5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
        6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
        6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8
      };
#else
      extern c3_y Bts_y[];
#endif
#     define c3_bits_byte(y)  Bts_y[y]
#     define c3_bits_word(w) \
        ( ((w) >> 24)                   \
            ? (24 + Bts_y[(w) >> 24])   \
            : ((w) >> 16)               \
              ? (16 + Bts_y[(w) >> 16]) \
              : ((w) >> 8)              \
                ? (8 + Bts_y[(w) >> 8]) \
                : Bts_y[(w)] )

    /* Min and max.
    */
#     define c3_max(x, y) ( ((x) > (y)) ? (x) : (y) )
#     define c3_min(x, y) ( ((x) < (y)) ? (x) : (y) )

    /* Emergency stdio fix.
    */
      int
      c3_cooked();

    /* Short integers.
    */
#     define c3_s1(a)          ( (a) )
#     define c3_s2(a, b)       ( ((b) << 8) | c3_s1(a) )
#     define c3_s3(a, b, c)    ( ((c) << 16) | c3_s2(a, b) )
#     define c3_s4(a, b, c, d) ( ((d) << 24) | c3_s3(a, b, c) )

#     define c3_s5(a, b, c, d, e) \
        ( ((uint64_t)c3_s1(e) << 32ULL) | c3_s4(a, b, c, d) )
#     define c3_s6(a, b, c, d, e, f) \
        ( ((uint64_t)c3_s2(e, f) << 32ULL) | c3_s4(a, b, c, d) )
#     define c3_s7(a, b, c, d, e, f, g) \
        ( ((uint64_t)c3_s3(e, f, g) << 32ULL) | c3_s4(a, b, c, d) )
#     define c3_s8(a, b, c, d, e, f, g, h) \
        ( ((uint64_t)c3_s4(e, f, g, h) << 32ULL) | c3_s4(a, b, c, d) )

    /* Logging shorthand.
    */
#     define c3_log_every(n, args...) \
        do {                          \
          static c3_w cnt_w = 0;      \
                                      \
          if ( 0 == cnt_w % (n) ) {   \
            uL(fprintf(uH, args));    \
          }                           \
          cnt_w = (cnt_w + 1) % (n);  \
        } while (0)
/* c3_malloc(): asserting malloc
 */
#define c3_malloc(s) ({                         \
      void* rut = malloc(s);                    \
      if ( 0 == rut ) {                         \
        c3_assert(!"memory lost");              \
      }                                         \
      rut;})
