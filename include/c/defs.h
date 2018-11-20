/**
  include/c/defs
  ==============

  This file is in the public domain.

  Loobeans -- Inverse Booleans to Match Nock
  ------------------------------------------

  - c3y          -- loobean true
  - c3n          -- loobean false
  - _(lob)       -- convert loobean to boolean
  - _(lob)       -- convert loobean to boolean
  - __(bol)      -- convert boolean to loobean
  - c3a(lob,lob) -- logical and for loobeans
  - c3o(lob,lob) -- logical or for loobeans
*/

#define c3y        0
#define c3n        1
#define _(x)       (c3y == (x))
#define __(x)      ((x) ? c3y : c3n)
#define c3a(x, y)  __(_(x) && _(y))
#define c3o(x, y)  __(_(x) || _(y))

/**
  Misc Macros
  -----------

  - c3_assert(x)       -- good to capture (XX wut?)
  - c3_stub            -- placeholder for something not yet implemented
  - c3_min(exp,exp)    -- minimum
  - c3_max(exp,exp)    -- maximum
  - c3_rotw(r,x)       -- rotate word `x` left by `r` bits
  - c3_wiseof(type)    -- size in words
  - c3_bits_word(word) -- number of significant bits in a word
      For example, `0b0100` has three significant bits.
*/

#define c3_assert(x)     ( (x) ? 0 : c3_cooked(), assert(x) )
#define c3_stub          (assert(!"stub"), 0)
#define c3_max(x, y)     ( ((x) > (y)) ? (x) : (y) )
#define c3_min(x, y)     ( ((x) < (y)) ? (x) : (y) )
#define c3_rotw(r, x)    ( ((x) << (r)) | ((x) >> (32 - (r))) )
#define c3_wiseof(x)     (((sizeof (x)) + 3) >> 2)
#define c3_bits_word(w)  ((w) ? (32 - __builtin_clz(w)) : 0)

/**
  Stdio Hacks
  -----------

  - c3_cooked(): Emergency stdio fix. (XX: What?)
*/

int c3_cooked();

/**
  Short Integers
  --------------
*/

#define c3_s1(a)          ( (a) )
#define c3_s2(a, b)       ( ((b) << 8) | c3_s1(a) )
#define c3_s3(a, b, c)    ( ((c) << 16) | c3_s2(a, b) )
#define c3_s4(a, b, c, d) ( ((d) << 24) | c3_s3(a, b, c) )

#define c3_s5(a, b, c, d, e) \
   ( ((uint64_t)c3_s1(e) << 32ULL) | c3_s4(a, b, c, d) )

#define c3_s6(a, b, c, d, e, f) \
   ( ((uint64_t)c3_s2(e, f) << 32ULL) | c3_s4(a, b, c, d) )

#define c3_s7(a, b, c, d, e, f, g) \
   ( ((uint64_t)c3_s3(e, f, g) << 32ULL) | c3_s4(a, b, c, d) )

#define c3_s8(a, b, c, d, e, f, g, h) \
   ( ((uint64_t)c3_s4(e, f, g, h) << 32ULL) | c3_s4(a, b, c, d) )

/**
  Byte-Order Twiddling
  --------------------
*/

#define c3_flip32(w) \
   ( (((w) >> 24) & 0xff) \
   | (((w) >> 16) & 0xff) << 8 \
   | (((w) >>  8) & 0xff) << 16 \
   | ( (w)        & 0xff) << 24 )

/**
  Logging Shorthand
  -----------------
*/

#define c3_log_every(n, args...) \
   do {                          \
     static c3_w cnt_w = 0;      \
                                 \
     if ( 0 == cnt_w % (n) ) {   \
       uL(fprintf(uH, args));    \
     }                           \
     cnt_w = (cnt_w + 1) % (n);  \
   } while (0)

/**
  Asserting Allocation Wrappers
  -----------------------------
*/

#define c3_malloc(s) ({         \
   void* rut = malloc(s);       \
   if ( 0 == rut ) {            \
     c3_assert(!"memory lost"); \
   }                            \
   rut;})

#define c3_calloc(s) ({      \
   void* rut = c3_malloc(s); \
   memset(rut, 0, s);        \
   rut;})
