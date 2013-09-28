/* include/c/types.h
**
** This file is in the public domain.
*/
  /** Integer typedefs.
  **/
    /* Canonical integers.
    */
      typedef uint64_t c3_d;
      typedef int64_t c3_ds;
      typedef uint32_t c3_w;
      typedef int32_t c3_ws;
      typedef uint16_t c3_s;
      typedef int16_t c3_ss;
      typedef uint8_t c3_y;   // byte
      typedef int8_t c3_ys;   // signed byte
      typedef uint8_t c3_b;   // bit

      typedef uint8_t c3_t;   // boolean
      typedef uint8_t c3_g;   // 32-bit log - 0-31 bits
      typedef uint32_t c3_l;  // little; 31-bit unsigned integer
      typedef uint32_t c3_m;  // mote; also c3_l; LSB first a-z 4-char string.

    /* C true and false; boolean logic
    */
#     define c3_true  1
#     define c3_false 0
#     define c3_and(x, y)   ((x) && (y))
#     define c3_or(x, y)    ((x) || (y))

    /* Deprecated integers. 
    */
      typedef char      c3_c;      // does not match int8_t or uint8_t
      typedef int       c3_i;      // int - really bad 
      typedef uintptr_t c3_p;      // pointer-length uint - really really bad
      typedef intptr_t c3_ps;      // pointer-length int - really really bad
