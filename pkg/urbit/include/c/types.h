#ifndef C3_TYPES_H
#define C3_TYPES_H

  /** Integer typedefs.
  **/
    /* Canonical integers.
    */
      typedef size_t c3_z;
      typedef ssize_t c3_zs;
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
      typedef uint8_t c3_o;   // loobean
      typedef uint8_t c3_g;   // 32-bit log - 0-31 bits
      typedef uint32_t c3_l;  // little; 31-bit unsigned integer
      typedef uint32_t c3_m;  // mote; also c3_l; LSB first a-z 4-char string.

    /* Deprecated integers.
    */
      typedef char      c3_c;      // does not match int8_t or uint8_t
      typedef int       c3_i;      // int - really bad
      typedef uintptr_t c3_p;      // pointer-length uint - really really bad
      typedef intptr_t c3_ps;      // pointer-length int - really really bad

      /* Print specifiers
      */

      /* c3_z */
      #define PRIc3_z  "zu"      /* unsigned dec */
      #define PRIc3_zs "zd"      /*   signed dec */
      #define PRIxc3_z "zx"      /* unsigned hex */
      #define PRIXc3_z "zX"      /* unsigned HEX */

      /* c3_d */
      #define PRIc3_d  PRIu64
      #define PRIc3_ds PRIi64
      #define PRIxc3_d PRIx64
      #define PRIXc3_d PRIX64

      /* c3_w */
      #define PRIc3_w  PRIu32
      #define PRIc3_ws PRIi32
      #define PRIxc3_w PRIx32
      #define PRIXc3_w PRIX32

      /* c3_s */
      #define PRIc3_s  PRIu16
      #define PRIc3_ss PRIi16
      #define PRIxc3_s PRIx16
      #define PRIXc3_s PRIX16

      /* c3_y */
      #define PRIc3_y  PRIu8
      #define PRIc3_ys PRIi8
      #define PRIxc3_y PRIx8
      #define PRIXc3_y PRIX8

      /* c3_b */
      #define PRIc3_b  PRIu8
      #define PRIxc3_b PRIx8
      #define PRIXc3_b PRIX8

#endif /* ifndef C3_TYPES_H */
