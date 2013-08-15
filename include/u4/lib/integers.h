/* include/integers.h
**
** This file is in the public domain.
*/
  /** Integer typedefs.
  **/
    /* Pointers as integers.
    */
      typedef uintptr_t u4_ptr;

    /* Iterators and generic lengths.
    */
      typedef uint32_t u4_i;

    /* Booleans.
    */
      typedef uint8_t u4_t;

    /* A native integer.  Highly discouraged.
    */
      typedef int u4_int;

    /* Generic unsigned integers.
    */
      typedef uint8_t u4_xt;   /* 1 bit (bit) */
      typedef uint8_t u4_xs;   /* 2 bits (base) */
      typedef uint8_t u4_xy;   /* 4 bits (nybble) */
      typedef uint8_t u4_xb;   /* 8 bits (byte) */
      typedef uint16_t u4_xh;  /* 16 bits (half word) */
      typedef uint32_t u4_xw;  /* 32 bits (word) */
      typedef uint64_t u4_xd;  /* 64 bits (double word) */

    /* Characters.
    */
      /* Byte characters: unknown, ASCII, C locale.
      */
        typedef char u4_c;
        typedef char u4_ca;
        typedef char u4_cl;

      /* Wide characters: unknown, UCS-4, C locale.
      */
        typedef uint32_t u4_cw;
        typedef uint32_t u4_cwu;
        typedef uint32_t u4_cwl;

    /* Roles.
    **
    ** If you need a role/modifier combination not defined here, add it.
    ** Think of it as lazy declaration.
    */
      /* k: block: unit in a variable-length block of data, buffer, etc.
      */
        typedef uint8_t u4_kb;
        typedef uint32_t u4_kw;
      
      /* p: point: unit position in a block.
      */
        typedef uint32_t u4_pt;
        typedef uint32_t u4_pb;
        typedef uint32_t u4_pw;

      /* s: span: length of a span in a block.
      */
        typedef uint32_t u4_st;
        typedef uint32_t u4_sy;
        typedef uint32_t u4_sb;
        typedef uint32_t u4_sh;
        typedef uint32_t u4_sw;

      /* h: shift: bit, or other, position within a block unit.
      */
        typedef uint8_t u4_hb;    /* bit position within byte */
        typedef uint8_t u4_hw;    /* bit position within word */
        typedef uint8_t u4_hbw;   /* byte position within word */

      /* g: log: log of a unit count.
      */
        typedef uint8_t u4_gt;

      /* a: nap: nap number.
      */
        typedef uint32_t u4_aw;

      
  /** Macros.
  **/
    /* Number of bytes in a type or variable.
    */
#     define u4_type_sb(t)   (sizeof(t))

    /* Number of bits in a type or variable.
    */
#     define u4_type_st(t)    ((u4_type_sb((t)) << 3)

    /* Number of words in a type or variable.
    */
#     define u4_type_sw(t)   u4_bblock(u4_type_st(t), 5)

    /* Maximum (unsigned integer) value of a type or variable.
    */
#     define u4_maxof(t) \
        ( (u4_type_st(t) == 64) \
            ? 0xffffffffffffffffULL \
            : ((1ULL << (uint64_t)(u4_type_st(t))) - 1ULL) )

    /* Byte pointer arithmetic - our preferred approach.
    */
#     define u4_bat(ptr) \
        ( (u4_xb *)(ptr) )

#     define u4_bat_to(base, offset) \
        ( (void *) (u4_bat(base) + (u4_pb)(offset)) ) 
 
#     define u4_bat_from(base, ptr) \
        ( (u4_pb) (u4_bat(ptr) - u4_bat(base)) )
