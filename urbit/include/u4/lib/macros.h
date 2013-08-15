/* utility.h: global data structures.
**
** This file is in the public domain.
*/
  /** Pseudosyntax.
  **/
    /* For declaring named function returns, as in:
    **
    **  _(u4_noun, pro)
    */
#     define _(a, b) a


  /** Global data-driven functions.
  **/
    /* A pseudo-randomly generated S-box.
    */
#     define u4_sbox_data \
        0xc52598d8, 0xba93dc82, 0xa9af3d6c, 0xfe83511a, 0xf40f7a8d, 0x6220e335,\
        0xf8820121, 0x99267e88, 0xdee5b579, 0x7824fff6, 0x738f8c1a, 0x766ff6e3,\
        0xb03d8836, 0x1994625d, 0x33ff3e0a,  0x3100eda, 0xda4952df, 0x62b3fdef,\
        0xf00f39e2, 0xcac0bdd8, 0xd156e14b, 0x22b9bd93, 0xeb9f040e, 0x9570fa8c,\
        0x151f4b17, 0x43680ec4, 0x362f6816, 0x534c205e, 0xe2ddee5a, 0x87652d85,\
        0x80722ba6, 0xa158b454, 0x5022a0c9, 0xb3457a9d, 0xc3e8c919, 0x7990bd4e,\
        0xf443697e, 0xf60ee7be, 0xdd68f3d2, 0xdcbfabae, 0x2ba16060, 0x2c171e36,\
        0x9b9e23e3, 0x68cfa466, 0x10cb813f, 0x40a3d8d3, 0xe14077a2, 0x12ce5e9f,\
        0x3fbeae41,  0xf4bb67c, 0xf5c7ea14, 0xae871243, 0xaed109cb, 0x51c3e98d,\
        0x5f606c88, 0x1e458e1d, 0x88fd1802, 0xcb7aac91, 0x53af8a56, 0xbddd8a18,\
        0x91878c73, 0x35e102b2, 0xf1923b3d, 0xb2078d1a, 0xf59ce52b, 0xbb396404,\
        0x2fe5b7f9, 0x5eab8ea3, 0x65a14a04, 0x6dd6f5f8,  0x88c5b42, 0x71cfffa2,\
        0xc3ab1b6f, 0x6cdd42a0, 0xb81a7b1d, 0x29079afa, 0xde331486, 0x18494012,\
        0x68d8efa2, 0x87cb540e, 0xc052b642, 0x81f4f73a, 0x44cc7e55, 0x863797ce,\
        0x8c5c9726, 0x8e498a90, 0xe808e302, 0x48d76271, 0xdeb0aa12, 0xb7a9e7c5,\
        0x8f1d0a6e, 0x8a1fd9ca, 0xd2e1d0d1, 0x909cfa5f, 0xbe74ea0b, 0xdcea1b9d,\
        0x51a57cdd, 0x516c4e92, 0xbc275702, 0x4341d236, 0x6717fac9, 0x56a8a013,\
        0x948c28b1, 0xdbaf1fa2, 0xcbff3889, 0x7fb44be7, 0x475d85c4, 0x5eba8280,\
        0x30903ccb,  0xc5857a4, 0x95b1eb1f, 0x5d3dc652, 0x1b97c827, 0x5e240703,\
        0x7c67a5f9,   0xfcef4e, 0xbcb3adac, 0x3fce40cc, 0x1a9d0bdc, 0xa46b9dd7,\
        0xa2c4566b, 0x61f4a52d, 0x44844992, 0xfaffdd2c, 0x14187d04, 0x7360006c,\
        0xa9417956, 0xd10fa3a2, 0x78eea5cd, 0x4743903f, 0xc7f42e48, 0xac765510,\
        0xe6ffe2fb, 0xe8cc50f0, 0xce5e358d, 0xdeeed3e8, 0xb530da9c, 0xa01ebf2b,\
        0xbced287a, 0x1cbee300,  0x26378ab, 0x3845a9e2, 0xb6c15b6a, 0xe5d52c8e,\
        0x62d39736, 0x6a523674, 0xc70f9e65, 0xe777884a, 0xcbeb6b4d, 0x953d349c,\
        0xb265b0ee, 0xa34f347f, 0x3415c213, 0xe9f360eb, 0x42c7062e, 0x566dd093,\
         0x5af69ed, 0xa7589924, 0xe0c038ed, 0xcf524c9e, 0x85a398d2, 0x2026a870,\
        0xfdf0043a, 0x28c29abc, 0x70378b33, 0x111a1094, 0x90c6b901, 0x712f6c16,\
        0x4c54fce7,  0x617eda8, 0x7fe89a8b, 0x6c7d01f5,  0xfa54e14, 0x72fb1b49,\
        0xd82afef8, 0x1055a07d,  0xf435845, 0x4f112d16, 0xe2309c6d, 0x9692c387,\
        0xf5b593c3,  0xca1f284, 0x36ca1157, 0x7255caee, 0x88502921, 0xbdf1cb83,\
        0xf54a4761, 0x54447357, 0xeefbdb16, 0x1931155c, 0x1b06df20, 0xe4c95998,\
        0xcbf794d3, 0x261389c8, 0x875f1bb9, 0x8b29fd75, 0x350d3a0c, 0xe95ea3b5,\
        0x12139f64, 0xd3e5b8fa, 0x1673f21d, 0xab1f9401, 0x37eb6480, 0x48f0a344,\
        0xc1c25e3a, 0x70186f9e, 0x9887957d, 0xfd0dbfee, 0xb799e1be, 0xb13c83a7,\
         0x3c2efad, 0xf3c63a9d, 0xffc07dd2, 0x7b941d3c, 0x9182f20d, 0x78a53743,\
         0xb399347, 0xf823b4ad, 0x4bbb0e3f, 0xb3816aba, 0xa47cc7b1, 0x5242aa7a,\
        0x40b92e52, 0xcf550453, 0x6dea345d, 0x5230256a,  0x209e49e, 0xc81585e2,\
        0x2d477432, 0x5fb51d7c, 0x1b45677c, 0x180d4405, 0x4b72be7f, 0x54464f51,\
        0xfbfbda37, 0xb9df9e6c, 0x12ad416c, 0x5d563751, 0x6d4b9ace, 0x75d934e1,\
        0x1804e2a5, 0xce77ee18, 0x12924b1a,  0x3fd4d14, 0x353d42ce, 0xec5ccb5b,\
        0xa5cef8c4, 0x6f8b93c6, 0xb9e06f14, 0x76d680b9, 0x523a46d8,  0x8445cc6,\
        0xbb412e0c, 0x2ae29f22, 0xee3c57b8, 0x45bd2fb

    /* Data structure for the above.
    */
#     ifdef U4_GLOBALS 
        uint32_t U4_sbox[256] = { u4_sbox_data };
#     else
        extern uint32_t U4_sbox[256];
#     endif

    /* Word mixing with 32x32 sbox.
    **
    ** Hardly a secure hash, but suitable for light duty.
    */
#     define u4_sbox_mix(xw) \
        ( U4_sbox[(xw >> 0) & 255] +\
          U4_sbox[(xw >> 8) & 255] +\
          U4_sbox[(xw >> 16) & 255] +\
          U4_sbox[(xw >> 24) & 255] )

    /* A table of bits per byte.
    */
#     define u4_byte_bits_data \
        0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4,\
        5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,\
        \
        6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,\
        6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,\
        \
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,\
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,\
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,\
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,\
        \
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,\
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,\
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,\
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,\
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,\
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,\
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,\
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8

    /* Data structure for the above.
    */
#     ifdef U4_GLOBALS 
        uint8_t U4_byte_bits_table[256] = { u4_byte_bits_data };
#     else
        extern uint8_t U4_byte_bits_table[256];
#     endif

    /* Word and bit measuring macros.
    */
#     define u4_byte_bits(byte) \
        U4_byte_bits_table[byte] 

#     define u4_word_bytes(word) \
        (   ((word) >> 24) ? 4 \
          : ((word) >> 16) ? 3 \
          : ((word) >> 8) ? 2 \
          : ((word) >> 0) ? 1 : 0 )

#     define u4_word_halves(word) \
        (   ((word) >> 16) ? 2 \
          : ((word) >> 0) ? 1 : 0 )

#     define u4_word_bits(word) \
        (    ((word) >> 24) ? (24 + u4_byte_bits((word) >> 24)) \
          : ((word) >> 16) ? (16 + u4_byte_bits((word) >> 16)) \
          : ((word) >> 8) ? (8 + u4_byte_bits((word) >> 8)) \
          : ((word) >> 0) ? u4_byte_bits(word) : 0 )

#     define u4_dword_bytes(dword) \
        (   ((dword) >> 56ULL) ? 8 \
          : ((dword) >> 48ULL) ? 7 \
          : ((dword) >> 40ULL) ? 6 \
          : ((dword) >> 32ULL) ? 5 \
          : ((dword) >> 24ULL) ? 4 \
          : ((dword) >> 16ULL) ? 3 \
          : ((dword) >> 8ULL) ? 2 \
          : ((dword) >> 0ULL) ? 1 : 0 )


  /** Ordinary macros.
  **/
    /* Never.
    */
#     define u4_never u4_assert(!"not reached")

    /* Min and max.
    */
#     define u4_max(x, y) ( ((x) > (y)) ? (x) : (y) )
#     define u4_min(x, y) ( ((x) < (y)) ? (x) : (y) )

    /* Structure field position.
    */
#     define u4_c_pb(type, field) \
        ( ((uint8_t *)&(((type *)0)->field)) - ((uint8_t *)0) )


    /** Useful arithmetic operators.
    **/
      /* Truncation modulo y.
      */
#       define u4_floor(x, y)   ( ((x) / (y)) * (y) )
          
      /* Ceiling modulo y.
      */
#       define u4_cling(x, y)   u4_floor(((x) + ((y) - 1)), (y))

      /* Number of units of y in x.
      */
#       define u4_block(x, y)   ( ((x) + ((y) - 1)) / (y) )

      /* As above, for y = 2^b.
      */
#       define u4_bmask(b)      ( (1U << (b)) - 1 )
#       define u4_bfloor(x, b)  ( ((x) >> (b)) << (b) )
#       define u4_bcling(x, b)  ( u4_bfloor(((x) + u4_bmask(b)), (b)) )
#       define u4_bblock(x, b)  ( ((x) + u4_bmask(b)) >> (b) )


    /** Short string integers.  The logical order is little-endian.
    **/
#     define u4_s1(a)          ( (a) )
#     define u4_s2(a, b)       ( ((b) << 8) | u4_s1(a) )
#     define u4_s3(a, b, c)    ( ((c) << 16) | u4_s2(a, b) )
#     define u4_s4(a, b, c, d) ( ((d) << 24) | u4_s3(a, b, c) )

#     define u4_s5(a, b, c, d, e) \
        ( ((uint64_t)u4_s1(e) << 32ULL) | u4_s4(a, b, c, d) )
#     define u4_s6(a, b, c, d, e, f) \
        ( ((uint64_t)u4_s2(e, f) << 32ULL) | u4_s4(a, b, c, d) )
#     define u4_s7(a, b, c, d, e, f, g) \
        ( ((uint64_t)u4_s3(e, f, g) << 32ULL) | u4_s4(a, b, c, d) )
#     define u4_s8(a, b, c, d, e, f, g, h) \
        ( ((uint64_t)u4_s4(e, f, g, h) << 32ULL) | u4_s4(a, b, c, d) )

