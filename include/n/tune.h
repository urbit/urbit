/* include/n/tune.h
**
** This file is in the public domain.
*/
  /** Tuning and configuration.
  **/
#   define u3_cc_fbox_no  28

#   define U3_MEMORY_DEBUG
#   ifdef U3_MEMORY_DEBUG
#     define  u3_leak_on(x) (u3_Code = x)
#     define  u3_leak_off  (u3_Code = 0)
#   endif

#   define u3_cc_bits   U2_OS_LoomBits                    // 28, max 29
#   define u3_cc_page   12                                // 16Kbyte pages
#   define u3_cc_pages  (1 << (u3_cc_bits - u3_cc_page))  // 2^16 pages
#   define u3_cc_words  (1 << u3_cc_bits)
#   define u3_cc_bytes  (1 << (2 + u3_cc_bits))
