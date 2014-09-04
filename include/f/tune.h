/* include/f/tune.h
**
** This file is in the public domain.
*/
  /** Tuning and configuration.
  **/
#   define u2_cc_fbox_no  28

#   undef U2_MEMORY_DEBUG
#   ifdef U2_MEMORY_DEBUG
#     define  u2_leak_on(x) (COD_w = x)
        extern  c3_w COD_w;
#     define  u2_leak_off  (COD_w = 0)
#   endif
