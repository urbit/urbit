/* include/n/tune.h
**
** This file is in the public domain.
*/
  /** Tuning and configuration.
  **/
#   define u3_cc_fbox_no  28

#   define U3_MEMORY_DEBUG
#   ifdef U3_MEMORY_DEBUG
#     define  u3_leak_on(x) (u2_Code = x)
#     define  u3_leak_off  (u2_Code = 0)
#   endif
