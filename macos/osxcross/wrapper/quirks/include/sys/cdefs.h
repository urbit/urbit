/*
 * Workaround for a GLIBC bug.
 * https://sourceware.org/bugzilla/show_bug.cgi?id=14952
 */

#include_next <sys/cdefs.h>

#ifdef __GLIBC__

#ifndef __extern_inline
# define __extern_inline \
  extern __inline __attribute__ ((__gnu_inline__))
#endif

#ifndef __extern_always_inline
# define __extern_always_inline \
  extern __always_inline __attribute__ ((__gnu_inline__))
#endif

#endif /* __GLIBC__ */
