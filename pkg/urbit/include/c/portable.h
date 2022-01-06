#ifndef C3_PORTABLE_H
#define C3_PORTABLE_H

#include "config.h"

#   ifndef __GNUC__
#     error "port me"
#   endif
#   ifndef _GNU_SOURCE
#   define _GNU_SOURCE
#   endif


  /** System include files.
  ***
  *** Do not put include files that are only used in the
  *** porting layer here.  Include them directly in the
  *** C file.
  **/
#   if defined(U3_OS_linux)
#     ifndef _XOPEN_SOURCE
#     define _XOPEN_SOURCE 700
#     endif
#     include <inttypes.h>
#     include <stdlib.h>
#     include <string.h>
#     include <stdarg.h>
#     include <unistd.h>
#     include <stdint.h>
#     include <assert.h>
#     include <byteswap.h>
#     include <setjmp.h>
#     include <stdio.h>
#     include <signal.h>
#     include <sys/time.h>
#     include <sys/resource.h>
#     include <sys/mman.h>

#   elif defined(U3_OS_osx)
#     include <inttypes.h>
#     include <stdlib.h>
#     include <string.h>
#     include <stdarg.h>
#     include <unistd.h>
#     include <stdint.h>
#     include <assert.h>
#     include <setjmp.h>
#     include <signal.h>
#     include <machine/endian.h>
#     include <machine/byte_order.h>
#     include <stdio.h>
#     include <sys/time.h>
#     include <sys/resource.h>
#     include <sys/syslimits.h>
#     include <sys/mman.h>

#   elif defined(U3_OS_bsd)
#     include <inttypes.h>
#     include <stdlib.h>
#     include <string.h>
#     include <stdarg.h>
#     include <unistd.h>
#     include <stdint.h>
#     include <assert.h>
#     include <machine/endian.h>
#     include <setjmp.h>
#     include <stdio.h>
#     include <signal.h>
#     include <sys/time.h>
#     include <sys/resource.h>
#     include <sys/mman.h>

#   elif defined(U3_OS_mingw)
#     define signal mingw_has_no_usable_signal
#     define raise  mingw_has_no_usable_raise
#     define _POSIX
#     include <inttypes.h>
#     include <stdlib.h>
#     include <string.h>
#     include <stdarg.h>
#     include <unistd.h>
#     include <stdint.h>
#     include <assert.h>
#     include <setjmp.h>
#     include <stdio.h>
#     include <dirent.h>
#     include <signal.h>
#     include <sys/time.h>
#     include "mman.h"
#     include "compat.h"

#   else
      #error "port: headers"
#   endif

#   ifndef __has_feature
#     define __has_feature(x) 0
#   endif

#   if __has_feature(address_sanitizer) || defined(__SANITIZE_ADDRESS__)
#     define ASAN_ENABLED
#   endif

  /** Address space layout.
  ***
  ***   NB: 2^29 words == 2GB
  **/
#   if defined(U3_OS_linux)
#     ifdef __LP64__
#       ifdef ASAN_ENABLED
#         define U3_OS_LoomBase 0x10007ffff000
#       else
#         define U3_OS_LoomBase 0x200000000
#       endif
#     else
#       define U3_OS_LoomBase 0x36000000
#     endif
#       define U3_OS_LoomBits 29
#   elif defined(U3_OS_mingw)
#       define U3_OS_LoomBase 0x28000000000
#       define U3_OS_LoomBits 29
#   elif defined(U3_OS_osx)
#     ifdef __LP64__
#       define U3_OS_LoomBase 0x28000000000
#     else
#       define U3_OS_LoomBase 0x4000000
#     endif
#       define U3_OS_LoomBits 29
#   elif defined(U3_OS_bsd)
#     ifdef __LP64__
#       define U3_OS_LoomBase 0x200000000
#     else
#       define U3_OS_LoomBase 0x4000000
#     endif
#       define U3_OS_LoomBits 29
#   else
#     error "port: LoomBase"
#   endif

  /** Global variable control.
  ***
  *** To instantiate globals, #define c3_global as extern.
  **/
#   ifndef c3_global
#     define c3_global
#   endif


  /** External, OS-independent library dependencies.
  **/
    /* The GMP (GNU arbitrary-precision arithmetic) library.
    ** (Tested with version 4.0.1.)
    */
#     include <gmp.h>


  /** Private C "extensions."
  ***
  *** Except for these and main(), any function, macro, or structure
  *** names must be prefixed either by u3_/U3_ (for public names),
  *** or _ (for static and other file-local names).
  **/
    /* Endianness.
    */
#     define c3_endian_little     0
#     define c3_endian_big        1

#     ifdef U3_OS_ENDIAN_little
#       define c3_endian c3_endian_little
#     elif defined(U3_OS_ENDIAN_big)
#       define c3_endian c3_endian_big
#     else
#       error "port: U3_OS_ENDIAN"
#     endif

    /* Byte swapping.
    */
#     if defined(U3_OS_linux) || defined(U3_OS_bsd) || defined(U3_OS_mingw)
#       define c3_bswap_16(x)  bswap_16(x)
#       define c3_bswap_32(x)  bswap_32(x)
#       define c3_bswap_64(x)  bswap_64(x)
#     elif defined(U3_OS_osx)
#       define c3_bswap_16(x)  NXSwapShort(x)
#       define c3_bswap_32(x)  NXSwapInt(x)
#       define c3_bswap_64(x)  NXSwapLongLong(x)
#     else
#       error "port: byte swap"
#     endif

    /* Sync.
    */
#     if defined(U3_OS_linux) || defined(U3_OS_mingw)
#       define c3_sync(fd) (fdatasync(fd))
#     elif defined(U3_OS_osx)
#       define c3_sync(fd) (fcntl(fd, F_FULLFSYNC, 0))
#     elif defined(U3_OS_bsd)
#       define c3_sync(fd) (fsync(fd))
#     else
#       error "port: sync"
#     endif

    /* Purge.
    */
#     if defined(U3_OS_linux)
#       include <stdio_ext.h>
#       define c3_fpurge __fpurge
#     elif defined(U3_OS_bsd) || defined(U3_OS_osx) || defined(U3_OS_mingw)
#       define c3_fpurge fpurge
#     else
#       error "port: fpurge"
#     endif

    /* Stat.
    */
#     if defined(U3_OS_linux) || defined(U3_OS_mingw)
#       define c3_stat_mtime(dp) (u3_time_t_in_ts((dp)->st_mtime))
#     elif defined(U3_OS_osx)
#       define c3_stat_mtime(dp) (u3_time_in_ts(&((dp)->st_mtimespec)))
#       define lseek64 lseek
#     elif defined(U3_OS_bsd)
#       define c3_stat_mtime(dp) (u3_time_in_ts(&((dp)->st_mtim)))
#       define lseek64 lseek
#     else
#       error "port: timeconvert"
#     endif

    /* Null.
    */
#     if defined(U3_OS_linux) || defined(U3_OS_bsd) || defined(U3_OS_osx)
#       define c3_dev_null "/dev/null"
#     elif defined(U3_OS_mingw)
#       define c3_dev_null "nul"
#     else
#       error "port: /dev/null"
#     endif

    /* Static assertion.
    */
#     define ASSERT_CONCAT_(a, b) a##b
#     define ASSERT_CONCAT(a, b) ASSERT_CONCAT_(a, b)
#     define STATIC_ASSERT(e,m) \
        ;enum { ASSERT_CONCAT(assert_line_, __LINE__) = 1/(int)(!!(e)) }

#endif /* ifndef C3_PORTABLE_H */
