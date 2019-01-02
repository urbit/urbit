#include <ent.h>
#include "config.h"

#if defined(ENT_GETENTROPY)
# include <unistd.h>
# if defined(ENT_GE_SYSRANDOM)
#   include <sys/random.h>
# endif

  int
  ent_getentropy(void* buf, size_t len)
  {
    return getentropy(buf, len);
  }

#elif defined(ENT_IMPL)
# include <assert.h>
# include <errno.h>
# if defined(ENT_GETRANDOM)
#   define _GNU_SOURCE
#   include <unistd.h>
#   include <sys/syscall.h>
#   define ENT_DEFS (void)0
#   define ENT_INIT() (void)0
#   define ENT_READ(B, L) syscall(SYS_getrandom, B, L, 0)
#   define ENT_FINI() (void)0
# elif defined(ENT_URANDOM)
#   include <stdio.h>
#   define ENT_DEFS FILE *f
#   define ENT_INIT() do {                    \
      if (!(f = fopen("/dev/urandom", "re"))) \
        return -1;                            \
    } while (0)
#   define ENT_READ(B, L) fread(B, 1, L, f)
#   define ENT_FINI() (void) fclose(f)
# endif

  int
  ent_getentropy(void* buf, size_t len)
  {
    ENT_DEFS;

    assert(len <= 256);
    ENT_INIT();
    if (len != ENT_READ(buf, len)) {
      ENT_FINI();
      errno = EIO;
      return -1;
    }
    ENT_FINI();
    return 0;
  }

#else
# error "libent: platform not supported"
#endif
