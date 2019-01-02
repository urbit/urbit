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

  static int
  _ent_fail()
  {
    errno = EIO;
    return -1;
  }

  int
  ent_getentropy(void* buf, size_t len)
  {
    int r;
    ENT_DEFS;

    if (len > 256)
      return _ent_fail();
    ENT_INIT();
    r = ENT_READ(buf, len);
    ENT_FINI();
    if (r != len)
      return _ent_fail();
    return 0;
  }

#else
# error "libent: platform not supported"
#endif
