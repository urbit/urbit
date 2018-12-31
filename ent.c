#include <ent/config.h>

/* ent.h undefs all constants and config.h is #pragma once. */

#if defined(ENT_GETRANDOM)

#define _ENT_IMPL
#define _GNU_SOURCE
#include <unistd.h>
#include <sys/syscall.h>

#define ENT_DEFS (void)0
#define ENT_INIT() (void)0
#define ENT_READ(B, L) sysall(SYS_getrandom, B, L, 0)
#define ENT_FINI() (void)0

#elif defined(ENT_URANDOM)

#define _ENT_IMPL
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define ENT_DEFS int fd
#define ENT_INIT() do {                           \
  if ((fd = open("/dev/urandom", O_RDONLY)) < 0)  \
    return -1;                                    \
} while (0)
#define ENT_READ(B, L) read(fd, B, L)
#define ENT_FINI() (void) close(fd)

#endif


#include <ent/ent.h>

#if defined(_ENT_IMPL)

#include <assert.h>
#include <errno.h>


int
ent_getentropy(void* buf, size_t len)
{
  char *cuf = buf;
  int   ret;
  ENT_DEFS;

  assert(len <= 256);
  if (!len)
    return 0;
  ENT_INIT();
  while (len && (ret = ENT_READ(cuf, len)) != 0) {
    if (ret < 0) {
      if (errno == EINTR)
        continue;
      break;
    }
    len -= ret;
    cuf += ret;
  }
  ENT_FINI();
  if (!ret) {
    ret = -1;
    errno = EIO;
  }
  return ret < 0 ? ret : 0;
}

#endif /* _ENT_IMPL */
