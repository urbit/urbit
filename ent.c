#include <ent/config.h>

/* ent.h undefs these constants and config.h is #pragma once. */

#if defined(ENT_GETRANDOM)
#define _ENT_GETRANDOM
#define _GNU_SOURCE
#endif

#if defined(ENT_URANDOM)
#define _ENT_URANDOM
#endif

#include <ent/ent.h>

#include <assert.h>
#include <errno.h>
#include <unistd.h>

#if defined(_ENT_GETRANDOM)

#include <sys/syscall.h>

#elif defined(_ENT_URANDOM)

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#endif

int
ent_getentropy(void* buf, size_t len)
{
  char *cuf = buf;
  int   ret;
#if defined(_ENT_URANDOM)
  int   fd;
#endif

  assert(len <= 256);
  if (!len)
    return 0;
#if defined(_ENT_URANDOM)
  if ((fd = open("/dev/urandom", O_RDONLY)) < 0)
    return -1;
#endif
  while (len &&
#if defined(_ENT_GETRANDOM)
          (ret = syscall(SYS_getrandom, cuf, len, 0)) != 0
#elif defined(_ENT_URANDOM)
          (ret = read(fd, cuf, len)) != 0
#endif
        ) {
    if (ret < 0) {
      if (errno == EINTR)
        continue;
      break;
    }
    len -= ret;
    cuf += ret;
  }
#if defined(_ENT_URANDOM)
  (void) close(fd);
#endif
  if (!ret) {
    ret = -1;
    errno = EIO;
  }
  return ret < 0 ? ret : 0;
}
