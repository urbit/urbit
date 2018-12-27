#include <ent.h>

#include <assert.h>

#include "config.h"

#if defined(USE_GETENTROPY)
# include <unistd.h>
# if defined(__APPLE__) || defined(__linux__)
#   include <sys/random.h>
# endif
# define getentropy_impl getentropy
#elif defined(USE_URANDOM)
# include <fcntl.h>
# include <sys/types.h>
# include <unistd.h>
#else
# error "port: getentropy"
#endif


#if defined(USE_URANDOM)

static int
getentropy_impl(void* buf, size_t buflen)
{
  int fd;

  assert(buflen <= 256);
  if (-1 == (fd = open("/dev/urandom", O_RDONLY))) {
    return -1;
  }
  if (buflen != read(fd, buf, buflen)) {
    (void) close(fd);
    return -1;
  }
  close(fd);
  return 0;
}

#endif

int
ent_getentropy(void *buf, size_t buflen)
{
  return getentropy_impl(buf, buflen);
}
