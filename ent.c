#include <ent/ent.h>

#if defined(ENT_USE_URANDOM)
# include <assert.h>
# include <fcntl.h>
# include <sys/types.h>
# include <unistd.h>


int
ent_getentropy(void* buf, size_t buflen)
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

#endif /* ENT_USE_URANDOM */
