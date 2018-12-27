#include <ent/ent.h>

#if defined(ENT_URANDOM)

#include <assert.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>


int
ent_getentropy(void* buf, size_t len)
{
  int     fd;
  ssize_t ret;
  char*   cuf;

  assert(len <= 256);
  if (-1 == (fd = open("/dev/urandom", O_RDONLY))) {
    return -1;
  }
  while (len != 0 && (ret = read(fd, cuf, len)) != 0) {
    if (ret == -1) {
      if (errno == EINTR)
        continue;
      break;
    }
    len -= ret;
    cuf += ret;
  }
  (void) close(fd);
  if (ret == 0) {
    ret = -1;
    errno = EIO;
  }
  return ret < 0 ? ret : 0;
}

#endif /* ENT_USE_URANDOM */
