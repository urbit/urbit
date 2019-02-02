// Use `getentropy` from unistd.h //////////////////////////////////////////////

#if defined(ENT_GETENTROPY_UNISTD)
#include <stddef.h>
#include <unistd.h>
int ent_getentropy(void* buf, size_t len) {
  return getentropy(buf, len);
}


// Use `getentropy` from sys/random.h //////////////////////////////////////////

#elif defined(ENT_GETENTROPY_SYSRANDOM)
#include <stddef.h>
#include <sys/random.h>
int ent_getentropy(void* buf, size_t len) {
  return getentropy(buf, len);
}


// Use the `getrandom` syscall /////////////////////////////////////////////////

#elif defined(ENT_GETRANDOM_SYSCALL)
#include <stddef.h>
#include <errno.h>
#define _GNU_SOURCE
#include <unistd.h>
#include <sys/syscall.h>

#define ENTFAIL { errno=EIO; return -1; }

int ent_getentropy(void* buf, size_t len) {
  if (len > 256) ENTFAIL
  int r = syscall(SYS_getrandom, buf, len, 0);
  if (r < 0) return r;
  if (r != len) ENTFAIL
  return 0;
}


// Use `/dev/urandom` //////////////////////////////////////////////////////////

#elif defined(ENT_DEV_URANDOM)
#include <stddef.h>
#include <errno.h>
#include <stdio.h>

#define ENTFAIL { errno=EIO; return -1; }

int ent_getentropy(void* buf, size_t len) {
    if (len > 256) ENTFAIL;
    FILE *f = fopen("/dev/urandom", "re");
    if (!f) return -1;
    int r = fread(buf, 1, len, f);
    (void) fclose(f);
    if (r != len) ENTFAIL
    return 0;
}

#else
#error "One of these must be set: ENT_DEV_URANDOM, ENT_GETENTROPY_UNISTD, ENT_GETENTROPY_SYSRANDOM, ENT_GETRANDOM_SYSCALL"
#endif
