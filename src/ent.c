#include <ent/ent.h>

#if defined __OPENBSD__ || (defined __linux__ && defined _DEFAULT_SOURCE)
# include <unistd.h>
# define getentropy_impl getentropy
#elif defined __APPLE__ && defined __MACH__
# include <sys/random.h>
# define getentropy_impl getentropy
#else
# error "Port: getentropy unimplemented"
#endif

int
ent_getentropy(void *buf, size_t buflen)
{
  return getentropy_impl(buf, buflen);
}
