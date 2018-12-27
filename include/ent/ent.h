#ifndef _ENT_H
#define _ENT_H

#include <ent/config.h>

#if defined(ENT_GETENTROPY)
# if defined(ENT_GE_SYSRANDOM)
#   include <stddef.h>
#   include <inttypes.h>  /* OSX sys/random.h needs Availability.h from this */
#   include <sys/random.h>
# elif defined(ENT_GE_UNISTD)
#   include <unistd.h>
# else
#   error "libent: this shouldn't happen"
# endif
# define ent_getentropy getentropy
#elif defined(ENT_URANDOM)

/*
 * Fills buf with high-quality entropy.
 *
 * buflen is the number of bytes, no greater than 256.
 *
 * Returns 0 on success. On failure, returns -1 and sets errno to
 * indicate the error.
 */
int
ent_getentropy(void* buf, size_t buflen);

#else
# error "libent: platform not supported"
#endif

#endif /* _ENT_H */
