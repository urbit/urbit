#ifndef _LIBENT_ENT_H
#define _LIBENT_ENT_H

#include <stddef.h>

#include <ent/detail.h>

ENT_EXPORT
int
ent_getentropy(void *buf, size_t buflen);

#endif /* _LIBENT_ENT_H */
