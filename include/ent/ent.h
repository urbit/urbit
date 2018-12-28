/* Copyright 2018 Steven Dee. All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#pragma once

#include <stddef.h>

#include <ent/config.h>

#if defined(ENT_GETENTROPY)
# if defined(ENT_GE_SYSRANDOM)
#   include <inttypes.h>  /* OSX sys/random.h needs Availability.h from this */
#   include <sys/random.h>
# elif defined(ENT_GE_UNISTD)
#   include <unistd.h>
# else
#   error "libent: this shouldn't happen"
# endif
# define ent_getentropy getentropy
#elif defined(ENT_URANDOM)

/* Fills buf with high-quality entropy.
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

#ifdef ENT_GETENTROPY
#undef ENT_GETENTROPY
#endif

#ifdef ENT_GE_SYSRANDOM
#undef ENT_GE_SYSRANDOM
#endif

#ifdef ENT_GE_UNISTD
#undef ENT_GE_UNISTD
#endif
