source $setup

header_dirs="
architecture
i386
libkern
mach
machine
mach-o
mach_debug
"

headers="
libunwind.h
Availability.h
AvailabilityMacros.h
AvailabilityInternal.h
sys/_endian.h
sys/_types.h
sys/_pthread/_pthread_types.h
sys/_types/_mach_port_t.h
sys/_types/_os_inline.h
sys/appleapiopts.h
"

for dir in $header_dirs; do
  d=$out/include/$(dirname $dir)/
  mkdir -p $d
  cp -r --no-preserve=mode $sdk/usr/include/$dir $d
done

for header in $headers; do
  mkdir -p $out/include/$(dirname $header)
  cp $sdk/usr/include/$header $out/include/$header
done

cat > $out/include/mach/mach_time.h <<EOF
// We need definitions for these functions.

#pragma once

#include <stdint.h>
#include <sys/time.h>
#include <mach/mach_types.h>

struct mach_timebase_info {
  uint32_t numer;
  uint32_t denom;
};

typedef struct mach_timebase_info * mach_timebase_info_t;
typedef struct mach_timebase_info mach_timebase_info_data_t;

kern_return_t mach_timebase_info(mach_timebase_info_t info)
{
  info->numer = 1000;
  info->denom = 1;
  return 0;
}

static inline uint64_t mach_absolute_time(void)
{
  struct timeval tv;
  if (gettimeofday(&tv, NULL)) { return 0; }
  return (uint64_t)tv.tv_sec * 1000000 + tv.tv_usec;
}
EOF

cat > $out/include/i386/_types.h <<EOF
// The SDK version defines things like __int64_t, causing errors.

#pragma once
#include <bits/types.h>
#include <sys/cdefs.h>
typedef long __darwin_intptr_t;
typedef unsigned int __darwin_natural_t;
EOF

cat > $out/include/i386/limits.h <<EOF
// The SDK version defines MB_LEN_MAX, which causes errors.

#pragma once
#include <limits.h>

#if !defined(_ANSI_SOURCE)
#if (!defined(_POSIX_C_SOURCE) && !defined(_XOPEN_SOURCE)) || defined(_DARWIN_C_SOURCE)
#define SIZE_T_MAX ULONG_MAX
#endif
#endif
EOF

cat > $out/include/string.h <<EOF
// MacOS programs expect string.h to define these.

#pragma once
#include_next <string.h>
#include <stdint.h>
#include <limits.h>
#include <i386/limits.h>

static inline size_t
strlcat(char * __restrict__ dst, const char * __restrict__ src, size_t size)
{
  size_t srclen = strnlen(src, size);
  if (!size) { return srclen; }
  size_t dstlen = strnlen(dst, size);

  size_t wantlen = SIZE_T_MAX;
  if (SIZE_T_MAX - srclen > dstlen)
  {
    wantlen = dstlen + srclen;
  }

  if (dstlen > size - 1) { return wantlen; }

  size_t cpylen = srclen;
  if (wantlen > size - 1) { cpylen = size - dstlen - 1; }

  memcpy(dst + dstlen, src, cpylen);
  dst[dstlen + cpylen] = 0;

  return wantlen;
}

static inline size_t
strlcpy(char * __restrict__ dst, const char * __restrict__ src, size_t size)
{
  if (!size) { return 0; /** strlen is not safe **/ }
  dst[0] = 0;
  return strlcat(dst, src, size);
}
EOF

# The MacOS SDK expects sys/cdefs.h to define __unused as an attribute.  But we
# can't have that definition here because glibc's linux/sysctl.h uses __unused as a
# variable name.  Instead, we just fix the SDK to not use __unused.
sed -i -r 's/\b__unused\b//g' $out/include/mach/mig_errors.h
