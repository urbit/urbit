source $setup

tar -xf $src
mv systemd-* systemd

cd systemd
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

size_flags="-DSIZEOF_PID_T=4 -DSIZEOF_UID_T=4 -DSIZEOF_GID_T=4 \
-DSIZEOF_TIME_T=4 -DSIZEOF_RLIM_T=8 -DSIZEOF_INO_T=8 -DSIZEOF_DEV_T=8"
$host-g++ -x c++ -c $size_flags - -o test.o <<EOF
#include <type_traits>
#include <sys/types.h>
#include <sys/resource.h>
static_assert(sizeof(pid_t) == SIZEOF_PID_T);
static_assert(sizeof(uid_t) == SIZEOF_UID_T);
static_assert(sizeof(gid_t) == SIZEOF_GID_T);
static_assert(sizeof(time_t) == SIZEOF_TIME_T);
static_assert(sizeof(rlim_t) == SIZEOF_RLIM_T);
static_assert(sizeof(dev_t) == SIZEOF_DEV_T);
static_assert(sizeof(ino_t) == SIZEOF_INO_T);
EOF
rm test.o

mkdir build
cd build

# -DHAVE_SECURE_GETENV: We don't have secure_getenv but we want to avoid a header error,
# and hopefully secure_getenv isn't actually needed by libudev.

$host-gcc -c -I$fill $fill/*.c
$host-gcc -c \
  -D_GNU_SOURCE \
  $size_flags \
  -I../systemd/src/libudev \
  -I../systemd/src/basic \
  -I../systemd/src/libsystemd/sd-device \
  -I../systemd/src/libsystemd/sd-hwdb \
  -I../systemd/src/systemd \
  ../systemd/src/libudev/*.c
$host-gcc -c \
  -D_GNU_SOURCE \
  $size_flags \
  -I../systemd/src/libsystemd/sd-device \
  -I../systemd/src/basic \
  -I../systemd/src/systemd \
  ../systemd/src/libsystemd/sd-device/device-enumerator.c
$host-gcc -c \
  -D_GNU_SOURCE \
  $size_flags \
  -I../systemd/src/basic \
  -I../systemd/src/systemd \
  -I$fill \
  ../systemd/src/basic/{prioq,log}.c
$host-ar cr libudev.a *.o

mkdir -p $out/lib/pkgconfig $out/include
cp libudev.a $out/lib/
cp ../systemd/src/libudev/libudev.h $out/include/

cat > $out/lib/pkgconfig/libudev.pc <<EOF
prefix=$out
libdir=\${prefix}/lib
includedir=\${prefix}/include

Name: libudev
Version: $version
Libs: -L\${libdir} -ludev
Cflags: -I\${includedir}
EOF

