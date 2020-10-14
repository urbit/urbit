source $setup

tar -xf $src

cd libsigsegv-$version
patch -p1 << 'PATCH_I386'
--- a/src/fault-linux-i386.h    2020-06-25 23:46:02.099235491 +0000
+++ b/src/fault-linux-i386.h    2020-06-25 23:45:48.679156892 +0000
@@ -18,6 +18,7 @@

 #include "fault-posix-ucontext.h"

+#define HAVE_STACKVMA 0
 #if defined __x86_64__
 /* 64 bit registers */

PATCH_I386
patch -p1 << 'PATCH_ARM'
--- a/src/fault-linux-arm.h
+++ b/src/fault-linux-arm.h
@@ -17,6 +17,7 @@

 #include "fault-posix-ucontext.h"

+#define HAVE_STACKVMA 0
 #if defined(__aarch64__) || defined(__ARM_64BIT_STATE) || defined(__ARM_PCS_AAPCS64) /* 64-bit */

 /* See glibc/sysdeps/unix/sysv/linux/aarch64/sys/ucontext.h.
 
PATCH_ARM
cd ..

mkdir build
cd build

# Hack
if [ $host = aarch64-linux-musleabi ]
then
  sed -i 's/^CFG_FAULT=$/CFG_FAULT=fault-linux-arm.h/' \
    ../libsigsegv-$version/configure
fi

../libsigsegv-$version/configure \
  --host=$host \
  --prefix=$out \
  --enable-static=yes \
  --enable-shared=no

make
make install
