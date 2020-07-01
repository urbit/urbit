source $setup

tar -xf $src

cd libsigsegv-$version
patch -p1 << 'HEREDOC'
--- a/src/fault-linux-i386.h    2020-06-25 23:46:02.099235491 +0000
+++ b/src/fault-linux-i386.h    2020-06-25 23:45:48.679156892 +0000
@@ -18,6 +18,7 @@

 #include "fault-posix-ucontext.h"

+#define HAVE_STACKVMA 0
 #if defined __x86_64__
 /* 64 bit registers */

HEREDOC
cd ..

mkdir build
cd build

../libsigsegv-$version/configure \
  --host=$host \
  --prefix=$out \
  --enable-static=yes \
  --enable-shared=no

make
make install
