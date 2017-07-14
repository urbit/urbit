source $stdenv/setup

shopt -u nullglob

unset CC CXX CFLAGS LDFLAGS

tar -xf $gcc_src
mv gcc-$gcc_version gcc
cd gcc
for patch in $patch_dir/gcc-$gcc_version/*; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

tar -xf $linux_src
mv linux-$linux_version linux

tar -xf $musl_src
mv musl-$musl_version musl

mkdir -p build

cd build
ln -s ../linux src_linux
ln -s ../gcc src_gcc
ln -s ../musl src_musl

mkdir obj_sysroot
mkdir obj_sysroot/include
mkdir obj_sysroot/lib
ln -s . obj_sysroot/usr
ln -s lib obj_sysroot/lib32
ln -s lib obj_sysroot/lib64

mkdir obj_gcc
mkdir obj_musl

MAKE="make MULTILIB_OSDIRNAMES= ac_cv_prog_lex_root=lex.yy.c"
gcc_conf="$gcc_conf --with-sysroot=/${host} --with-build-sysroot=$(pwd)/obj_sysroot "
musl_conf="$musl_conf CC=../obj_gcc/gcc/xgcc\ -B\ ../obj_gcc/gcc LIBCC=../obj_gcc/$TARGET/libgcc/libgcc.a"

cd obj_gcc
../src_gcc/configure $gcc_conf
cd ..
$MAKE -C obj_gcc MAKE="$MAKE" all-gcc
cd obj_musl
bash -c "../src_musl/configure $musl_conf"
cd ..
$MAKE -C obj_musl DESTDIR=$(pwd)/obj_sysroot install-headers
$MAKE -C obj_gcc MAKE="$MAKE" all-target-libgcc
$MAKE -C obj_musl
$MAKE -C obj_musl DESTDIR=$(pwd)/obj_sysroot install
$MAKE -C obj_gcc MAKE="$MAKE"
mkdir -p obj_kernel_headers/staged
$MAKE -C src_linux ARCH=$LINUX_ARCH O=$(pwd)/obj_kernel_headers INSTALL_HDR_PATH=$(pwd)/obj_kernel_headers/staged headers_install
find obj_kernel_headers/staged/include '(' -name .install -o -name ..install.cmd ')' -exec rm {} +
$MAKE -C obj_musl DESTDIR=$out$SYSROOT install
$MAKE -C obj_gcc MAKE="$MAKE" DESTDIR=$out install
mkdir -p $out$SYSROOT/include
cp -R obj_kernel_headers/staged/include/* $out$SYSROOT/include
