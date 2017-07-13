source $stdenv/setup

shopt -u nullglob

unset CC CXX CFLAGS LDFLAGS

tar -xf $gcc_src
cd gcc-6.3.0
for patch in $patch_dir/gcc-6.3.0/*; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

tar -xf $linux_src
cd linux-4.4.10
for patch in $patch_dir/linux-4.4.10/*; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

tar -xf $musl_src

mkdir -p build

cd build
ln -s ../linux-4.4.10 src_kernel_headers
ln -s ../gcc-6.3.0 src_gcc
ln -s ../musl-1.1.16 src_musl

mkdir src_toolchain
cd src_toolchain
ln -s ../src_gcc/* .
cd ..

mkdir obj_sysroot
mkdir obj_sysroot/include
mkdir obj_sysroot/lib
ln -s . obj_sysroot/usr
ln -s lib obj_sysroot/lib32
ln -s lib obj_sysroot/lib64

mkdir obj_toolchain
mkdir obj_musl

# TODO: fix xgcc; it searches directories outside of the Nix store for libraries

MAKE="make MULTILIB_OSDIRNAMES= INFO_DEPS= infodir= ac_cv_prog_lex_root=lex.yy.c MAKEINFO=false"
FULL_TOOLCHAIN_CONFIG="$gcc_conf --with-sysroot=/${host} --with-build-sysroot=$(pwd)/obj_sysroot "
FULL_MUSL_CONFIG="$musl_conf CC=../obj_toolchain/gcc/xgcc\ -B\ ../obj_toolchain/gcc LIBCC=../obj_toolchain/$TARGET/libgcc/libgcc.a"
SYSROOT="/$TARGET"
CURDIR=$(pwd)

cd obj_toolchain
../src_toolchain/configure $FULL_TOOLCHAIN_CONFIG
$MAKE MAKE="$MAKE" all-gcc
cd ..
cd obj_musl
bash -c "../src_musl/configure $FULL_MUSL_CONFIG"
$MAKE DESTDIR=$CURDIR/obj_sysroot install-headers
cd ..
$MAKE -C obj_toolchain MAKE="$MAKE enable_shared=no" all-target-libgcc
$MAKE -C obj_musl
$MAKE -C obj_musl DESTDIR=$CURDIR/obj_sysroot install
$MAKE -C obj_toolchain MAKE="$MAKE"
mkdir -p $CURDIR/obj_kernel_headers/staged
$MAKE -C src_kernel_headers ARCH=$LINUX_ARCH O=$CURDIR/obj_kernel_headers INSTALL_HDR_PATH=$CURDIR/obj_kernel_headers/staged headers_install
find obj_kernel_headers/staged/include '(' -name .install -o -name ..install.cmd ')' -exec rm {} +
$MAKE -C obj_musl DESTDIR=$out$SYSROOT install
$MAKE -C obj_toolchain MAKE="$MAKE" DESTDIR=$out install
mkdir -p $out$SYSROOT/include
cp -R obj_kernel_headers/staged/include/* $out$SYSROOT/include

