source $stdenv/setup

shopt -u nullglob

unset CC CXX CFLAGS LDFLAGS

cp --no-preserve=mode -r $scripts scripts

tar -xf $binutils_src
cd binutils-2.27
for patch in ../scripts/patches/binutils-2.27/*; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

tar -xf $gcc_src
cd gcc-6.3.0
for patch in ../scripts/patches/gcc-6.3.0/*; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

tar -xf $gmp_src

tar -xf $linux_src
cd linux-4.4.10
for patch in ../scripts/patches/linux-4.4.10/*; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

tar -xf $mpc_src
tar -xf $mpfr_src
tar -xf $musl_src

mkdir -p build

cp scripts/litecross/Makefile build

cat > build/config.mak <<EOF
TARGET = $TARGET
LINUX_ARCH = $LINUX_ARCH
HOST =
MUSL_SRCDIR = ../musl-1.1.16
GCC_SRCDIR = ../gcc-6.3.0
BINUTILS_SRCDIR = ../binutils-2.27
GMP_SRCDIR = ../gmp-6.1.1
MPC_SRCDIR = ../mpc-1.0.3
MPFR_SRCDIR = ../mpfr-3.1.4
LINUX_SRCDIR = ../linux-4.4.10
EOF

cd build
ln -s ../linux-4.4.10 src_kernel_headers
ln -s ../mpfr-3.1.4 src_mpfr
ln -s ../mpc-1.0.3 src_mpc
ln -s ../gmp-6.1.1 src_gmp
ln -s ../binutils-2.27 src_binutils
ln -s ../gcc-6.3.0 src_gcc
ln -s ../musl-1.1.16 src_musl

mkdir src_toolchain
cd src_toolchain
ln -sf ../src_binutils/* .
ln -sf ../src_gcc/* .
ln -s ../src_gmp gmp
ln -s ../src_mpc mpc
ln -s ../src_mpfr mpfr
cd ..

make
make install OUTPUT=$out
