source $stdenv/setup

shopt -u nullglob

unset CC CXX CFLAGS LDFLAGS

export TARGET=$host

cp --no-preserve=mode -r $scripts scripts

cd scripts
tar -xf $binutils_src
cd binutils-2.27
for patch in ../patches/binutils-2.27/*; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

tar -xf $gcc_src
cd gcc-6.3.0
for patch in ../patches/gcc-6.3.0/*; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

tar -xf $gmp_src

tar -xf $linux_src
cd linux-4.4.10
for patch in ../patches/linux-4.4.10/*; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

tar -xf $mpc_src
tar -xf $mpfr_src
tar -xf $musl_src

ls -d binutils-2.27 gmp-6.1.1 mpfr-3.1.4 gcc-6.3.0  mpc-1.0.3  musl-1.1.16

make

make install OUTPUT=$out
