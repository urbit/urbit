source $setup

tar -xf $src
mv gcc-* gcc
cd gcc
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

tar -xf $musl_src
mv musl-* musl

mkdir -p $out/$host
cp -r --no-preserve=mode $headers/include $out/$host

mkdir build_gcc
cd build_gcc
../gcc/configure --prefix=$out $gcc_conf
cd ..
make -C build_gcc all-gcc
mkdir build_musl
cd build_musl
../musl/configure --prefix=$out/$host $musl_conf \
  CC="../build_gcc/gcc/xgcc -B ../build_gcc/gcc" \
  LIBCC=../build_gcc/$host/libgcc/libgcc.a
cd ..
make -C build_musl install-headers
make -C build_gcc all-target-libgcc
make -C build_musl
make -C build_musl install
make -C build_gcc
make -C build_gcc install
