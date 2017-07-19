source $stdenv/setup
shopt -u nullglob
unset CC CXX CFLAGS LDFLAGS

tar -xf $gcc_src
mv gcc-* gcc
cd gcc
for patch in $patch_dir/gcc-$gcc_version/*; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

tar -xf $musl_src
mv musl-* musl

gcc_conf="$gcc_conf --prefix=$out"
musl_conf="$musl_conf --prefix=$out/$host CC=../build_gcc/gcc/xgcc\ -B\ ../build_gcc/gcc LIBCC=../build_gcc/$host/libgcc/libgcc.a"

mkdir -p $out/$host

cp -r --no-preserve=mode $headers/include $out/$host

mkdir build_gcc
cd build_gcc
../gcc/configure $gcc_conf
cd ..
make -C build_gcc all-gcc
mkdir build_musl
cd build_musl
bash -c "../musl/configure $musl_conf"
cd ..
make -C build_musl install-headers
make -C build_gcc all-target-libgcc
make -C build_musl
make -C build_musl install
make -C build_gcc
make -C build_gcc install
