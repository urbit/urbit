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

tar -xf $musl_src
mv musl-$musl_version musl

mkdir -p build

cd build
ln -s ../gcc src_gcc
ln -s ../musl src_musl

gcc_conf="$gcc_conf --prefix=$out --with-sysroot=$out/$host"
musl_conf="$musl_conf --prefix=$out/$host CC=../obj_gcc/gcc/xgcc\ -B\ ../obj_gcc/gcc LIBCC=../obj_gcc/$host/libgcc/libgcc.a"

mkdir -p $out/$host
mkdir $out/$host/include
mkdir $out/$host/lib
ln -s . $out/$host/usr

cp -r --no-preserve=mode $headers/include $out/$host

mkdir obj_gcc
cd obj_gcc
../src_gcc/configure $gcc_conf
cd ..
make -C obj_gcc all-gcc
mkdir obj_musl
cd obj_musl
bash -c "../src_musl/configure $musl_conf"
cd ..
make -C obj_musl install-headers
make -C obj_gcc all-target-libgcc
make -C obj_musl
make -C obj_musl install
make -C obj_gcc
make -C obj_gcc install
