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

mkdir obj_sysroot
mkdir obj_sysroot/include
mkdir obj_sysroot/lib
ln -s . obj_sysroot/usr
ln -s lib obj_sysroot/lib32
ln -s lib obj_sysroot/lib64

mkdir obj_gcc
mkdir obj_musl

gcc_conf="$gcc_conf --with-sysroot=/${host} --with-build-sysroot=$(pwd)/obj_sysroot "
musl_conf="$musl_conf CC=../obj_gcc/gcc/xgcc\ -B\ ../obj_gcc/gcc LIBCC=../obj_gcc/$host/libgcc/libgcc.a"

mkdir -p $out/$host
cp -r --no-preserve=mode $headers/include $out/$host

cd obj_gcc
../src_gcc/configure $gcc_conf
cd ..
make -C obj_gcc all-gcc
cd obj_musl
bash -c "../src_musl/configure $musl_conf"
cd ..
make -C obj_musl DESTDIR=$(pwd)/obj_sysroot install-headers
make -C obj_gcc all-target-libgcc
make -C obj_musl
make -C obj_musl DESTDIR=$(pwd)/obj_sysroot install
make -C obj_gcc
make -C obj_musl DESTDIR=$out/$host install
make -C obj_gcc DESTDIR=$out install
