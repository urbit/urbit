source $stdenv/setup

unset CC

tar -xf $src

cd mingw-w64-v$version
for patch in $patches
do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

mkdir build_crt_and_headers
cd build_crt_and_headers
../mingw-w64-v$version/configure \
    --host=$host --prefix=$out
make
make install
cd ..

mkdir build_winpthreads
cd build_winpthreads
LDFLAGS="-L${out}/lib" ../mingw-w64-v$version/mingw-w64-libraries/winpthreads/configure \
    --disable-shared --enable-static \
    --host=$host --prefix=$out
make
make install
cd ..
