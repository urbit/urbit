source $stdenv/setup

unset CC

cp -r $src mingw-w64
chmod -R u+w mingw-w64

cd mingw-w64
for patch in $patches
do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

mkdir build_crt_and_headers
cd build_crt_and_headers
../mingw-w64/configure \
    --host=$host --prefix=$out
make
make install
cd ..

mkdir build_winpthreads
cd build_winpthreads
LDFLAGS="-L${out}/lib" ../mingw-w64/mingw-w64-libraries/winpthreads/configure \
    --disable-shared --enable-static \
    --host=$host --prefix=$out
make
make install
cd ..
