source $setup

cp -r $src mingw-w64
chmod -R u+w mingw-w64

cd mingw-w64
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

if [ -n "$just_headers" ]; then
  mkdir build_headers
  cd build_headers
  ../mingw-w64/mingw-w64-headers/configure --prefix=$out $configure_flags
  make
  make install
  cd ..
else
  mkdir build_crt_and_headers
  cd build_crt_and_headers
  ../mingw-w64/configure --prefix=$out $configure_flags
  make
  make install
  cd ..

  mkdir build_winpthreads
  cd build_winpthreads
  LDFLAGS="-L${out}/lib" ../mingw-w64/mingw-w64-libraries/winpthreads/configure \
      --host=$host --prefix=$out --disable-shared --enable-static
  make
  make install
  cd ..
fi
