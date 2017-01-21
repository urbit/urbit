source $stdenv/setup

# TODO: make this cross-platform
cat > toolchain.txt <<EOF
set(CMAKE_SYSTEM_NAME Windows)
set(CMAKE_C_COMPILER ${host}-gcc)
set(CMAKE_CXX_COMPILER ${host}-gcc)
set(CMAKE_RC_COMPILER ${host}-windres)
EOF

tar -xf $src

cd libusbp-$version
for patch in $patches
do
  echo applying patch $patch
  patch -p1 -i $patch
done
cd ..

mkdir build
cd build

cmake ../libusbp-$version \
  -DCMAKE_TOOLCHAIN_FILE=../toolchain.txt \
  -DCMAKE_INSTALL_PREFIX=$out

make

make install
