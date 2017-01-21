source $stdenv/setup

cat > toolchain.txt <<EOF
set(CMAKE_SYSTEM_NAME Windows)
set(CMAKE_C_COMPILER ${host}-gcc)
set(CMAKE_CXX_COMPILER ${host}-gcc)
EOF

tar -xf $src

mkdir build
cd build

cmake ../libusbp-$version -DCMAKE_TOOLCHAIN_FILE=../toolchain.txt

make

make install DESTDIR=$out
