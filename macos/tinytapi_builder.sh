source $setup

tar -xf $src
mv tinytapi-* tinytapi

mkdir build
cd build

CFLAGS="-g -O2 -std=c++14 -Wall -Wextra"
CFLAGS="$CFLAGS -I../tinytapi/include"
g++ -c $CFLAGS ../tinytapi/src/tapi.cpp $(pkg-config --cflags yaml-0.1) -o tapi.o
ar cr libtapi.a tapi.o

mkdir -p $out/lib/pkgconfig

cp libtapi.a $out/lib
cp -r ../tinytapi/include $out/include

cat > $out/lib/pkgconfig/libtapi.pc <<EOF
prefix=$out
libdir=\${prefix}/lib
includedir=\${prefix}/include

Name: libtapi
Version: 2.0.0
Libs: -L\${libdir} -ltapi
Cflags: -I\${includedir}
Requires: yaml-0.1
EOF

ln -s $libyaml/lib/pkgconfig/yaml-0.1.pc $out/lib/pkgconfig/
