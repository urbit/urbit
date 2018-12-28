source $setup

CFLAGS="-g -O2 -std=c++14 -Wall -Wextra"
CFLAGS="$CFLAGS -I$include_dir"
g++ -c $CFLAGS $src_dir/tapi.cpp $(pkg-config --cflags yaml-0.1) -o tapi.o
ar cr libtapi.a tapi.o

mkdir -p $out/lib/pkgconfig

cp libtapi.a $out/lib
cp -r $include_dir $out/include

cat > $out/lib/pkgconfig/libtapi.pc <<EOF
prefix=$out
libdir=\${prefix}/lib
includedir=\${prefix}/include

Version: 2.0.0
Libs: -L\${libdir} -ltapi
Cflags: -I\${includedir}
Requires: yaml-0.1
EOF

ln -s $libyaml/lib/pkgconfig/yaml-0.1.pc $out/lib/pkgconfig/
