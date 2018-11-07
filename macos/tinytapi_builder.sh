source $setup

cp $src_h tapi.h
cp $src_cpp tapi.cpp

g++ -c -Wall -Wextra tapi.cpp $(pkg-config --cflags yaml-0.1) -o tapi.o
ar cr libtapi.a tapi.o

mkdir -p $out/lib/pkgconfig $out/include/tapi

cp libtapi.a $out/lib
cp tapi.h $out/include/tapi

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
