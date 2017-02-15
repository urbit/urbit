source $stdenv/setup

cp -r $src/samples .
cp -r $src/util .

mkdir include
cp -r $src/include/export.h include/
cp -r $src/util/* include/
cp -r $src/samples/sample_util/* include/

mkdir build
cd build
mkdir bin lib

$host-g++ -I../include -I"$angle/include" -L"$angle/lib" ../samples/hello_triangle/HelloTriangle.cpp -o bin/hello_triangle
