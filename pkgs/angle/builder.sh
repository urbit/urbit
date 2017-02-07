source $stdenv/setup

cp -R $src src

mkdir build
cd build

gyp $gypFlags ../src/src/angle.gyp

ninja -C out/Release -j1

