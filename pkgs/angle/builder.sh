source $stdenv/setup

cp -R $src src

chmod -R u+rw src

cd src
for patch in $patches
do
  echo applying patch $patch
  patch -p1 -i $patch
done
eval "$patchTmphax"
cd ..

mkdir build
cd build

gyp $gypFlags ../src/src/angle.gyp

ninja -C out/Release -j1 src/src/common/angle_common.string_utils.o

