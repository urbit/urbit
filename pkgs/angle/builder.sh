source $stdenv/setup

cp -R $src asrc

chmod -R u+rw asrc

cd asrc
for patch in $patches
do
  echo applying patch $patch
  patch -p1 -i $patch
done
eval "$patchTmphax"
cd ..

mkdir build
cd build

gyp $gypFlags ../asrc/src/angle.gyp

ninja -C out/Release
