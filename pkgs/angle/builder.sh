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

# ANGLE's gyp build files do not handle out-of-tree builds properly,
# so let's just do an in tree build.

gyp $gypFlags src/angle.gyp

ninja -C out/Release
