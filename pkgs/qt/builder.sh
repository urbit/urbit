source $setup

mkdir -p $out
pushd $out
tar -xf $src
mv qtbase-opensource-src-* src
cd src
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
popd

mkdir build
cd build

PKG_CONFIG=pkg-config-cross \
$out/src/configure -prefix $out $configure_flags

make

make install

