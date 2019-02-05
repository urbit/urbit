source $setup

tar -xf $src
mv libxkbcommon-* libxkbcommon

mkdir build
cd build

meson-cross . ../libxkbcommon \
  --buildtype release --prefix $out \
  -Denable-wayland=false \
  -Denable-docs=false
ninja

ninja install
