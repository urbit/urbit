source $setup

tar -xf $src
mv libxkbcommon-* libxkbcommon

mkdir build
cd build

meson-cross . ../libxkbcommon \
  --prefix $out \
  --buildtype release \
  --default-library static \
  -Denable-wayland=false \
  -Denable-docs=false
ninja

ninja install
