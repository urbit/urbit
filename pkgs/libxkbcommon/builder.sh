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

# Workaround for static builds being broken.
mv libxkbcommon-x11-internal.a libxkbcommon-x11.a

ninja install
