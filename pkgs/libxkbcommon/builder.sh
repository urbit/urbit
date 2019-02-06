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

# Static builds are broken, so we need to replace empty
# static libraries with the real ones here.
mv libxkbcommon-internal.a libxkbcommon.a
mv libxkbcommon-x11-internal.a libxkbcommon-x11.a

ninja install
