source $setup

cat > cross.txt <<END
[binaries]
c = '$host-gcc'
cpp = '$host-g++'
ar = '$host-ar'
strip = '$host-strip'
pkgconfig = 'pkg-config-cross'
[host_machine]
system = 'linux'
cpu_family = 'x86'
cpu = 'i686'
endian = 'little'
END
# TODO: hardcoded host machine stuff above is tmphax

tar -xf $src
mv libxkbcommon-* libxkbcommon

mkdir build
cd build

meson . ../libxkbcommon --cross-file ../cross.txt \
  --buildtype release --prefix $out \
  -Denable-wayland=false \
  -Denable-docs=false
ninja

ninja install
