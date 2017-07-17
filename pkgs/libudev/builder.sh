source $setup

tar -xf $src
mv systemd-* systemd

mkdir build
cd build

touch sd-id128.h

$host-gcc -c \
  -I../systemd/src/libudev \
  -I../systemd/src/basic \
  -I../systemd/src/libsystemd/sd-device \
  -I. \
  ../systemd/src/libudev/*.c
$host-ar cr libudev.a *.o

mkdir -p $out/lib/pkgconfig $out/include
cp libudev.a $out/lib

cat > $out/lib/pkgconfig/libudev.pc <<EOF
hi there
EOF

