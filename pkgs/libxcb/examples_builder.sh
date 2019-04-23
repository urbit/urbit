source $setup

pkg-config-cross xcb --cflags --libs

$host-gcc -Wall $example1 \
  $(pkg-config-cross xcb --cflags --libs) \
  -o example1$exe_suffix

mkdir -p $out/bin
cp example1$exe_suffix $out/bin/
