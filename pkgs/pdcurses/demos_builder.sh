source $setup

tar -xf $src
mv PDCurses-$version/demos .
rm -r PDCurses-$version

mkdir build
cd build

CFLAGS="-g -O2 -I$pdcurses/include -DPDC_WIDE"

$host-gcc $CFLAGS -c ../demos/tui.c -o tui.o
$host-ar r tui.a tui.o

demos="firework newdemo ptest rain testcurs worm xmas tuidemo"

for name in $demos; do
  src=../demos/$name.c
  echo "compiling $name"
  $host-gcc $CFLAGS -L"$pdcurses/lib" \
    "$src" tui.a -lpdcurses -o "$name.exe"
done

mkdir -p $out/bin
mv *.exe $out/bin/
