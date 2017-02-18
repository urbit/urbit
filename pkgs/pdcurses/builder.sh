source $stdenv/setup

tar -xf $src

mkdir build
cd build

source_files=../PDCurses-$version/pdcurses/*.c

if [ "$os" == "windows" ]; then
  source_files="$source_files ../PDCurses-$version/win32/*.c"
fi

for s in $source_files; do
  echo "compiling $s"
  ${host}-gcc -O2 -Wall -I../PDCurses-$version \
    -DPDC_WIDE -DPDC_FORCE_UTF8 -c "$s" -o "$(basename $s).o"
done

${host}-ar r libpdcurses.a *.o

mkdir -p $out/{lib,include}
mv libpdcurses.a $out/lib/
cd ../PDCurses-$version
cp curses.h panel.h term.h $out/include/
