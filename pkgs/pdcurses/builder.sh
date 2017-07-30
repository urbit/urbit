source $setup

tar -xf $src
mv PDCurses-* pdcurses

mkdir build
cd build

source_files=../pdcurses/pdcurses/*.c

if [ "$os" == "windows" ]; then
  os_files=../pdcurses/win32/*.c
fi

if [ "$os" == "linux" ]; then
  os_files=
fi

source_files="$source_files $os_files"

for s in $source_files; do
  echo "compiling $s"
  $host-gcc -g -O2 -I../pdcurses \
    -DPDC_WIDE -DPDC_FORCE_UTF8 -c "$s" -o "$(basename $s).o"
done

$host-ar r libpdcurses.a *.o
$host-ranlib libpdcurses.a

mkdir -p $out/{lib,include}
cp libpdcurses.a $out/lib/libpdcurses.a

# Make libcurses.a so programs like GDB can find pdcurses.
ln -s $out/lib/libpdcurses.a $out/lib/libcurses.a

cd ../pdcurses
cp curses.h panel.h term.h $out/include/
