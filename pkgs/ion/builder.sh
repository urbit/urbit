source $setup

tar -xf $src
mv bitwise-* bitwise

mkdir build
cd build

$host-gcc -O2 ../bitwise/ion/main.c -o ion$exe_suffix \
   -DIONHOME=\"$out/ionhome\"

# TODO: make -DIONHOME actually work

mkdir $out

mkdir $out/bin
mv ion$exe_suffix $out/bin/

mkdir $out/ionhome
mv ../bitwise/ion/system_packages $out/ionhome/
