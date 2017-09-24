source $setup

tar -xf $src
mv cctools-* cctools

mkdir build
cd build

gcc $CFLAGS -c ../cctools/libmacho/*.c

gcc $CFLAGS ../cctools/ld/ld.c -o $host-ld

mkdir -p $out/bin
cp $host-ld $out/bin/
