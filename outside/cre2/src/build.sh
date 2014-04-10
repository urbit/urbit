if [ ! -d "=build" ]; then
mkdir "=build"
(libtoolize || glibtoolize)
sh autogen.sh
cd "=build"
../configure --enable-maintainer-mode LDFLAGS=-pthread
make
cd ..
fi
if [ ! -d "../lib" ]; then
mkdir ../lib
fi
if [ ! -d "../include" ]; then
mkdir ../include
fi
cp \=build/.libs/* ../lib
cp src/cre2.h ../include
