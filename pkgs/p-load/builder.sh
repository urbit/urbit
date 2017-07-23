source $setup

tar -xf $src
mv p-load-* p-load

mkdir build
cd build

# TODO: why is this necessary?  What .so is p-load trying to load and why?
export LDFLAGS="-static"

cmake-cross ../p-load \
  -DCMAKE_INSTALL_PREFIX=$out

make

make install
