source $setup

tar -xf $src
mv xcb-demo-* demo

# There has been some bit rot.  Prepare a directory structure that matches what
# this code expects.
mkdir -p headers/X11/XCB
touch headers/X11/Xlib.h
for input in $cross_inputs; do
  if [ -d $input/include/xcb ]; then
    ln -s $input/include/xcb/* headers/X11/XCB
  fi
done

mkdir build
cd build

CFLAGS="-I $(realpath ../headers)" \
PKG_CONFIG=pkg-config-cross \
../demo/configure --prefix=$out $configure_flags

make

make install
