source $stdenv/setup

tar -xf $src qtbase-opensource-src-$version/examples

mv qtbase-opensource-src-$version/examples .
rmdir qtbase-opensource-src-$version

mkdir build
cd build
mkdir bin moc

# TODO: use $host-pkg-config to get the compiler settings

echo "compiling rasterwindow"
$qtbase/bin/moc ../examples/gui/rasterwindow/rasterwindow.h > moc/rasterwindow.cpp
$host-g++ \
  -I . \
  -I $qtbase/include/ \
  -I $qtbase/include/QtGui \
  -L $qtbase/lib \
  -L $qtbase/plugins/platforms \
  ../examples/gui/rasterwindow/rasterwindow.cpp \
  ../examples/gui/rasterwindow/main.cpp \
  moc/rasterwindow.cpp \
  -lQt5Gui -lQt5Core \
  -lmpr -lwinmm -lqtpcre -lqtlibpng -lqtharfbuzz \
  -lole32 -luuid -lwinmm -lws2_32 \
  -lqwindows -lqminimal -lqdirect2d -lqoffscreen \
  -o bin/rasterwindow${exe_suffix}

mkdir $out

cp -r bin $out
# TODO: cp -r $qtbase/license $out
