source $stdenv/setup

tar -xf $src qtbase-opensource-src-$version/examples

mv qtbase-opensource-src-$version/examples .
rmdir qtbase-opensource-src-$version

mkdir build
cd build
mkdir bin moc obj

# TODO: use $host-pkg-config to get the compiler settings

cat > obj/plugins.cpp <<EOF
#include <QtPlugin>
Q_IMPORT_PLUGIN (QWindowsIntegrationPlugin);
EOF

echo "compiling reference to plugins"
$host-g++ \
  -I $qtbase/include \
  -I $qtbase/include/QtCore \
  -c obj/plugins.cpp \
  -o obj/plugins.o

echo "compiling rasterwindow"
$qtbase/bin/moc ../examples/gui/rasterwindow/rasterwindow.h > moc/rasterwindow.cpp
$host-g++ \
  -mwindows \
  -I . \
  -I $qtbase/include/ \
  -I $qtbase/include/QtGui \
  -I $qtbase/include/QtCore \
  -L $qtbase/lib \
  -L $qtbase/plugins/platforms \
  ../examples/gui/rasterwindow/rasterwindow.cpp \
  ../examples/gui/rasterwindow/main.cpp \
  moc/rasterwindow.cpp \
  obj/plugins.o \
  -lqwindows -lqminimal -lqdirect2d -lqoffscreen \
  -lQt5Gui -lQt5ThemeSupport -lQt5FontDatabaseSupport \
  -lQt5EventDispatcherSupport -lQt5Core \
  -lqtpcre -lqtlibpng -lqtharfbuzz  -lqtfreetype \
  -lole32 -luuid -lwinmm -lws2_32 -loleaut32 -limm32 -ldwmapi -lmpr -lwinmm \
  -o bin/rasterwindow${exe_suffix}

mkdir $out

cp -r bin $out
# TODO: cp -r $qtbase/license $out
