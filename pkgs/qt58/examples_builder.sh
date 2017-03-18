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

CFLAGS="-mwindows"
CFLAGS="$CFLAGS -I. -I$qtbase/include/ -I$qtbase/include/QtGui -I$qtbase/include/QtCore"

LDFLAGS="-L$qtbase/lib -L$qtbase/plugins/platforms"

# TODO: make this junk come from $host-pkg-config, so that it is cross-platform
LIBS="
-lqwindows
-lQt5Gui -lQt5ThemeSupport -lQt5FontDatabaseSupport
-lQt5EventDispatcherSupport -lQt5Core
-lqtpcre -lqtlibpng -lqtharfbuzz  -lqtfreetype
-lole32 -luuid -lwinmm -lws2_32 -loleaut32 -limm32 -ldwmapi -lmpr -lwinmm
"

echo "compiling rasterwindow"
$qtbase/bin/moc ../examples/gui/rasterwindow/rasterwindow.h > moc/rasterwindow.cpp
$host-g++ \
  -mwindows \
  $CFLAGS \
  $LDFLAGS \
  ../examples/gui/rasterwindow/rasterwindow.cpp \
  ../examples/gui/rasterwindow/main.cpp \
  moc/rasterwindow.cpp \
  obj/plugins.o \
  $LIBS \
  -o bin/rasterwindow${exe_suffix}

$host-strip bin/*

mkdir $out

cp -r bin $out
# TODO: cp -r $qtbase/license $out
