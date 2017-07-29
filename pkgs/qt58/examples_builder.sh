source $setup

tar -xf $src qtbase-opensource-src-$version/examples

mv qtbase-opensource-src-$version/examples .
rmdir qtbase-opensource-src-$version

mkdir build
cd build
mkdir bin moc obj

# TODO: use $host-pkg-config to get the compiler settings

cat > obj/plugins.cpp <<EOF
#include <QtPlugin>
#ifdef _WIN32
Q_IMPORT_PLUGIN (QWindowsIntegrationPlugin);
#endif
#ifdef __linux__
Q_IMPORT_PLUGIN (QLinuxFbIntegrationPlugin);
#endif
EOF

echo "compiling reference to plugins"
$host-g++ \
  -I $qtbase/include \
  -I $qtbase/include/QtCore \
  -c obj/plugins.cpp \
  -o obj/plugins.o

CFLAGS="
-I.
-I$qtbase/include/
-I$qtbase/include/QtWidgets
-I$qtbase/include/QtGui
-I$qtbase/include/QtCore
"

if [ $os = "windows" ]; then
  CFLAGS="-mwindows $CFLAGS"
fi

LDFLAGS="-L$qtbase/lib -L$qtbase/plugins/platforms -Wl,-gc-sections"

LIBS="$(pkg-config-cross --libs Qt5Widgets)"

if [ $os = "linux" ]; then
  LIBS+="-lqlinuxfb -lQt5FbSupport -lQt5Gui -lQt5InputSupport -lQt5DeviceDiscoverySupport -lQt5EventDispatcherSupport -lQt5FontDatabaseSupport -lQt5ServiceSupport -lQt5Core -lqtfreetype -lqtharfbuzz -lqtlibpng -lqtpcre"
fi

if [ $os = "windows" ]; then
  LIBS+="-lqwindows -lQt5Widgets -lQt5Gui -lQt5ThemeSupport -lQt5FontDatabaseSupport -lQt5EventDispatcherSupport -lQt5Core -lqtpcre -lqtlibpng -lqtharfbuzz  -lqtfreetype -lole32 -luuid -lwinmm -lws2_32 -loleaut32 -limm32 -ldwmapi -lmpr -lwinmm -luxtheme -lopengl32"
fi

echo "compiling dynamiclayouts"
$qtbase/bin/moc ../examples/widgets/layouts/dynamiclayouts/dialog.h > moc/dynamiclayouts.cpp
$host-g++ $CFLAGS $LDFLAGS \
  ../examples/widgets/layouts/dynamiclayouts/dialog.cpp \
  ../examples/widgets/layouts/dynamiclayouts/main.cpp \
  moc/dynamiclayouts.cpp \
  obj/plugins.o \
  $LIBS -o bin/dynamiclayouts$exe_suffix

mkdir $out && cp -r bin $out && exit 0

echo "compiling rasterwindow"
$qtbase/bin/moc ../examples/gui/rasterwindow/rasterwindow.h > moc/rasterwindow.cpp
$host-g++ $CFLAGS $LDFLAGS \
  ../examples/gui/rasterwindow/rasterwindow.cpp \
  ../examples/gui/rasterwindow/main.cpp \
  moc/rasterwindow.cpp \
  obj/plugins.o \
  $LIBS -o bin/rasterwindow$exe_suffix

echo "compiling analogclock"
$host-g++ $CFLAGS $LDFLAGS \
  -I../examples/gui/rasterwindow/ \
  ../examples/gui/analogclock/main.cpp \
  ../examples/gui/rasterwindow/rasterwindow.cpp \
  moc/rasterwindow.cpp \
  obj/plugins.o \
  $LIBS -o bin/analogclock$exe_suffix

echo "compiling openglwindow"
$qtbase/bin/moc ../examples/gui/openglwindow/openglwindow.h > moc/openglwindow.cpp
$host-g++ $CFLAGS $LDFLAGS \
  ../examples/gui/openglwindow/main.cpp \
  ../examples/gui/openglwindow/openglwindow.cpp \
  moc/openglwindow.cpp \
  obj/plugins.o \
  $LIBS -o bin/openglwindow$exe_suffix

# TODO: try to compile some stuff with $qtbase/bin/qmake too, make sure that works

mkdir $out

cp -r bin $out
