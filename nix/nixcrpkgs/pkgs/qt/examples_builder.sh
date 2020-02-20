source $setup

examples=$qtbase/src/examples

mkdir build
cd build
mkdir bin moc obj

cat > obj/plugins.cpp <<EOF
#include <QtPlugin>
#ifdef _WIN32
Q_IMPORT_PLUGIN(QWindowsIntegrationPlugin)
#if QT_VERSION >= QT_VERSION_CHECK(5, 10, 0)
Q_IMPORT_PLUGIN(QWindowsVistaStylePlugin)
#endif
#endif
#ifdef __linux__
Q_IMPORT_PLUGIN(QLinuxFbIntegrationPlugin)
Q_IMPORT_PLUGIN(QXcbIntegrationPlugin)
#endif
#ifdef __APPLE__
Q_IMPORT_PLUGIN(QCocoaIntegrationPlugin)
#endif
EOF

CFLAGS="-std=gnu++11"

echo "compiling reference to plugins"
$host-g++ $CFLAGS \
  $(pkg-config-cross --cflags Qt5Core) \
  -c obj/plugins.cpp \
  -o obj/plugins.o

CFLAGS="$CFLAGS -g -I. $(pkg-config-cross --cflags Qt5Widgets)"
LIBS="$(pkg-config-cross --libs Qt5Widgets)"
LDFLAGS=""

if [ $os = "windows" ]; then
  CFLAGS="-mwindows $CFLAGS"
fi

echo "compiling dynamiclayouts"
$qtbase/bin/moc $examples/widgets/layouts/dynamiclayouts/dialog.h > moc/dynamiclayouts.cpp
$host-g++ $CFLAGS $LDFLAGS \
  $examples/widgets/layouts/dynamiclayouts/dialog.cpp \
  $examples/widgets/layouts/dynamiclayouts/main.cpp \
  moc/dynamiclayouts.cpp \
  obj/plugins.o \
  $LIBS -o bin/dynamiclayouts$exe_suffix

echo "compiling rasterwindow"
$qtbase/bin/moc $examples/gui/rasterwindow/rasterwindow.h > moc/rasterwindow.cpp
$host-g++ $CFLAGS $LDFLAGS \
  $examples/gui/rasterwindow/rasterwindow.cpp \
  $examples/gui/rasterwindow/main.cpp \
  moc/rasterwindow.cpp \
  obj/plugins.o \
  $LIBS -o bin/rasterwindow$exe_suffix

echo "compiling analogclock"
$host-g++ $CFLAGS $LDFLAGS \
  -I$examples/gui/rasterwindow/ \
  $examples/gui/analogclock/main.cpp \
  $examples/gui/rasterwindow/rasterwindow.cpp \
  moc/rasterwindow.cpp \
  obj/plugins.o \
  $LIBS -o bin/analogclock$exe_suffix

# We haven't gotten OpenGL support to work on Linux yet (TODO)
if [ $os != "linux" ]; then
  echo "compiling openglwindow"
  $qtbase/bin/moc $examples/gui/openglwindow/openglwindow.h > moc/openglwindow.cpp
  $host-g++ $CFLAGS $LDFLAGS \
    $examples/gui/openglwindow/main.cpp \
    $examples/gui/openglwindow/openglwindow.cpp \
    moc/openglwindow.cpp \
    obj/plugins.o \
   $LIBS -o bin/openglwindow$exe_suffix
fi

mkdir -p $out/bin

for prog in analogclock dynamiclayouts openglwindow rasterwindow; do
  if [ -f bin/$prog$exe_suffix ]; then
    $host-strip bin/$prog$exe_suffix
    cp bin/$prog$exe_suffix $out/bin/
  fi
done

if [ $os = "linux" ]; then
  cp $dejavu/ttf/DejaVuSans.ttf $out/bin/
fi
