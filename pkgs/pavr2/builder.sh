source $setup

tar -xf $src
mv pololu-usb-avr-programmer-v2-* pavr2

cd pavr2

cat >> gui/main.cpp <<END
#ifdef QT_STATIC
#include <QtPlugin>
#ifdef _WIN32
Q_IMPORT_PLUGIN (QWindowsIntegrationPlugin);
#endif
#ifdef __linux__
Q_IMPORT_PLUGIN (QLinuxFbIntegrationPlugin);
Q_IMPORT_PLUGIN (QXcbIntegrationPlugin);
#endif
#endif
END

cd ..

mkdir build
cd build

cmake-cross ../pavr2 \
  -DCMAKE_INSTALL_PREFIX=$out

make

make install
