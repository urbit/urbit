source $stdenv/setup

cp --no-preserve=mode -r $src/usb/usbview .

cd usbview
rm usbschema.hpp xmlhelper.cpp
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cp $my_xmlhelper_c .
cd ..

mkdir build
cd build

$host-windres ../usbview/uvcview.rc rc.o

$host-gcc -mwindows --std=c99 -O2 \
  -Iinclude \
  -DNTDDI_VERSION=0x06020000 -D_WIN32_WINNT=0x0602 \
  -DSTRSAFE_NO_DEPRECATE -Doffsetof=__builtin_offsetof \
  ../usbview/*.c rc.o \
  -lcomctl32 -lcomdlg32 -lsetupapi -lshell32 -lshlwapi -lole32 -lgdi32 \
  -o usbview.exe
