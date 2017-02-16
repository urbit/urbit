source $stdenv/setup

cp -r $src/util .

mkdir include
cp -r $src/util/*.h include/
cp -r $src/include/export.h include/
mkdir include/common
cp -r $src/src/common/*.h include/common/
mkdir -p include/windows/win32
cp -r $src/util/windows/win32/*.h include/windows/win32/

mkdir build
cd build
mkdir obj lib

source_files=../util/*.cpp

if [ "$os" == "windows"]; then
  source_files="$source_files ../util/windows/win32/*.cpp"
fi

# NOTE: We would need to add other source files here to support other OSes

for c in $source_files; do
  echo "compiling $(basename $c)"
  $host-g++ -c -fpermissive \
    -I../include -I"$angle/include" -L"$angle/lib" \
    -DGL_GLEXT_PROTOTYPES -DEGL_EGLEXT_PROTOTYPES -DLIBANGLE_UTIL_IMPLEMENTATION \
    $c -lGLESv2 -lEGL \
    -o obj/$(basename $c).o
done

$host-ar r lib/libangle_util.a obj/*.o

mkdir -p $out/{license,lib,include}
cp $src/LICENSE $out/license/
cp lib/libangle_util.a $out/lib/
cp ../util/*.h $out/include/
