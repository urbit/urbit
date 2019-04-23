source $setup

cp -r $src/util .

mkdir include
cp -r $src/util/*.h include/
cp -r $src/include/export.h include/
mkdir include/common
cp -r $src/src/common/*.h include/common/
mkdir -p include/windows/win32
cp -r $src/util/windows/*.h include/windows/
cp -r $src/util/windows/win32/*.h include/windows/win32/

mkdir -p build/{obj,lib}
cd build

source_files=../util/*.cpp

if [ "$os" == "windows" ]; then
  source_files="$source_files ../util/windows/*.cpp ../util/windows/win32/*.cpp"
fi

for c in $source_files; do
  echo "compiling $(basename $c)"
  $host-g++ -c -g -O2 -fpermissive \
    -I../include -I"$angle/include" -L"$angle/lib" \
    -DGL_APICALL= -DANGLE_EXPORT= -DEGLAPI= \
    -DGL_GLEXT_PROTOTYPES -DEGL_EGLEXT_PROTOTYPES -DLIBANGLE_UTIL_IMPLEMENTATION \
    $c -lGLESv2 -lEGL \
    -o obj/$(basename $c).o
done

$host-ar r lib/libangle_util.a obj/*.o

mkdir -p $out/{license,lib}
cp $src/LICENSE $out/license/
cp lib/libangle_util.a $out/lib/
cp -r ../include $out/
