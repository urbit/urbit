source $stdenv/setup

cp -r $src/samples .
cp -r $src/util .

mkdir include
cp -r $src/samples/sample_util/* include/

mkdir build
cd build
mkdir bin

#ls "$angle_util/lib" "$angle/lib"; exit 3 # tmphax
#grep -r SwapBuffersWithDamage "$angle_util/lib" "$angle/lib"; exit 2 # tmphax

# TODO: add -O2 option here after things are working

exe_suffix=.exe  # TODO: this should come from crossenv

CFLAGS="-I../include -I$angle_util/include -I$angle/include"
CFLAGS="$CFLAGS -DGL_APICALL= -DANGLE_EXPORT= -DGL_GLEXT_PROTOTYPES"
LDFLAGS="-mwindows -L$angle_util/lib -L$angle/lib"
LIBS="-langle_util -lEGL_static -lGLESv2_static -lANGLE -ltranslator -lpreprocessor -langle_image_util -langle_common -ld3d9 -lgdi32"

echo "compiling hello_triangle"
$host-g++ -g $CFLAGS $LDFLAGS \
  ../samples/hello_triangle/HelloTriangle.cpp \
  ../samples/sample_util/SampleApplication.cpp \
  $LIBS -o bin/hello_triangle${exe_suffix}

mkdir -p $out/license
cp $src/LICENSE $out/license/
cp -r bin $out/
