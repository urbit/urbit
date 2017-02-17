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

exe_suffix=.exe  # TODO: this should come from crossenv

CFLAGS="-I../include -I$angle_util/include -I$angle/include"
CFLAGS="$CFLAGS -DGL_APICALL= -DANGLE_EXPORT= -DGL_GLEXT_PROTOTYPES"
LDFLAGS="-L$angle_util/lib -L$angle/lib"
LIBS="-langle_util -lEGL_static -lGLESv2_static -lANGLE -langle_common -ltranslator -lpreprocessor -langle_common -langle_image_util -langle_common -ld3d9 -lgdi32"

$host-g++ $CFLAGS $LDFLAGS \
  ../samples/hello_triangle/HelloTriangle.cpp \
  ../samples/sample_util/SampleApplication.cpp \
  $LIBS -o bin/hello_triangle${exe_suffix}

mkdir -p $out/license
cp $src/LICENSE $out/license/
cp -r bin $out/
