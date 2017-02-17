source $stdenv/setup

cp -r $src/samples .
cp -r $src/util .

mkdir include
mkdir include/common
cp -r $src/src/common/{angleutils,platform,Optional}.h include/common/
cp -r $src/include/export.h include/
#cp -r $src/util/{OSWindow,Event,keyboard,mouse,Timer,EGLWindow,shader_utils,random_utils}.h include/
cp -r $src/samples/sample_util/* include/

mkdir build
cd build
mkdir bin lib

#grep -r DestroyTLSIndex "$angle_util/lib" "$angle/lib"; exit 2 # tmphax

$host-g++ -I../include -I"$angle_util/include" -I"$angle/include" -L"$angle_util/lib" -L"$angle/lib" -DGL_APICALL= -DANGLE_EXPORT= -DGL_GLEXT_PROTOTYPES ../samples/hello_triangle/HelloTriangle.cpp ../samples/sample_util/SampleApplication.cpp -langle_util -lGLESv2 -langle_common -lEGL -o bin/hello_triangle
