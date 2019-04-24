source $setup

cp -R $src src

chmod -R u+rw src

cd src
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done

# ANGLE's gyp build files do not handle out-of-tree builds properly,
# so let's just do an in tree build.

gyp $gyp_flags src/angle.gyp

if [ -n "$debug" ]; then
  cd out/Debug
else
  cd out/Release
fi

ninja

libs="
obj/src/libANGLE.a
obj/src/libangle_common.a
obj/src/libangle_image_util.a
obj/src/libEGL_static.a
obj/src/libGLESv2_static.a
obj/src/libpreprocessor.a
obj/src/libtranslator.a
"

# Make the static libraries not be thin.
for lib in $libs; do
  $AR rvs $lib.new $($AR -t $lib)
  mv $lib.new $lib
  $RANLIB $lib
done

mkdir -p $out/{lib,license,include}
cp -r ../../LICENSE $out/license/
cp $libs $out/lib/
cp -r ../../include/{KHR,EGL,GLES2,GLES3,GLSLANG} $out/include/
