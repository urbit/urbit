source $stdenv/setup

cp -r $src $TMP/$name
chmod -R u+w $TMP/$name
cd $TMP/$name

cd ./build/Linux-386-SSE2-GCC
sed -i 's|gcc|$(CC)|'        Makefile
sed -i 's/ar crs/$(AR) crs/' Makefile

make -j4

mkdir -p $out/{lib,include}
cp $src/source/include/*.h $out/include
cp softfloat.a $out/lib/libsoftfloat3.a
