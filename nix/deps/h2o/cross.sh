source $stdenv/setup

cp -r $src src
chmod -R u+w src
cd src

cmake-cross .                      \
  -DZLIB_LIBRARY=$zlib/lib/libz.a  \
  -DZLIB_INCLUDE_DIR=$zlib/include \
  -DCMAKE_INSTALL_PREFIX=$out      \
  -DBUILD_SHARED_LIBS=off          \
  -DWITH_MRUBY=off                 \
  -DWITH_BUNDLED_SSL=off           \
  -DWITH_PICOTLS=on

make libh2o

mkdir -p $out/{lib,lib/pkgconfig,include}

cp ./libh2o.a $out/lib

cp ./libh2o.pc $out/lib/pkgconfig

cp -r include/* $out/include

cp deps/picohttpparser/picohttpparser.h $out/include
