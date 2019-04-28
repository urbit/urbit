source $setup

cp -r $src ./src
chmod -R u+w ./src
cd src/libraries/liblmdb

mkdir ../out
DESTDIR="../out" make install
cp -r ../out/usr/local "$out"
