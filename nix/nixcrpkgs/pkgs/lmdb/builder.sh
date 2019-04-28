source $setup

cp -r $src ./src
chmod -R u+w ./src
cd src/libraries/liblmdb

sed -i 's/liblmdb.a liblmdb..SOEXT.$/liblmdb.a/' Makefile
sed -i "s/gcc/$host-gcc/"                        Makefile
sed -i "s/ar/$host-ar/"                          Makefile
sed -i 's/^CC.*/CC = '"$host-gcc/"               Makefile

cat Makefile

make CFLAGS+="-fPIC"

make DESTDIR="$out" prefix=/ install
