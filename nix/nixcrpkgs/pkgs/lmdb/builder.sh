source $setup

cp -r $src ./src
chmod -R u+w ./src
cd src

export DESTDIR="$out"

make install
