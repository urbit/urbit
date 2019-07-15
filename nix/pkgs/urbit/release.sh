source $setup

cp -r $src ./src
chmod -R u+w ./src
cd src

for dep in $cross_inputs; do
   export CFLAGS="${CFLAGS-} -I$dep/include"
   export LDFLAGS="${LDFLAGS-} -L$dep/lib"
done

CC=$host-gcc                \
PKG_CONFIG=pkg-config-cross \
HOST=$host                  \
bash ./configure

make build/urbit build/urbit-worker -j8

mkdir -p $out/bin
cp -r $NCURSES/share/terminfo $out/bin/$exename-terminfo
cp ./build/urbit              $out/bin/$exename
cp ./build/urbit-worker       $out/bin/$exename-worker
