source $setup

tar -xf $src
mv avrdude-* avrdude

cd avrdude
chmod -R u+w .
cp $config_dot_sub config.sub
cat $extra_conf >> avrdude.conf.in
cd ..

mkdir build
cd build

../avrdude/configure --host=$host --prefix=$out \
  --enable-static \
  --disable-shared \
  --disable-dependency-tracking

make

make install
