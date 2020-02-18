source $setup

tar -xf $src
mv avrdude-* avrdude

cd avrdude
chmod -R u+w .
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
cp $config_dot_sub config.sub
cat $extra_conf >> avrdude.conf.in
echo -n > windows/giveio.sys
cd ..

mkdir build
cd build

../avrdude/configure --host=$host --prefix=$out \
  --enable-static \
  --disable-shared \
  --disable-dependency-tracking

make

make install

cd $out/bin
rm -fv loaddrv* *.bat *.sys
