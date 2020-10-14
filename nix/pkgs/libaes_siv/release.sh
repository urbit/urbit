source $setup

cp -r $src ./src
chmod -R u+w ./src
cd ./src

for dep in $cross_inputs; do
   export CFLAGS="${CFLAGS-} -I$dep/include"
   export LDFLAGS="${LDFLAGS-} -L$dep/lib"
done

PREFIX=$out make install

