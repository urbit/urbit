source $setup

tar -xf $src
mv CommonCrypto-* cc

mkdir build
cd build

for f in ../cc/lib/ccDispatch.c; do
  echo "compiling $f"
  gcc -c $CFLAGS $f -o $(basename $f).o
done

ar cr libCommonCrypto.a *.o

mkdir -p $out/lib

cp libCommonCrypto.a $out/lib/

cp ../cc/include $out/
