source $setup

mkdir -p $out/bin

eval "g++ $CXXFLAGS $wrapper/*.cpp -o $out/bin/$host-wrapper"

cd $out/bin

ln -s $clang/bin/llvm-ar $host-ar
ln -s $binutils/bin/$host-strip $host-strip
ln -s $binutils/bin/$host-nm $host-nm

ln -s $host-wrapper $host-cc
ln -s $host-wrapper $host-c++

ln -s $host-wrapper $host-clang
ln -s $host-wrapper $host-clang++

ln -s $host-wrapper $host-gcc
ln -s $host-wrapper $host-g++


