source $setup

mkdir -p $out/bin

cd $out/bin

eval "g++ $CXXFLAGS $src_file -o $host-wrapper"

ln -s $ar/bin/$host-ar
ln -s $ranlib/bin/$host-ranlib
ln -s $ranlib/bin/$host-libtool

ln -s $host-wrapper $host-cc
ln -s $host-wrapper $host-c++

ln -s $host-wrapper $host-clang
ln -s $host-wrapper $host-clang++

ln -s $host-wrapper $host-gcc
ln -s $host-wrapper $host-g++
