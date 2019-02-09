source $setup

mkdir -p $out/bin

cd $out/bin

CXXFLAGS="$CXXFLAGS -DWRAPPER_PATH=\\\"$out/bin:$clang/bin\\\""

eval "g++ $CXXFLAGS $src_file -o $host-wrapper"

ln -s $clang/bin/llvm-dsymutil dsymutil
ln -s $ar/bin/$host-ar
ln -s $lipo/bin/lipo
ln -s $ranlib/bin/$host-ranlib
ln -s $ranlib/bin/$host-libtool
ln -s $strip/bin/$host-strip
ln -s $ld/bin/$host-ld

ln -s $host-wrapper $host-cc
ln -s $host-wrapper $host-c++

ln -s $host-wrapper $host-clang
ln -s $host-wrapper $host-clang++

ln -s $host-wrapper $host-gcc
ln -s $host-wrapper $host-g++
