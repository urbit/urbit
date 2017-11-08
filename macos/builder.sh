source $setup

mkdir -p $out/bin

cd $out/bin

eval "g++ $CXXFLAGS $wrapper/*.cpp -o $host-wrapper"

# TODO: why does incorporating binutils ar or ranlib result in
# linker errors in macos.p-load?  Maybe ar does a bad job at
# archiving mach-o files and ranlib does a bad job indexing them?

# TODO: And why does llvm-ar give malformed archives according to both
# "llvm-ar" t and "gnu-ar t"?

ln -s $clang/bin/llvm-ar llvm-ar
ln -s $binutils/bin/$host-ar gnu-ar
ln -s $xar/bin/xar xar
ln -s llvm-ar $host-ar

ln -s $binutils/bin/$host-ranlib gnu-ranlib
# ln -s $binutils/bin/$host-ranlib $host-ranlib

ln -s $binutils/bin/$host-strip $host-strip

ln -s $binutils/bin/$host-nm $host-nm

ln -s $host-wrapper $host-cc
ln -s $host-wrapper $host-c++

ln -s $host-wrapper $host-clang
ln -s $host-wrapper $host-clang++

ln -s $host-wrapper $host-gcc
ln -s $host-wrapper $host-g++


