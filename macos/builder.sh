source $setup

cp -r $osxcross osxcross
chmod -R u+w osxcross
cd osxcross

mkdir -p $out/bin

cd wrapper

eval "g++ $CXXFLAGS *.cpp programs/*.cpp -o $out/bin/$host-wrapper"

cd $out/bin

ln -s $host-wrapper $host-cc
ln -s $host-wrapper $host-c++

ln -s $host-wrapper $host-clang
ln -s $host-wrapper $host-clang++

ln -s $host-wrapper $host-gcc
ln -s $host-wrapper $host-g++

ln -s $host-wrapper $host-dsymutil
ln -s $host-wrapper $host-sw_vers
ln -s $host-wrapper $host-xcrun

