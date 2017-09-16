source $setup

cp -r $osxcross osxcross
chmod -R u+w osxcross
cd osxcross

export UNATTENDED=1
export CC=gcc
export CXX=g++
bash -x ./build.sh

cd target/bin
ln -s $host-wrapper $host-c++
ln -s $host-wrapper $host-cc
ln -s $host-wrapper $host-clang
ln -s $host-wrapper $host-clang++
ln -s $host-wrapper $host-dsymutil
ln -s $host-wrapper $host-gcc
ln -s $host-wrapper $host-g++
ln -s $host-wrapper $host-sw_vers
ln -s $host-wrapper $host-xcrun
cd ../..

mv target $out
