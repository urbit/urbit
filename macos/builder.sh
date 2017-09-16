source $setup

cp -r $osxcross osxcross
chmod -R u+w osxcross
cd osxcross

export UNATTENDED=1
export CC=gcc
export CXX=g++

export LC_ALL="C"

BASE_DIR=$PWD

TARBALL_DIR=$BASE_DIR/tarballs
BUILD_DIR=$BASE_DIR/build
TARGET_DIR=$BASE_DIR/target

mkdir -p $BUILD_DIR
mkdir -p $TARGET_DIR

cd $BUILD_DIR

mkdir -p $TARGET_DIR/bin

export OSXCROSS_BUILD_DIR=$BUILD_DIR

cd $BASE_DIR/wrapper

make wrapper

mv wrapper $TARGET_DIR/bin/$host-wrapper

cd $TARGET_DIR/bin
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

mv $TARGET_DIR $out
