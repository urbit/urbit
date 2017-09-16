source $setup

cp -r $osxcross osxcross
chmod -R u+w osxcross
cd osxcross

export UNATTENDED=1
export CC=gcc
export CXX=g++

set +u
echo hi

source tools/tools.sh

echo hey

mkdir -p $BUILD_DIR
mkdir -p $TARGET_DIR
mkdir -p $SDK_DIR

pushd $BUILD_DIR &>/dev/null

mkdir -p $TARGET_DIR/bin

export OSXCROSS_BUILD_DIR=$BUILD_DIR

# NOTE: osxcross set this to LLVM_LIB_DIR from cctools config.log,
# so maybe we should do the same
export OSXCROSS_LIBLTO_PATH=

cd $BASE_DIR/wrapper

FLAGS="$CXXFLAGS "

mkdir -p ../target ../target/bin
export PLATFORM
export CXX

OSXCROSS_CXXFLAGS="$FLAGS" make wrapper

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
