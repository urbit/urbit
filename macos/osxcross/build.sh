#!/usr/bin/env bash

pushd "${0%/*}" &>/dev/null

source tools/tools.sh

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


