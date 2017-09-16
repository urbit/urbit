source $setup

cp -r $osxcross osxcross
chmod -R u+w osxcross
cd osxcross

mkdir tarballs
cp $sdk tarballs/MacOSX$SDK_VERSION.sdk.tar.xz

export UNATTENDED=1

export CC=gcc
export CXX=g++
bash -x ./build.sh

cp -R target $out
