source $setup

cp -r $osxcross osxcross
chmod -R u+w osxcross
cd osxcross

mkdir tarballs
cp $sdk tarballs/MacOSX10.11.sdk.tar.xz
cp $cctools_src tarballs/cctools-895-ld64-274.2_8e9c3f2.tar.xz
cp $xar_src tarballs/xar-1.6.1.tar.gz

export UNATTENDED=1

# TODO: build with GCC:
# export CC=gcc
# export CXX=g++

bash -x ./build.sh

cp -R target $out
