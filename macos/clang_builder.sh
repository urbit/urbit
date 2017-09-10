source $setup

cp -r $osxcross osxcross
chmod -R u+w osxcross
cd osxcross

mkdir tarballs
cp $src tarballs/cfe-$version.src.tar.xz
cp $llvm_src tarballs/llvm-$version.src.tar.xz

ls -l tarballs

export UNATTENDED=1
export INSTALLPREFIX=$out

./build_clang.sh
