source $setup

cp -r $osxcross osxcross
chmod -R u+w osxcross
cd osxcross

mkdir tarballs

export UNATTENDED=1
export CC=gcc
export CXX=g++
bash -x ./build.sh

ln -s $host-cc target/bin/$host-gcc

mv target $out
