source $setup
shopt -u nullglob

tar -xf $src
mv linux-$version linux

mkdir -p obj/staged
make -C linux headers_install \
  ARCH=$linux_arch \
  O=$(pwd)/obj \
  INSTALL_HDR_PATH=$out

find $out '(' -name .install -o -name ..install.cmd ')' -exec rm {} +
