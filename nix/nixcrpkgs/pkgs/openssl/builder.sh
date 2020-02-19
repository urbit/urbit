source $setup

tar -xf $src

mkdir build
cd build

err () { echo ERR "$@" >&2; }

case $host in
    i686-linux-musleabi)
        confighost=linux-x86;;
    x86_64-linux-musleabi)
        confighost=linux-x86_64;;
    aarch64-linux-musleabi)
        confighost=linux-aarch64;;
    x86_64-apple-darwin*)
        confighost=darwin64-x86_64-cc;;
    *)
        err openssl builder.sh needs to excplicitly translate
        err "'host=$host'" to something openssl understands.
        confighost=$host;;
esac

# TODO The `no-async` option seems weird, but
# https://github.com/openssl/openssl/issues/1607

# TODO I stole the no-dso option from the here[1], but is it
# needed? I seems to be related to shared libraries, which we aren't using
# anyways, but I don't like not understanding.
#
# [1]: https://github.com/rust-embedded/cross/blob/master/docker/openssl.sh

# TODO Why `-fPIC`? I stole it from [2]
#
# [2]: https://github.com/rust-embedded/cross/pull/218/files

sed -i "1s|usr|$coreutils|" ../openssl-$version/Configure

../openssl-$version/Configure   \
  --prefix=$out                 \
  --cross-compile-prefix=$host- \
  no-shared                     \
  no-dso                        \
  no-async                      \
  $confighost                   \
  -fPIC

make
make install
