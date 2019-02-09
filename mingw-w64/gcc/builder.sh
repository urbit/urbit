source $setup

tar -xf $src
mv gcc-* src

cd src
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done

# Prevents a name collision with mingw-w64 headers.
# See: https://gcc.gnu.org/ml/gcc-help/2017-05/msg00121.html
cd libstdc++-v3
sed -i 's/\b__in\b/___in/g' \
  include/ext/random.tcc \
  include/ext/vstring.tcc \
  include/std/utility \
  include/std/tuple \
  include/std/istream \
  include/tr2/bool_set.tcc \
  include/tr2/bool_set \
  include/bits/basic_string.h \
  include/bits/basic_string.tcc \
  include/bits/locale_facets.h \
  include/bits/istream.tcc \
  include/tr1/utility \
  include/tr1/tuple
sed -i 's/\b__out\b/___out/g' \
  include/ext/random.tcc \
  include/ext/algorithm \
  include/ext/pb_ds/detail/debug_map_base.hpp \
  include/std/ostream \
  include/std/thread \
  include/tr2/bool_set \
  include/bits/ostream.tcc \
  include/bits/regex.tcc \
  include/bits/stl_algo.h \
  include/bits/locale_conv.h \
  include/bits/regex.h \
  include/bits/ostream_insert.h \
  include/tr1/regex \
  include/parallel/algo.h \
  include/parallel/set_operations.h \
  include/parallel/multiway_merge.h \
  include/parallel/unique_copy.h \
  include/experimental/algorithm \
  config/locale/dragonfly/c_locale.h \
  config/locale/generic/c_locale.h \
  config/locale/gnu/c_locale.h

cd ../..

mkdir build
cd build

../src/configure --prefix=$out $configure_flags

make $make_flags

make $install_targets

# Remove "install-tools" so we don't have a reference to bash.
rm -r "$out/libexec/gcc/$target/$version/install-tools/"
