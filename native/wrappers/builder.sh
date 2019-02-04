source $setup

mkdir -p $out/bin

cat > $out/bin/pkg-config-cross <<EOF
#!$(which bash)
PKG_CONFIG_LIBDIR=\$PKG_CONFIG_CROSS_PATH \\
PKG_CONFIG_PATH=\$PKG_CONFIG_CROSS_PATH \\
exec pkg-config \$@
EOF

cat > $out/bin/cmake-cross <<EOF
#!$(which bash)
PKG_CONFIG=pkg-config-cross \\
CMAKE_PREFIX_PATH=\$CMAKE_CROSS_PREFIX_PATH \\
exec cmake -DCMAKE_TOOLCHAIN_FILE=\$cmake_toolchain \$@
EOF

chmod a+x $out/bin/*
