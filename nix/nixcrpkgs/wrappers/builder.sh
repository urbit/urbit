source $setup

mkdir -p $out/bin

cd $out

cat > cmake_toolchain.txt <<EOF
set(CMAKE_SYSTEM_NAME ${cmake_system})
set(CMAKE_C_COMPILER ${host}-gcc)
set(CMAKE_CXX_COMPILER ${host}-g++)
set(CMAKE_RC_COMPILER ${host}-windres)
EOF

cat > meson_cross.txt <<END
[binaries]
c = '$host-gcc'
cpp = '$host-g++'
ar = '$host-ar'
strip = '$host-strip'
pkgconfig = 'pkg-config-cross'
[host_machine]
system = '$meson_system'
cpu_family = '$meson_cpu_family'
cpu = '$meson_cpu'
endian = 'little'
END

cat > bin/pkg-config-cross <<EOF
#!$(which bash)
PKG_CONFIG_LIBDIR=\$PKG_CONFIG_CROSS_PATH \\
PKG_CONFIG_PATH=\$PKG_CONFIG_CROSS_PATH \\
exec pkg-config \$@
EOF

cat > bin/cmake-cross <<EOF
#!$(which bash)
PKG_CONFIG=pkg-config-cross \\
CMAKE_PREFIX_PATH=\$CMAKE_CROSS_PREFIX_PATH \\
exec cmake -DCMAKE_TOOLCHAIN_FILE=$out/cmake_toolchain.txt \$@
EOF

cat > bin/meson-cross <<EOF
#!$(which bash)
exec meson --cross-file $out/meson_cross.txt \$@
EOF

chmod a+x bin/*
