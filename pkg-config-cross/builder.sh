source $stdenv/setup

mkdir -p $out/bin

cat > $out/bin/pkg-config-cross <<EOF
PKG_CONFIG_LIBDIR=\$PKG_CONFIG_CROSS_PATH \\
PKG_CONFIG_PATH=\$PKG_CONFIG_CROSS_PATH \\
exec pkg-config \$@
EOF

chmod a+x $out/bin/pkg-config-cross
