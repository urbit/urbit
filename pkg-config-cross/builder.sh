source $stdenv/setup

mkdir -p $out/bin

cat > $out/bin/$host-pkg-config <<EOF
PKG_CONFIG_LIBDIR=\$${host_as_var}_PKG_CONFIG_LIBDIR \\
PKG_CONFIG_PATH=\$${host_as_var}_PKG_CONFIG_PATH \\
exec pkg-config \$@
EOF

chmod a+x $out/bin/$host-pkg-config
