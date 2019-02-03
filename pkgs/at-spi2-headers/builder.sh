source $setup

mkdir -p $out/include/atspi $out/lib/pkgconfig

cp $src/atspi/*.h $out/include/atspi/

cat > $out/lib/pkgconfig/atspi-2.pc <<EOF
prefix=$out
includedir=\${prefix}/include
Name: atspi
Version: 2
Cflags: -I\${includedir}
EOF
