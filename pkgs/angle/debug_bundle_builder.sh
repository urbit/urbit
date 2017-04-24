source $setup

mkdir -p $out

cp -r $gdb/* $out
chmod -R u+w $out

cp -r $examples/* $out
chmod -R u+w $out

mkdir -p $out/src
cp -r $src $out/src/angle
chmod -R u+w $out

cat <<EOF > $out/gdbcmd.txt
set substitute-path ../samples src/angle/samples
set substitute-path ../util src/angle/util
EOF
