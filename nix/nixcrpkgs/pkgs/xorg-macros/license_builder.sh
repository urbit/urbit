source $setup

tar -xf $src
mv util-macros-* xorg-macros

license=$(cat xorg-macros/COPYING)

cat > $out <<EOF
<h2>xorg-macros</h2>

<pre>
$license
</pre>
EOF
