source $setup

tar -xf $src
mv xcb-util-* xcb-util

license=$(cat xcb-util/COPYING)

cat > $out <<EOF
<h2>xcb-util</h2>

<pre>
$license
</pre>
EOF
