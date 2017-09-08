source $setup

tar -xf $src
mv xcb-proto-* xcb-proto

license=$(cat xcb-proto/COPYING)

cat > $out <<EOF
<h2>xcb-proto</h2>

<pre>
$license
</pre>
EOF
