source $setup

tar -xf $src
mv xcb-util-keysyms-* xcb-util-keysyms

license=$(cat xcb-util-keysyms/COPYING)

cat > $out <<EOF
<h2>xcb-util-keysyms</h2>

<pre>
$license
</pre>
EOF
