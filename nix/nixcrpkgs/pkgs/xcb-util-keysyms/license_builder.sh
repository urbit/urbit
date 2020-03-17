source $setup

tar -xf $src
mv xcb-util-keysyms-* xcb-util-keysyms

license=$(head -n31 xcb-util-keysyms/keysyms/keysyms.c)

cat > $out <<EOF
<h2>xcb-util-keysyms</h2>

<pre>
$license
</pre>
EOF
