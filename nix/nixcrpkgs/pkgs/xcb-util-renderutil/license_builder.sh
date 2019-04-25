source $setup

tar -xf $src
mv xcb-util-renderutil-* xcb-util-renderutil

license=$(cat xcb-util-renderutil/COPYING)

cat > $out <<EOF
<h2>xcb-util-renderutil</h2>

<pre>
$license
</pre>
EOF
