source $setup

tar -xf $src
mv xcb-util-image-* xcb-util-image

license=$(cat xcb-util-image/COPYING)

cat > $out <<EOF
<h2>xcb-util-image</h2>

<pre>
$license
</pre>
EOF
