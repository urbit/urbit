source $setup

tar -xf $src
mv xcb-util-wm-* xcb-util-wm

license=$(cat xcb-util-wm/COPYING)

cat > $out <<EOF
<h2>xcb-util-wm</h2>

<pre>
$license
</pre>
EOF
