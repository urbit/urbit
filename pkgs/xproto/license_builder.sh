source $setup

tar -xf $src
mv xproto-* xproto

license=$(cat xproto/COPYING)

cat > $out <<EOF
<h2>xproto</h2>

<pre>
$license
</pre>
EOF
