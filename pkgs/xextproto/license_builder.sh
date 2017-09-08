source $setup

tar -xf $src
mv xextproto-* xextproto

license=$(cat xextproto/COPYING)

cat > $out <<EOF
<h2>xextproto</h2>

<pre>
$license
</pre>
EOF
