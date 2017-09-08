source $setup

tar -xf $src
mv libxau-* libxau

license=$(cat xcb-proto/COPYING)

cat > $out <<EOF
<h2>libxau</h2>

<pre>
$license
</pre>
EOF
