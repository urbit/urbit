source $setup

tar -xf $src
mv libX11-* libx11

license=$(cat libx11/COPYING)

cat > $out <<EOF
<h2>libx11</h2>

<pre>
$license
</pre>
EOF
