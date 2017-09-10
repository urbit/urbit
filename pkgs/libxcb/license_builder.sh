source $setup

tar -xf $src
mv libxcb-* libxcb

license=$(cat libxcb/COPYING)

cat > $out <<EOF
<h2>libxcb</h2>

<pre>
$license
</pre>
EOF
