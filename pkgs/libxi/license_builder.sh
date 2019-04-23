source $setup

tar -xf $src
mv libXi-* libxi

license=$(cat libxi/COPYING)

cat > $out <<EOF
<h2>libxi</h2>

<pre>
$license
</pre>
EOF
