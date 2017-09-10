source $setup

tar -xf $src
mv fixesproto-* fixesproto

license=$(cat fixesproto/COPYING)

cat > $out <<EOF
<h2>fixesproto</h2>

<pre>
$license
</pre>
EOF
