source $setup

tar -xf $src
mv libXfixes-* libxfixes

license=$(cat libxfixes/COPYING)

cat > $out <<EOF
<h2>libxfixes</h2>

<pre>
$license
</pre>
EOF
