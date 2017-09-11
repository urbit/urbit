source $setup

tar -xf $src
mv libXext-* libxext

license=$(cat libxext/COPYING)

cat > $out <<EOF
<h2>libxext</h2>

<pre>
$license
</pre>
EOF
