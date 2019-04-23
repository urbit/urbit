source $setup

tar -xf $src
mv libXau-* libxau

license=$(cat libxau/COPYING)

cat > $out <<EOF
<h2>libxau</h2>

<pre>
$license
</pre>
EOF
