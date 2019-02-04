source $setup

tar -xf $src
mv libxkbcommon-* libxkbcommon

license=$(cat libxkbcommon/COPYING)

cat > $out <<EOF
<h2>libxkbcommon</h2>

<pre>
$license
</pre>
EOF
