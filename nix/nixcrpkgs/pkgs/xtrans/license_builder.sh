source $setup

tar -xf $src
mv xtrans-* xtrans

license=$(cat xtrans/COPYING)

cat > $out <<EOF
<h2>xtrans</h2>

<pre>
$license
</pre>
EOF
