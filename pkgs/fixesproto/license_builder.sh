source $setup

license=$(cat $src/COPYING)

cat > $out <<EOF
<h2>fixesproto</h2>

<pre>
$license
</pre>
EOF
