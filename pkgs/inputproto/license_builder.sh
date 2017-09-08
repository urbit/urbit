source $setup

tar -xf $src
mv inputproto-* inputproto

license=$(cat inputproto/COPYING)

cat > $out <<EOF
<h2>inputproto</h2>

<pre>
$license
</pre>
EOF
