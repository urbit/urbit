source $setup

tar -xf $src
mv kbproto-* kbproto

license=$(cat kbproto/COPYING)

cat > $out <<EOF
<h2>kbproto</h2>

<pre>
$license
</pre>
EOF
