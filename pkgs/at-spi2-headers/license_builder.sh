source $setup

license=$(cat $src/COPYING)

cat > $out <<EOF
<h2>at-spi2-headers</h2>
<pre>
$license
</pre>
EOF
