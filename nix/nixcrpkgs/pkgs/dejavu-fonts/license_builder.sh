source $setup

tar -xf $src
mv dejavu-* dejavu

license=$(cat dejavu/LICENSE)

cat > $out <<EOF
<h2>DejaVu Fonts</h2>

<pre>
$license
</pre>
EOF
