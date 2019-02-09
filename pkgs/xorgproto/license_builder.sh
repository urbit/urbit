source $setup

# Note: Concatenating license files like this is ugly.
license=$(cat $src/COPYING*)

cat > $out <<EOF
<h2>xorgproto</h2>

<pre>
$license
</pre>
EOF
