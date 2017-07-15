source $setup

tar -xf $src

cat > $out <<EOF
<p>
  The Pololu USB Library (libusbp) is licensed under the following license:
</p>

<pre>
$(cat libusbp-*/LICENSE.txt)
</pre>
EOF
