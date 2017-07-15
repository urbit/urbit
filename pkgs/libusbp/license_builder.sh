source $setup

tar -xf $src
mv libusbp-* libusbp

license=$(cat libusbp/LICENSE.txt)

cat > $out <<EOF
<p>
  The Pololu USB Library (libusbp) is licensed under the following license:
</p>

<pre>
$license
</pre>
EOF
