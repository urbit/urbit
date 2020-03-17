source $setup

tar -xf $src
mv libusbp-* libusbp

license=$(cat libusbp/LICENSE.txt)

{
  cat > $out <<EOF
<h2>Pololu USB library (libusbp)</h2>

<p>
  The Pololu USB Library (libusbp) is licensed under the following license:
</p>

<pre>
$license
</pre>
EOF
} > $out
