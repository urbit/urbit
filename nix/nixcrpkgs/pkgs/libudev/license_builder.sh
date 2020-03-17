source $setup

tar -xf $src
mv systemd-* systemd

license=$(cat systemd/LICENSE.LGPL2.1)

cat > $out <<EOF
<h2>libudev (part of systemd)</h2>

<pre>
$license
</pre>
EOF
