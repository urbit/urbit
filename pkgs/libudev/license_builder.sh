source $setup

tar -xf $src
mv systemd-* systemd

ls systemd
exit 3

license=$(cat systemd/LICENSE)

cat > $out <<EOF
<h2>libudev (part of systemd)</h2>

<pre>
$license
</pre>
EOF
