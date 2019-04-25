source $setup

tar -xf $gcc_src
mv gcc-* gcc

tar -xf $musl_src
mv musl-* musl

tar -xf $linux_src
mv linux-* linux

license_gcc=$(cat gcc/COPYING3.LIB)
license_musl=$(cat musl/COPYRIGHT)
license_linux=$(cat linux/COPYING)

cat > $out <<EOF
<p>
  The third-party software included with this software may
  have been patched or otherwise modified.
</p>

<h2>GCC run-time libraries</h2>

<p>
  The GCC run-time libraries libgcc and libstdc++ are licensed under the GNU
  General Public License Version 3 (GPLv3) as shown below.
</p>

<pre>
$license_gcc
</pre>

<h2>musl libc</h2>

<pre>
$license_musl
</pre>

<h2>Linux headers</h2>

<pre>
$license_linux
</pre>

EOF
