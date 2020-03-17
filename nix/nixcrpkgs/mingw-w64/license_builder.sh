source $setup

tar -xf $gcc_src
mv gcc-* gcc

license_gcc=$(cat gcc/COPYING3.LIB)
cd $src
license_runtime=$(cat COPYING.MinGW-w64-runtime/COPYING.MinGW-w64-runtime.txt)
license_winpthread=$(cat mingw-w64-libraries/winpthreads/COPYING)

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

<h2>MinGW-w64 runtime components</h2>

<pre>
$license_runtime
</pre>

<p>
  libwinpthread also comes from the mingw-w64 project and its license is below.
</p>

<pre>
$license_winpthread
</pre>

EOF
