# Last updated for qtbase-opensource-src-5.8.0.tar.xz

source $setup

if [ "$version" != "5.8.0" ]; then
  echo "You need to update the license fragment builder for Qt $version."
  exit 1
fi

tar -xf $src
mv qtbase-* qtbase

# TODO: need to make it a fatal error if a file below is missing

cat > $out <<EOF
<p>
  The Qt Toolkit is licensed under the GNU General Public License Version 3
  (GPLv3) as shown below.
</p>

<pre>
$(cat qtbase/LICENSE.GPLv3)
</pre>

<h2>Third-party components bundled with Qt</h2>

<p>
  From src/3rdparty/android:
</p>

<pre>
$(cat xasdasd)
</pre>

<p>
  From src/3rdparty/angle:
</p>

<pre>
TODO
</pre>

<p>
  From src/3rdparty/atspi2:
</p>

<pre>
TODO
</pre>

<p>
  From src/3rdparty/double-conversion:
</p>

<pre>
TODO
</pre>

<p>
  From src/3rdparty/easing:
</p>

<pre>
TODO
</pre>

<p>
  From src/3rdparty/forkfd:
</p>

<pre>
TODO
</pre>

<p>
  From src/3rdparty/freebsd:
</p>

<pre>
TODO
</pre>

<p>
  From src/3rdparty/freetype:
</p>

<pre>
TODO
</pre>

<p>
  From src/3rdparty/harfbuzz:
</p>

<pre>
TODO
</pre>

<p>
  From src/3rdparty/iaccessible2:
</p>

<pre>
TODO
</pre>

<p>
  From src/3rdparty/libjpeg:
</p>

<pre>
TODO
</pre>

<p>
  From src/3rdparty/libpng:
</p>

<pre>
TODO
</pre>

<p>
  From src/3rdparty/pcre:
</p>

<pre>
TODO
</pre>

<p>
  From src/3rdparty/pixman:
</p>

<pre>
TODO
</pre>

<p>
  From src/3rdparty/rfc6234:
</p>

<pre>
TODO
</pre>

<p>
  From src/3rdparty/sha3:
</p>

<pre>
TODO
</pre>

<p>
  From src/3rdparty/xcb:
</p>

<pre>
TODO
</pre>

<p>
  From src/3rdparty/xkbcommon:
</p>

<pre>
TODO
</pre>

<p>
  From src/3rdparty/zlib:
</p>

<pre>
TODO
</pre>
EOF
