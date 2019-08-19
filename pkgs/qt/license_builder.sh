# Last updated for qtbase-opensource-src-5.8.0.tar.xz

source $setup

if [ "$version" != "5.12.4" ]; then
  echo "You need to update the license fragment builder for Qt $version."
  echo "(Check for new third-party libraries.)"
  exit 1
fi

tar -xf $src
mv qtbase-* qtbase

# Read the license files here instead of in the big string so it is a fatal
# error if any of them are missing.
license_qt=$(cat qtbase/LICENSE.LGPLv3)
cd qtbase/src/3rdparty
license_android=$(cat android/LICENSE)
# angle: we don't use it
# dbus-ifaces: just some XML files, no license
license_dc=$(cat double-conversion/LICENSE)
license_easing=$(cat easing/LICENSE)
license_forkfd=$(cat forkfd/LICENSE)
license_freebsd=$(cat freebsd/LICENSE)
license_freetype=$(cat freetype/docs/GPLv2.TXT)
license_gradle=$(cat gradle/LICENSE-GRADLEW.txt)
license_harfbuzz=$(cat harfbuzz/COPYING)
license_harfbuzz_ng=$(cat harfbuzz-ng/COPYING)
license_ia2=$(cat iaccessible2/LICENSE)
license_icc=$(cat icc/LICENSE.txt)
license_libjpeg=$(cat libjpeg/LICENSE)
license_libpng=$(cat libpng/LICENSE)
# md4: public domain
# md5: public domain
license_pcre2=$(cat pcre2/LICENCE)
license_pixman=$(cat pixman/LICENSE)
license_rfc6234=$(cat rfc6234/LICENSE)
# sha1: public domain according to JSON file
license_sha3_1=$(cat sha3/BRG_ENDIAN_LICENSE)
license_sha3_2=$(cat sha3/CC0_LICENSE)
# sqlite: public domain
license_tinycbor=$(cat tinycbor/LICENSE)
# wasm: Stands for web assembly; we don't use it (yet)
# wintab: No licensing restrictions according to LICENSE.txt.
# xcb: Not used; it has its own nixcrpkgs package.
license_zlib=$(cat zlib/LICENSE)

cat > $out <<EOF
<h2>Qt</h2>

<p>
  The Qt Toolkit is licensed under the
  GNU Lesser General Public License Version 3 (LGPLv3) as shown below.
</p>

<pre>
$license_qt
</pre>

<h2>Third-party components bundled with Qt</h2>

<p>
  This software might include code from third-party comoponents bundled with Qt.
  The copyright notices of those components are reproduced below.
</p>

<pre>
$license_android
</pre>

<pre>
$license_dc
</pre>

<pre>
$license_easing
</pre>

<pre>
$license_forkfd
</pre>

<pre>
$license_freebsd
</pre>

<pre>
$license_freetype
</pre>

<pre>
$license_gradle
</pre>

<pre>
$license_harfbuzz
</pre>

<pre>
$license_harfbuzz_ng
</pre>

<pre>
$license_ia2
</pre>

<pre>
$license_icc
</pre>

<pre>
$license_libjpeg
</pre>

<pre>
$license_libpng
</pre>

<pre>
$license_pcre2
</pre>

<pre>
$license_pixman
</pre>

<pre>
$license_rfc6234
</pre>

<pre>
$license_sha3_1
</pre>

<pre>
$license_sha3_2
</pre>

<pre>
$license_tinycbor
</pre>

<pre>
$license_zlib
</pre>
EOF
