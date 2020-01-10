source $stdenv/setup

cp -r $src tmp
chmod -R u+w tmp

ZUSE=tmp/sys/zuse.hoon
AMES=tmp/sys/vane/ames.hoon
ACME=tmp/app/acme.hoon

# replace the mainnet azimuth contract with the ropsten contract
sed --in-place \
  's/\(\+\+  contracts  \)mainnet\-contracts/\1ropsten-contracts/' \
  $ZUSE

# increment the %ames protocol version
sed -r --in-place \
  's_^(=/  protocol\-version=\?\(.*\)  %)([0-7])_echo "\1$(echo "(\2+1) % 8" | bc)"_e' \
  $AMES

# use the staging API in :acme
sed --in-place \
  's_https://acme-v02.api.letsencrypt.org/directory_https://acme-staging-v02.api.letsencrypt.org/directory_' \
  $ACME

cp -r tmp $out
chmod -R u+w $out

rm $out/.gitignore
rm $out/.travis.yml
rm -rf $out/.travis

mkdir -p $out/app/chat/css
mkdir -p $out/app/chat/js

cp $landscape/chat/index.css $out/app/chat/css/index.css
cp $landscape/chat/index-min.js $out/app/chat/js/index-min.js
cp $landscape/chat/tile-min.js $out/app/chat/js/tile-min.js

mkdir -p $out/app/clock/js

cp $landscape/clock/tile-min.js $out/app/clock/js/tile-min.js

mkdir -p $out/app/launch/css
mkdir -p $out/app/launch/js

cp $landscape/launch/index.css $out/app/launch/css/index.css
cp $landscape/launch/index-min.js $out/app/launch/js/index-min.js

mkdir -p $out/app/publish/css
mkdir -p $out/app/publish/js

cp $landscape/publish/index.css $out/app/publish/css/index.css
cp $landscape/publish/index-min.js $out/app/publish/js/index-min.js
cp $landscape/publish/tile-min.js $out/app/publish/js/tile-min.js

mkdir -p $out/app/soto/css
mkdir -p $out/app/soto/js

cp $landscape/soto/index.css $out/app/soto/css/index.css
cp $landscape/soto/index-min.js $out/app/soto/js/index-min.js
cp $landscape/soto/tile-min.js $out/app/soto/js/tile-min.js

mkdir -p $out/app/weather/js

cp $landscape/weather/tile-min.js $out/app/weather/js/tile-min.js

