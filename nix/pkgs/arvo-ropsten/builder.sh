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

rm -f $out/.gitignore
rm -f $out/.travis.yml
rm -rf $out/.travis

rcp() {
  if [ ! -f $1 ]; then
    >&2 echo "WARNING: $1 does not exist!"
  else
    cp $1 $2
  fi
}

# chat

mkdir -p $out/app/chat/css
mkdir -p $out/app/chat/js

rcp $landscape/chat/index.css $out/app/chat/css/index.css
rcp $landscape/chat/index.js $out/app/chat/js/index.js
rcp $landscape/chat/tile.js $out/app/chat/js/tile.js

# clock

mkdir -p $out/app/clock/js

rcp $landscape/clock/tile.js $out/app/clock/js/tile.js

# launch

mkdir -p $out/app/launch/css
mkdir -p $out/app/launch/js

rcp $landscape/launch/index.css $out/app/launch/css/index.css
rcp $landscape/launch/index.js $out/app/launch/js/index.js

# publish
mkdir -p $out/app/publish/css
mkdir -p $out/app/publish/js

rcp $landscape/publish/index.css $out/app/publish/css/index.css
rcp $landscape/publish/index.js $out/app/publish/js/index.js
rcp $landscape/publish/tile.js $out/app/publish/js/tile.js

# soto

mkdir -p $out/app/soto/css
mkdir -p $out/app/soto/js

rcp $landscape/soto/index.css $out/app/soto/css/index.css
rcp $landscape/soto/index.js $out/app/soto/js/index.js
rcp $landscape/soto/tile.js $out/app/soto/js/tile.js

# weather

mkdir -p $out/app/weather/js

rcp $landscape/weather/tile.js $out/app/weather/js/tile.js

