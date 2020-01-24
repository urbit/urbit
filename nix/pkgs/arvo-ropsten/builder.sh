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
