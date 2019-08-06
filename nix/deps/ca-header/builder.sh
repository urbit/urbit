source $stdenv/setup

set -ex

cleanup () {
  echo "done"
}

trap cleanup EXIT


if ! [ -f "$SSL_CERT_FILE" ]; then
  echo "$SSL_CERT_FILE doesn't exist"
  exit 1
fi

mkdir -p ./include

cat $SSL_CERT_FILE > include/ca-bundle.crt
xxd -i include/ca-bundle.crt > ca-bundle.h

mkdir -p $out/include

mv ca-bundle.h $out/include
rm -rf ./include

set +x
