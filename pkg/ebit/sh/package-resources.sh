ver=0.8.2
host=https://bootstrap.urbit.org

rm -rf ./resources
mkdir -p resources/{linux,mac}

(
  cd resources/linux
  curl $host/urbit-linux64-v$ver.tgz | gunzip | tar x
)

(
  cd resources/mac
  curl $host/urbit-darwin-v$ver.tgz | gunzip | tar x
)

(
  cd resources
  wget $host/urbit-$ver.pill -O solid.pill
)
