source $stdenv/setup

rcp() {
  if [ ! -f $1 ]; then
    >&2 echo "WARNING: $1 does not exist!"
  else
    cp $1 $2
  fi
}

# chat

mkdir -p $out/chat

rcp $src/chat/dist/index.js $out/chat/index.js
rcp $src/chat/dist/tile.js $out/chat/tile.js
rcp $src/chat/dist/index.css $out/chat/index.css

# clock

mkdir -p $out/clock

rcp $src/clock/dist/tile.js $out/clock/tile.js

# launch

mkdir -p $out/launch

rcp $src/launch/dist/index.js $out/launch/index.js
rcp $src/launch/dist/index.css $out/launch/index.css

# publish

mkdir -p $out/publish

rcp $src/publish/dist/index.js $out/publish/index.js
rcp $src/publish/dist/tile.js $out/publish/tile.js
rcp $src/publish/dist/index.css $out/publish/index.css

# soto

mkdir -p $out/soto

rcp $src/soto/dist/index.js $out/soto/index.js
rcp $src/soto/dist/tile.js $out/soto/tile.js
rcp $src/soto/dist/index.css $out/soto/index.css

# weather

mkdir -p $out/weather

rcp $src/weather/dist/tile.js $out/weather/tile.js

chmod -R u+w $out
