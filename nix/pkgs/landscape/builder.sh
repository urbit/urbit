source $stdenv/setup

# chat

mkdir -p $out/chat

cp $src/chat/dist/index.js $out/chat/index.js
cp $src/chat/dist/tile.js $out/chat/tile.js
cp $src/chat/dist/index.css $out/chat/index.css

# clock

mkdir -p $out/clock

cp $src/clock/dist/tile.js $out/clock/tile.js

# launch

mkdir -p $out/launch

cp $src/launch/dist/index.js $out/launch/index.js
cp $src/launch/dist/index.css $out/launch/index.css

# publish

mkdir -p $out/publish

cp $src/publish/dist/index.js $out/publish/index.js
cp $src/publish/dist/tile.js $out/publish/tile.js
cp $src/publish/dist/index.css $out/publish/index.css

# soto

mkdir -p $out/soto

cp $src/soto/dist/index.js $out/soto/index.js
cp $src/soto/dist/tile.js $out/soto/tile.js
cp $src/soto/dist/index.css $out/soto/index.css

# weather

mkdir -p $out/weather

cp $src/weather/dist/tile.js $out/weather/tile.js

chmod -R u+w $out
