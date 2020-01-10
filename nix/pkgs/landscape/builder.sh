source $stdenv/setup

# chat

mkdir -p $out/chat

cp $src/chat/dist/index-min.js $out/chat
cp $src/chat/dist/tile-min.js $out/chat
cp $src/chat/dist/index.css $out/chat

# clock

mkdir -p $out/clock

cp $src/clock/dist/tile-min.js $out/clock

# launch

mkdir -p $out/launch

cp $src/launch/dist/index-min.js $out/launch
cp $src/launch/dist/index.css $out/launch

# publish

mkdir -p $out/publish

cp $src/publish/dist/index-min.js $out/publish
cp $src/publish/dist/tile-min.js $out/publish
cp $src/publish/dist/index.css $out/publish

# soto

mkdir -p $out/soto

cp $src/soto/dist/index-min.js $out/soto
cp $src/soto/dist/tile-min.js $out/soto
cp $src/soto/dist/index.css $out/soto

# weather

mkdir -p $out/weather

cp $src/weather/dist/tile-min.js $out/weather

chmod -R u+w $out
