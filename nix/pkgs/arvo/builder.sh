source $stdenv/setup

cp -r $src/ $out

chmod -R u+w $out

rm -f $out/.gitignore
rm -f $out/.travis.yml
rm -rf $out/.travis

mkdir -p $out/app/chat/css
mkdir -p $out/app/chat/js

cp $landscape/chat/index.css $out/app/chat/css/index.css
cp $landscape/chat/index.js $out/app/chat/js/index.js
cp $landscape/chat/tile.js $out/app/chat/js/tile.js

mkdir -p $out/app/clock/js

cp $landscape/clock/tile.js $out/app/clock/js/tile.js

mkdir -p $out/app/launch/css
mkdir -p $out/app/launch/js

cp $landscape/launch/index.css $out/app/launch/css/index.css
cp $landscape/launch/index.js $out/app/launch/js/index.js

mkdir -p $out/app/publish/css
mkdir -p $out/app/publish/js

cp $landscape/publish/index.css $out/app/publish/css/index.css
cp $landscape/publish/index.js $out/app/publish/js/index.js
cp $landscape/publish/tile.js $out/app/publish/js/tile.js

mkdir -p $out/app/soto/css
mkdir -p $out/app/soto/js

cp $landscape/soto/index.css $out/app/soto/css/index.css
cp $landscape/soto/index.js $out/app/soto/js/index.js
cp $landscape/soto/tile.js $out/app/soto/js/tile.js

mkdir -p $out/app/weather/js

cp $landscape/weather/tile.js $out/app/weather/js/tile.js

