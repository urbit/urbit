source $stdenv/setup

cp -r $src/ $out

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

