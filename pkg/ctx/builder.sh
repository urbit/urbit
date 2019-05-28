source $stdenv/setup

cp -r $src ./src
chmod -R u+w ./src
cd src

spago build
spago bundle
cp index.js $out
