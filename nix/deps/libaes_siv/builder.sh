source $stdenv/setup

cp -r $src ./src
chmod -R u+w ./src
cd ./src

cmake -DCMAKE_INSTALL_PREFIX=$out . && \
make && \
make test && \
make install
