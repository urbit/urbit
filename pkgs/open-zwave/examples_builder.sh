source $setup

tar -xf $src
mv open-zwave-* ozw

mkdir build
cd build

# TODO: combine the examples
$host-g++ -Wall ../ozw/cpp/examples/windows/MinOZW/Main.cpp -o minozw$exe_suffix

$host-strip minozw$exe_suffix

mkdir -p $out/bin
mv minozw$exe_suffix $out/bin
