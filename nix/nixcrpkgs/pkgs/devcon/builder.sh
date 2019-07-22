source $setup

cp --no-preserve=mode -r $src/setup/devcon .

cd devcon
for patch in $patches; do
  echo applying patch $patch
  patch -p1 -i $patch
done
$host-windmc msg.mc
cd ..

mkdir build
cd build

$host-windres ../devcon/devcon.rc rc.o

$host-g++ -municode -O2 \
  -DUNICODE -D_UNICODE \
  ../devcon/*.cpp rc.o \
  -lsetupapi -lole32 \
  -o devcon.exe

mkdir -p $out/bin $out/license
cp devcon.exe $out/bin
cp $src/LICENSE $out/license
