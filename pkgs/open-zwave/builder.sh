source $setup

tar -xf $src
mv open-zwave-* ozw

mkdir build
cd build

for n in Driver Group Manager Msg Node Notification Options Scene Utils ZWSecurity platform/Event platform/Controller platform/FileOps platform/HidController platform/Log platform/Mutex platform/SerialController platform/Stream platform/Thread platform/TimeStamp platform/Wait platform/$platform/{Event,FileOps,Log,Mutex,SerialController,Thread,TimeStamp,Wait}Impl; do
  $host-g++ -c $CFLAGS ../ozw/cpp/src/$n.cpp -o $(basename $n).o
  chmod a-w $(basename $n).o
done

$host-ar cr libopenzwave.a *.o

mkdir -p $out/lib/pkgconfig
cp *.a $out/lib

cat > $out/lib/pkgconfig/libopenzwave.pc <<EOF
prefix=$out
libdir=\${prefix}/lib
includedir=\${prefix}/include

Version: $version
Libs: -L\${libdir} -lopenzwave
Cflags: -I\${includedir}
EOF
