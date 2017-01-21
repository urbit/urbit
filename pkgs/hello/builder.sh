source $stdenv/setup

exec_suffix=.exe

$host-gcc $src_file -o hello$exec_suffix
$host-strip hello$exec_suffix

mkdir -p $out/bin/

cp hello$exec_suffix $out/bin/

