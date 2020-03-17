source $setup

$host-gcc $src_file -o hello$exe_suffix

mkdir -p $out/bin

cp hello$exe_suffix $out/bin/
