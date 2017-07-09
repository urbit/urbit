source $setup

# TODO: remove -static once the musl toolchain is fixed
$host-gcc -static $src_file -o hello$exe_suffix

mkdir -p $out/bin/

cp hello$exe_suffix $out/bin/
