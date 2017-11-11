source $setup

cat > hello.c <<EOF
#include <stdio.h>
void hello()
{
  printf("hello, world\n");
}
EOF

cat > main.c <<EOF
void hello(void);
int main(int argc, char ** argv)
{
  hello();
  return 0;
}
EOF

$host-gcc -c hello.c -o hello.o
$host-ar qc hello.a hello.o

ls -l hello.a
# /usr/bin/sha256sum hello.a

# cp hello.a /home/david/nixcrpkgs/hello_good.a

# MYRANLIB=/home/david/osxcross1/osxcross/target/bin/x86_64-apple-darwin15-ranlib
# $MYRANLIB hello.a
$host-ranlib hello.a

# cp hello.a /home/david/nixcrpkgs/hello_bad.a

# /usr/bin/sha256sum hello.a

ls -l hello.a

$host-gcc main.c hello.a -o hello$exe_suffix

mkdir -p $out/bin

cp hello$exe_suffix $out/bin/
