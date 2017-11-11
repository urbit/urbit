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

gnu-ar qc gnu-hello.a hello.o
llvm-ar qc llvm-hello.a hello.o
xar -c -f xar-hello.a hello.o

ls -l *.a

echo ranlib
gnu-ranlib llvm-hello.a
gnu-ranlib gnu-hello.a

ls -l *.a

$host-gcc main.c llvm-hello.a -o hello$exe_suffix

mkdir -p $out/bin

cp hello$exe_suffix $out/bin/
