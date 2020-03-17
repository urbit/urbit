{ crossenv }:

crossenv.make_derivation rec {
  name = "hello";
  src_file = ./hello.c;
  builder = ./builder.sh;
}
