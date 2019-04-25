{ crossenv }:

crossenv.make_derivation rec {
  name = "hello_cpp";
  src_file = ./hello.cpp;
  builder = ./builder.sh;
}
