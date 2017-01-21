{ crossenv }:

# TODO: get rid of boilerplate, don't refer to nixpkgs
crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "hello-${crossenv.host}";
  inherit (crossenv) host;
  buildInputs = [ crossenv.gcc crossenv.binutils ];
  src_file = ./hello.c;
  builder = ./builder.sh;
}
