{ crossenv }:

# TODO: get rid of boilerplate, don't refer to nixpkgs
crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "hello-${crossenv.host}";

  inherit (crossenv) host;

  src_file = ./hello.cpp;

  buildInputs = [ crossenv.gcc crossenv.binutils ];

  builder = ./builder.sh;
}
