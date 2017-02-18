{ crossenv, zlib, expat, readline }:

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "gdb-${version}-${crossenv.host}";

  # TODO: build gdb
}
