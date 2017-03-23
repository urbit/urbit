{ nixpkgs, host, host_as_var }:

nixpkgs.stdenv.mkDerivation rec {
  name = "pkg-config-wrapper-${host}";

  inherit host host_as_var;

  builder = ./builder.sh;
}
