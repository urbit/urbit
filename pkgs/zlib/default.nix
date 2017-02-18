{ crossenv }:

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "zlib-${version}-${crossenv.host}";

  version = "1.2.11";

  src = crossenv.nixpkgs.fetchurl {
    url = "http://zlib.net/zlib-${version}.tar.gz";
    sha256 = "18dighcs333gsvajvvgqp8l4cx7h1x7yx9gd5xacnk80spyykrf3";
  };

  builder = ./builder.sh;

  buildInputs = [
    crossenv.gcc
    crossenv.binutils
  ];

  inherit (crossenv) host;
}
