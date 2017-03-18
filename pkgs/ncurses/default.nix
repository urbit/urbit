# NOTE: ncurses currently does not compile for mingw because it cannot
# find its own _nc_mingw_tcgetattr and _nc_mingw_tcsetattr functions.
# We will probably just use pdcurses instead.

{ crossenv }:

if true then "not working yet" else

crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "ncurses-${version}-${crossenv.host}";

  version = "6.0";

  src = crossenv.nixpkgs.fetchurl {
    url = "mirror://gnu/ncurses/ncurses-${version}.tar.gz";
    sha256 = "0q3jck7lna77z5r42f13c4xglc7azd19pxfrjrpgp2yf615w4lgm";
  };

  builder = ./builder.sh;

  buildInputs = [
    crossenv.gcc
    crossenv.binutils
  ];

  inherit (crossenv) host os;
}
