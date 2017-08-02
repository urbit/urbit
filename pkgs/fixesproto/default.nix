{ crossenv, xorg-macros, xextproto }:

crossenv.native.make_derivation rec {
  name = "fixesproto";
  version = "2017-01-26";

  src = crossenv.nixpkgs.fetchgit {
    url = "https://anongit.freedesktop.org/git/xorg/proto/fixesproto";
    rev = "4292ec1c63180c5f4e7c0e606fa68c51913f172b";
    sha256 = "0mmx4cmkbrsmbq1j58g8gcx4h3qz9y4xbjpz7jcl7crki7zrz3kx";
  };

  builder = ./builder.sh;

  native_inputs = [
    crossenv.nixpkgs.autoconf
    crossenv.nixpkgs.automake
  ];

  ACLOCAL_PATH = "${xorg-macros}/lib/aclocal";

  inherit xextproto;
}
