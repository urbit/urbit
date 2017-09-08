{ crossenv, xproto, xextproto, libx11, fixesproto }:

let
  version = "5.0.3";

  name = "libxfixes-${version}";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://www.x.org/releases/individual/lib/libXfixes-${version}.tar.bz2";
    sha256 = "1miana3y4hwdqdparsccmygqr3ic3hs5jrqfzp70hvi2zwxd676y";
  };

  lib = crossenv.make_derivation rec {
    inherit version name src;

    builder = ./builder.sh;

    configure_flags =
      "--host=${crossenv.host} " +
      "--enable-static " +
      "--disable-shared";

    cross_inputs = [ xproto xextproto libx11 fixesproto ];

    inherit xproto libx11 fixesproto;
  };

  license = crossenv.native.make_derivation {
    name = "${name}-license";
    inherit src;
    builder = ./license_builder.sh;
  };

  license_set =
    xproto.license_set //
    xextproto.license_set //
    libx11.license_set //
    fixesproto.license_set //
    { "${name}" = license; };

in
  lib // { inherit license_set; }
