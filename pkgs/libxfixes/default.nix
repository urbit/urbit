{ crossenv, xorgproto, libx11 }:

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

    cross_inputs = [ xorgproto libx11 ];

    inherit xorgproto libx11;
  };

  license = crossenv.native.make_derivation {
    name = "${name}-license";
    inherit src;
    builder = ./license_builder.sh;
  };

  license_set =
    xorgproto.license_set //
    libx11.license_set //
    { "${name}" = license; };

in
  lib // { inherit license_set; }
