{ crossenv, xorg-macros, xorgproto }:

let
  version = "1.0.8";

  name = "libxau-${version}";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://www.x.org/archive/individual/lib/libXau-${version}.tar.bz2";
    sha256 = "1wm4pv12f36cwzhldpp7vy3lhm3xdcnp4f184xkxsp7b18r7gm7x";
  };

  lib = crossenv.make_derivation rec {
    inherit version name src;

    builder = ./builder.sh;

    configure_flags =
      "--host=${crossenv.host} " +
      "--enable-static " +
      "--disable-shared";

    cross_inputs = [ xorg-macros xorgproto ];

    inherit xorgproto;
  };

  license = crossenv.native.make_derivation {
    name = "${name}-license";
    inherit src;
    builder = ./license_builder.sh;
  };

  license_set =
    xorg-macros.license_set //
    xorgproto.license_set //
    { "${name}" = license; };

in
  lib // { inherit license_set; }
