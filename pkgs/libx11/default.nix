{ crossenv, xorg-macros, xorgproto, libxcb, xtrans }:

let
  version = "1.6.7";

  name = "libx11-${version}";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://xorg.freedesktop.org/releases/individual/lib/libX11-${version}.tar.gz";
    sha256 = "1lara6j4qcmflya34spyhj009n3fpdanlwiqqcfmxdc75a6bhapn";
  };

  lib = crossenv.make_derivation rec {
    inherit version name src;

    builder = ./builder.sh;

    configure_flags =
      "--host=${crossenv.host} " +
      "--disable-malloc0returnsnull " +
      "--enable-static " +
      "--disable-shared";

    cross_inputs = [
      xorg-macros
      xorgproto
      libxcb
      xtrans
    ];

    inherit xorgproto libxcb;
  };

  license = crossenv.native.make_derivation {
    name = "${name}-license";
    inherit src;
    builder = ./license_builder.sh;
  };

  license_set =
    xorg-macros.license_set //
    xorgproto.license_set //
    libxcb.license_set //
    xtrans.license_set //
    { "${name}" = license; };

in
  lib // { inherit license_set; }
