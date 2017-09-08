{ crossenv, libxcb, xcb-util }:

let
  version = "0.4.0";

  name = "xcb-util-image-${version}";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://xcb.freedesktop.org/dist/xcb-util-image-${version}.tar.bz2";
    sha256 = "1z1gxacg7q4cw6jrd26gvi5y04npsyavblcdad1xccc8swvnmf9d";
  };

  lib = crossenv.make_derivation rec {
    inherit version name src;
    builder = ./util_image_builder.sh;

    configure_flags =
      "--host=${crossenv.host} " +
      "--enable-static " +
      "--disable-shared";

    cross_inputs = [ libxcb xcb-util ];

    inherit libxcb;
    libxcb_util = xcb-util;
  };

  license = crossenv.native.make_derivation {
    name = "${name}-license";
    inherit src;
    builder = ./license_builder.sh;
  };

  license_set =
    libxcb.license_set //
    xcb-util.license_set //
    { "${name}" = license; };

in
  lib // { inherit license_set; }
