{ crossenv, libxcb }:

let
  version = "0.4.1";

  name = "xcb-util-wm-${version}";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://xcb.freedesktop.org/dist/xcb-util-wm-${version}.tar.bz2";
    sha256 = "0gra7hfyxajic4mjd63cpqvd20si53j1q3rbdlkqkahfciwq3gr8";
  };

  lib = crossenv.make_derivation rec {
    inherit version name src;

    builder = ./util_wm_builder.sh;

    configure_flags =
      "--host=${crossenv.host} " +
      "--enable-static " +
      "--disable-shared";

    cross_inputs = [ libxcb ];

    native_inputs = [ crossenv.nixpkgs.m4 ];
  };

  license = crossenv.native.make_derivation {
    name = "${name}-license";
    inherit src;
    builder = ./license_builder.sh;
  };

  license_set =
    libxcb.license_set //
    { "${name}" = license; };

in
  lib // { inherit license_set; }
