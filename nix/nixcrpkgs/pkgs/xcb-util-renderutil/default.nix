{ crossenv, libxcb }:

let
  version = "0.3.9";

  name = "xcb-util-renderutil"; # TODO: add -${version} (mass rebuild)

  src = crossenv.nixpkgs.fetchurl {
    url = "https://xcb.freedesktop.org/dist/xcb-util-renderutil-${version}.tar.bz2";
    sha256 = "0nza1csdvvxbmk8vgv8vpmq7q8h05xrw3cfx9lwxd1hjzd47xsf6";
  };

  lib = crossenv.make_derivation {
    inherit version name src;

    # TODO: rename all xcb-util builders to builder.sh (mass rebuild)
    builder = ./util_renderutil_builder.sh;

    configure_flags =
      "--host=${crossenv.host} " +
      "--enable-static " +
      "--disable-shared";

    cross_inputs = [ libxcb ];

    xcb = libxcb;
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
