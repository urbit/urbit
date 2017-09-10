{ crossenv, xorg-macros }:

let
  version = "7.0.31";

  name = "xproto-${version}";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://www.x.org/releases/individual/proto/xproto-${version}.tar.gz";
    sha256 = "1is3xl0zjk4l0d8d0zinkfbfapgdby2i56jjfp6caibvwam5wxbd";
  };

  lib = crossenv.make_derivation {
    inherit version name src;

    builder = ./builder.sh;

    configure_flags =
      "--host=${crossenv.host} " +
      "--enable-static " +
      "--disable-shared";

    cross_inputs = [ xorg-macros ];

    # Need the latest version of config.sub so we can support musl.
    gnu_config = crossenv.native.gnu_config;
  };

  license = crossenv.native.make_derivation {
    name = "${name}-license";
    inherit src;
    builder = ./license_builder.sh;
  };

  license_set = xorg-macros.license_set // { "${name}" = license; };

in
  lib // { inherit license_set; }
