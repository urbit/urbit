{ crossenv, xcb-proto, libxau }:

let
  version = "1.12";

  name = "libxcb-${version}";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://xcb.freedesktop.org/dist/libxcb-${version}.tar.bz2";
    sha256 = "0nvv0la91cf8p5qqlb3r5xnmg1jn2wphn4fb5jfbr6byqsvv3psa";
  };

  lib = crossenv.make_derivation rec {
    inherit version name src;

    patches = [ ./no-pthread-stubs.patch ];

    builder = ./builder.sh;

    configure_flags =
      "--host=${crossenv.host} " +
      "--enable-static " +
      "--disable-shared " +
      "--enable-xinput " +
      "--enable-xkb";

    cross_inputs = [ xcb-proto libxau ];

    inherit libxau;

    native_inputs = [ crossenv.nixpkgs.python2 ];
  };

  examples = crossenv.make_derivation rec {
    name = "libxcb-examples";

    builder = ./examples_builder.sh;

    cross_inputs = [ lib ];

    example1 = ./example1.c;
  };

  license = crossenv.native.make_derivation {
    name = "${name}-license";
    inherit src;
    builder = ./license_builder.sh;
  };

  license_set =
    xcb-proto.license_set //
    libxau.license_set //
    { "${name}" = license; };

in
  lib // { inherit examples license_set; }
