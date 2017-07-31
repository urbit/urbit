{ crossenv, xcb-proto, libxau, xproto }:

let
  lib = crossenv.make_derivation rec {
    name = "libxcb-${version}";
    version = "1.12";

    src = crossenv.nixpkgs.fetchurl {
      url = "https://xcb.freedesktop.org/dist/libxcb-${version}.tar.bz2";
      sha256 = "0nvv0la91cf8p5qqlb3r5xnmg1jn2wphn4fb5jfbr6byqsvv3psa";
    };

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

  util = crossenv.make_derivation rec {
    name = "xcb-util-${version}";
    version = "0.4.0";

    src = crossenv.nixpkgs.fetchurl {
      url = "https://xcb.freedesktop.org/dist/xcb-util-${version}.tar.bz2";
      sha256 = "1sahmrgbpyki4bb72hxym0zvxwnycmswsxiisgqlln9vrdlr9r26";
    };

    builder = ./util_builder.sh;

    configure_flags =
      "--host=${crossenv.host} " +
      "--enable-static " +
      "--disable-shared";

    cross_inputs = [ lib ];
  };

  util-image = crossenv.make_derivation rec {
    name = "xcb-util-image-${version}";
    version = "0.4.0";

    src = crossenv.nixpkgs.fetchurl {
      url = "https://xcb.freedesktop.org/dist/xcb-util-image-${version}.tar.bz2";
      sha256 = "1z1gxacg7q4cw6jrd26gvi5y04npsyavblcdad1xccc8swvnmf9d";
    };

    builder = ./util_image_builder.sh;

    configure_flags =
      "--host=${crossenv.host} " +
      "--enable-static " +
      "--disable-shared";

    cross_inputs = [ lib util ];
  };

  util-keysyms = crossenv.make_derivation rec {
    name = "xcb-util-keysyms";
    version = "0.4.0";

    src = crossenv.nixpkgs.fetchurl {
      url = "https://xcb.freedesktop.org/dist/xcb-util-keysyms-${version}.tar.bz2";
      sha256 = "1nbd45pzc1wm6v5drr5338j4nicbgxa5hcakvsvm5pnyy47lky0f";
    };

    builder = ./util_keysyms_builder.sh;

    configure_flags =
      "--host=${crossenv.host} " +
      "--enable-static " +
      "--disable-shared";

    cross_inputs = [ lib ];
  };

  util-wm = crossenv.make_derivation rec {
    name = "xcb-util-wm-${version}";
    version = "0.4.1";

    src = crossenv.nixpkgs.fetchurl {
      url = "https://xcb.freedesktop.org/dist/xcb-util-wm-${version}.tar.bz2";
      sha256 = "0gra7hfyxajic4mjd63cpqvd20si53j1q3rbdlkqkahfciwq3gr8";
    };

    builder = ./util_wm_builder.sh;

    configure_flags =
      "--host=${crossenv.host} " +
      "--enable-static " +
      "--disable-shared";

    cross_inputs = [ lib ];

    native_inputs = [ crossenv.nixpkgs.m4 ];
  };

  examples = crossenv.make_derivation rec {
    name = "xcb-examples";

    builder = ./examples_builder.sh;

    cross_inputs = [ lib ];

    example1 = ./example1.c;
  };
in
  lib // { inherit util util-image util-keysyms util-wm examples; }
