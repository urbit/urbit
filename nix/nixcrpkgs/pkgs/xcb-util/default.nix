{ crossenv, libxcb }:

let
  version = "0.4.0";

  name = "xcb-util-${version}";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://xcb.freedesktop.org/dist/xcb-util-${version}.tar.bz2";
    sha256 = "1sahmrgbpyki4bb72hxym0zvxwnycmswsxiisgqlln9vrdlr9r26";
  };

  lib = crossenv.make_derivation rec {
    inherit version name src;

    builder = ./util_builder.sh;

    configure_flags =
      "--host=${crossenv.host} " +
      "--enable-static " +
      "--disable-shared";

    cross_inputs = [ libxcb ];
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
