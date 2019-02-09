{ crossenv, libxcb }:

let
  version = "0.8.3";

  name = "libxkbcommon-${version}";

  nixpkgs = crossenv.nixpkgs;

  src = nixpkgs.fetchurl {
    url = "https://github.com/xkbcommon/libxkbcommon/archive/xkbcommon-${version}.tar.gz";
    sha256 = "0schaliwd5garrq3w7c8bg2cpnyr0ijg7583m3q2517iyqis9zmf";
  };

  lib = crossenv.make_derivation rec {
    inherit version name src;

    builder = ./builder.sh;

    configure_flags =
      "--host=${crossenv.host} " +
      "--enable-static " +
      "--disable-shared";

    cross_inputs = [ libxcb ];

    native_inputs = [ nixpkgs.meson nixpkgs.bison ];
  };

  license = crossenv.native.make_derivation {
    name = "${name}-license";
    inherit src;
    builder = ./license_builder.sh;
  };

  license_set =
    { "${name}" = license; };

in
  lib // { inherit license_set; }
