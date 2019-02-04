{ crossenv, libxcb }:

let
  version = "0.8.2";

  name = "libxkbcommon-${version}";

  nixpkgs = crossenv.nixpkgs;

  src = nixpkgs.fetchurl {
    url = "https://github.com/xkbcommon/libxkbcommon/archive/xkbcommon-${version}.tar.gz";
    sha256 = "08k30k9nx70swik2816rsxf40ll9xsmsfar9a6krvg5wxx58f6gx";
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
