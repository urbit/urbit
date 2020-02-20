{ crossenv, xorg-macros }:

let
  version = "2019-01-30";

  name = "xorgproto-${version}";

  src = crossenv.nixpkgs.fetchgit {
    url = "https://anongit.freedesktop.org/git/xorg/proto/xorgproto";
    rev = "a06b0e273420d34fe4c1fc0be50d3a5400e1545f";
    sha256 = "0cck4c04wrix75nqc6gsa8yggasjgb1yi10ii8rj8wmzhzd83s21";
  };

  lib = crossenv.native.make_derivation rec {
    inherit version name src;

    builder = ./builder.sh;

    native_inputs = [
      crossenv.nixpkgs.autoconf
      crossenv.nixpkgs.automake
    ];

    ACLOCAL_PATH = "${xorg-macros}/lib/aclocal";
  };

  license = crossenv.native.make_derivation {
    name = "${name}-license";
    inherit src;
    builder = ./license_builder.sh;
  };

  license_set =
    xorg-macros.license_set //
    { "${name}" = license; };

in
  lib // { inherit license license_set; }
