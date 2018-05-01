{ crossenv }:

# TODO: SDL integration would be nice, so we can use noir.ion

let
  version = "f2c00c9";  # 2018-04-30

  name = "ion-${version}";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/DavidEGrayson/bitwise/archive/${version}.tar.gz";
    sha256 = "0w4cac9b4l51hv1j2wr2i4a0kdrs27zc80shq1hdh444gpnj0pcl";
  };

  ion = crossenv.make_derivation {
    inherit version name src;
    builder = ./builder.sh;
  };

in
  ion
