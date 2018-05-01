{ crossenv }:

# TODO: SDL integration would be nice, so we can use noir.ion

let
  version = "7524dc7";  # 2018-04-30

  name = "ion-${version}";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/DavidEGrayson/bitwise/archive/${version}.tar.gz";
    sha256 = "169j7yhphvcyfbqgi5p1i4lhd9n5a31n99fv2kxyrh7djmr8g2s9";
  };

  ion = crossenv.make_derivation {
    inherit version name src;
    builder = ./builder.sh;
  };

in
  ion
