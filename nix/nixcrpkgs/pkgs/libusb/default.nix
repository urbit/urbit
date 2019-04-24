{ crossenv, libudev }:

let
  version = "1.0.22";

  name = "libusbp-${version}";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/libusb/libusb/releases/download/v1.0.22/libusb-1.0.22.tar.bz2";
    sha256 = "0mw1a5ss4alg37m6bd4k44v35xwrcwp5qm4s686q1nsgkbavkbkm";
  };

  lib = crossenv.make_derivation {
    inherit version name src;
    builder = ./builder.sh;
    libudev = if crossenv.os == "linux" then libudev else null;
  };

in
  lib
