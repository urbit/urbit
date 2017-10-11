# TODO: this is far from complete, need to build and test an example program with it

{ crossenv }:

let
  lib = crossenv.make_derivation rec {
    name = "open-zwave-${version}";
    version = "1.5";
    src = crossenv.nixpkgs.fetchurl {
      url = "https://github.com/DavidEGrayson/open-zwave/archive/04902eb.tar.gz";
      sha256 = "1mrlkzya39w1ycx10y8qm0fxx37l8rfj59pjyfgv8gb95pw0izrs";
    };
    builder = ./builder.sh;
    platform = crossenv.os;
    CFLAGS = "-Wall -I../ozw/cpp/src -I../ozw/cpp/tinyxml -I../ozw/cpp/hidapi/hidapi";
  };

in
  lib
