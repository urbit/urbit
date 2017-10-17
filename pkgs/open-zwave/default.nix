# TODO: this is far from complete, need to build and test an example program with it

{ crossenv }:

let

  lib = crossenv.make_derivation rec {
    name = "open-zwave-${version}";
    version = "1.5";
    src = crossenv.nixpkgs.fetchurl {
      url = "https://github.com/DavidEGrayson/open-zwave/archive/8ad5667.tar.gz";
      sha256 = "1f4rgzrkkz097fj997n174czbymlz31f70h9m3ljzrysdskrbmx6";
    };
    builder = ./builder.sh;
    platform = crossenv.os;
    CFLAGS = "-Wall -I../ozw/cpp/src -I../ozw/cpp/tinyxml -I../ozw/cpp/hidapi/hidapi";
  };

  examples = crossenv.make_derivation rec {
    name = "open-zwave-examples";
    src = lib.src;
    builder = ./examples_builder.sh;
    inherit lib;
  };

in
  lib // { inherit examples; }
