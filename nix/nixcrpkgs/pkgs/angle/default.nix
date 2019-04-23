{ crossenv, gdb, debug ? false }:

if crossenv.os != "windows" then "" else

let
  angle = import ./lib.nix {
    inherit crossenv debug;
  };

  util = import ./util.nix {
    inherit crossenv angle;
  };

  examples = import ./examples.nix {
    inherit crossenv angle;
    angle_util = util;
  };

  debug_bundle = import ./debug_bundle.nix {
    inherit crossenv gdb angle examples;
  };

in angle // { inherit util examples debug_bundle; }
