{ crossenv, gdb, debug ? false }:
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

  # TODO: make a convenient way to generate bundles like this
  # for debugging and release, handle LICENSE files correctly
  debug_bundle = import ./debug_bundle.nix {
    inherit crossenv gdb angle examples;
  };

in angle // { inherit util examples debug_bundle; }
