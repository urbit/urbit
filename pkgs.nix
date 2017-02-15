{ crossenv }:
let pkgs =
rec {
  inherit (crossenv) binutils gcc;

  hello = import ./pkgs/hello {
    inherit crossenv;
  };

  hello_cpp = import ./pkgs/hello_cpp {
    inherit crossenv;
  };

  libusbp = import ./pkgs/libusbp {
    inherit crossenv;
  };

  p-load = import ./pkgs/p-load {
    inherit crossenv libusbp;
  };

  angle = import ./pkgs/angle {
    inherit crossenv;
  };

  angle-samples = import ./pkgs/angle/samples.nix {
    inherit crossenv angle;
  };

  qt58 = import ./pkgs/qt58 {
    inherit crossenv angle;
  };
};
in pkgs
