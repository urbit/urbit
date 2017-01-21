{ crossenv }:
let pkgs =
rec {

  inherit (crossenv) binutils gcc;

  hello = import ./pkgs/hello { inherit crossenv; };

  libusbp = import ./pkgs/libusbp { inherit crossenv; };

};
in pkgs
