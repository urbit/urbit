{ crossenv }:
let pkgs =
rec {

  inherit (crossenv) binutils gcc;

  hello = import ./pkgs/hello { inherit crossenv; };

};
in pkgs
