{ nixpkgs, arch }:
let
  binutils = import ./binutils { inherit nixpkgs arch; };
in {
  inherit binutils nixpkgs arch;
}
