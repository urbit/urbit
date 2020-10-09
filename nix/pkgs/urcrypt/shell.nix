let
  pkgs = import ../../nixpkgs.nix;
  deps = import ../../deps { inherit pkgs; };
in

import ./default.nix {
  inherit pkgs;
  inherit deps;
}
