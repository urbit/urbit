let

  nixpkgs = import ./nixpkgs.nix;

in

import ./nixcrpkgs/top.nix { inherit nixpkgs; }
