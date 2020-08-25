{ sources ? import ./sources.nix, ... }@args:

let

  haskellNix = import sources.haskell-nix { };

  nixpkgsArgs = haskellNix.nixpkgsArgs // args;

  # By using haskell.nix's own pins we should get a higher cache
  # hit rate from `cachix use iohk`.
  nixpkgs = import haskellNix.sources.nixpkgs-2003 nixpkgsArgs;

  fetchgithublfs = import ./fetchgithublfs { pkgs = nixpkgs; };

in

nixpkgs // {
  inherit sources fetchgithublfs;
}
