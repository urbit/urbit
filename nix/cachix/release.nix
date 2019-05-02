let

  util         = import ./util.nix;
  nixcrpkgs    = import ../nixcrpkgs.nix;
  release      = import ../release.nix;
  all_releases = util.flattenSetPrefix release;
  crosstools   = { inherit (nixcrpkgs.native) pkgconf; };

in

  crosstools // all_releases
