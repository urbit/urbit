{ system ? builtins.currentSystem
, crossSystem ? null
, overlays ? [ ]
, crossOverlays ? [ ]
, config ? { }
, sources ? { } 
}:

let

  allSources = import ./sources.nix { inherit pkgs; } // sources;

  haskellNix = import allSources."haskell.nix" {
    sourcesOverride = {
      hackage = allSources."hackage.nix";
      stackage = allSources."stackage.nix";
    };
  };

  extraOverlays = [
    # Add top-level `.sources` attribute.
    (_final: _prev: { sources = allSources; })

    # General native nixpkgs package overrides.
    (import ./overlays/nixpkgs.nix)
  ];

  extraCrossOverlays = [
    # Add general overrides guarded by the host platform so
    # we can apply them unconditionally.
    (import ./overlays/darwin.nix)
    (import ./overlays/musl.nix)
  ];

  pkgs = import allSources.nixpkgs {
    inherit system crossSystem;

    crossOverlays = extraCrossOverlays ++ crossOverlays;
    overlays = haskellNix.overlays ++ extraOverlays ++ overlays;
    config = haskellNix.config // config;
  };

in pkgs
