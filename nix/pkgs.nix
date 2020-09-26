{ system ? builtins.currentSystem
, crossSystem ? null
, sourcesOverride ? { }
, configOverride ? { } 
, overlaysOverride ? [ ]
}:

let

  sources = import ./sources.nix { inherit pkgs; } // sourcesOverride;

  haskellNix = import sources."haskell.nix" {
    sourcesOverride = {
      hackage = sources."hackage.nix";
      stackage = sources."stackage.nix";
    };
  };

  config = haskellNix.config // configOverride;

  overlays = [
    # Add top-level `.sources` attribute.
    (_final: _prev: { inherit sources; })

    # General native nixpkgs package overrides.
    (import ./overlays/nixpkgs.nix)

    # Additional overrides guarded by the host platform so we can
    # apply them unconditionally.
    (import ./overlays/darwin.nix)
    (import ./overlays/musl.nix)
  ] ++ haskellNix.overlays
    ++ overlaysOverride;

  pkgs = import sources.nixpkgs {
    inherit system crossSystem config overlays;
  };

in pkgs
