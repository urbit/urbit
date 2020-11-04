# The build system where packages will be _built_.
{ system ? builtins.currentSystem
  # The host system where packages will _run_.
, crossSystem ? null
  # Additional sources.json overrides.
, sources ? { }
  # Additional nixpkgs.config overrides.
, config ? { }
  # Additional nixpkgs.overlays.
, overlays ? [ ]
  # Overlays to apply to the last package set in cross compilation.
, crossOverlays ? [ ] }:

let

  finalSources = import ./sources.nix { inherit pkgs; } // sources;

  haskellNix = import finalSources."haskell.nix" {
    sourcesOverride = {
      hackage = finalSources."hackage.nix";
      stackage = finalSources."stackage.nix";
    };
  };

  finalOverlays = haskellNix.overlays ++ [
    # Add top-level .sources attribute for other overlays to access niv sources.
    (_final: _prev: { sources = finalSources; })

    # General unguarded (native) overrides for nixpkgs.
    (import ./overlays/native.nix)

    # Specific overrides guarded by the host platform.
    (import ./overlays/musl.nix)
  ] ++ overlays;

  pkgs = import finalSources.nixpkgs {
    inherit system crossSystem crossOverlays;

    config = haskellNix.config // config;
    overlays = finalOverlays;
  };

in pkgs // {
  pkgsStatic = pkgs.pkgsStatic.extend (import ./overlays/static.nix);
}
