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

  finalSources = import ./sources.nix { } // sources;

  pkgs = import finalSources.nixpkgs {
    inherit system config crossSystem crossOverlays;

    overlays = [
      # Make prev.sources available to subsequent overlays.
      (_final: _prev: { sources = finalSources; })
      # General unguarded (native) overrides for nixpkgs.
      (import ./overlays/native.nix)
      # Specific overrides guarded by the host platform.
      (import ./overlays/musl.nix)
    ];
  };

in pkgs // {
  pkgsStatic = pkgs.pkgsStatic.extend (import ./overlays/static.nix);
}
