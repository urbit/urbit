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

  sourcesFinal = import ./sources.nix { inherit pkgs; } // sources;

  haskellNix = import sourcesFinal."haskell.nix" {
    sourcesOverride = {
      hackage = sourcesFinal."hackage.nix";
      stackage = sourcesFinal."stackage.nix";
    };
  };

  configFinal = haskellNix.config // config;

  overlaysFinal = haskellNix.overlays ++ [
    (_final: prev: {
      # Add top-level .sources attribute for other overlays to access sources.
      sources = sourcesFinal;

      # Additional non-convential package/exe mappings for shellFor.tools.
      haskell-nix = prev.haskell-nix // {
        toolPackageName = prev.haskell-nix.toolPackageName // {
          shellcheck = "ShellCheck";
        };
      };
    })
    (import ./overlays/haskell.nix)
    # General unguarded (native) overrides for nixpkgs.
    (import ./overlays/native.nix)

    # Specific overrides guarded by the host platform.
    (import ./overlays/musl.nix)
  ] ++ overlays;

  pkgs = import sourcesFinal.nixpkgs {
    inherit system crossSystem crossOverlays;

    config = configFinal;
    overlays = overlaysFinal;
  };

in pkgs // {
  pkgsStatic = pkgs.pkgsStatic.extend (import ./overlays/static.nix);
}
