{ recurseIntoAttrs, haskell-nix }:

# Borrowed from https://github.com/cachix/ghcide-nix/pull/4/files#diff-70bfff902f4dec33e545cac10ee5844d
# Tweaked to use builtins.mapAttrs instead of needing the one from nixpkgs lib
rec {
  /*
    dimension: name -> attrs -> function -> attrs
    where
      function: keyText -> value -> attrsOf package
    WARNING: Attribute names must not contain periods (".").
             See https://github.com/NixOS/nix/issues/3088
    NOTE: The dimension name will be picked up by agent and web ui soon.
    Specifies a dimension of the build matrix. For example
        dimensionWith "Example" {
          withP = { p = true; }
          withoutP = { p = false; }
        } (key:      # either "withP" or "withoutP"
          { p }:     # either p = true or p = false
          myProject p
        )
    evaluates roughly to
        {
          withP = myProject true;
          withoutP = myProject false;
        }
    Use nested calls for multiple dimensions.
    Example:
        dimensionWith "System" {
          "x86_64-linux" = {};
          # ...
          }: (system: {}:
          dimensionWith "Nixpkgs release" (
            {
              "nixpkgs-19_03".nixpkgs = someSource
            } // optionalAttrs (system != "...") {
              "nixpkgs-unstable".nixpkgs = someOtherSource
            }
            ) (_key: { nixpkgs }:
            myProject system nixpkgs
          )
        )
    evaluates roughly to
        {
          x86_64-linux.nixpkgs-19_03 = myProject "x86_64-linux" someSource;
          x86_64-linux.nixpkgs-unstable = myProject "x86_64-linux" someOtherSource;
          ...
        }
    If you need to make references across attributes, you can do so by binding
    the result. Wherever you write
        dimension "My dimension" {} (key: value: f1 key value)
    You can also write
        let
          myDimension = dimensionWith "My dimension" {} (key: value: f2 key value myDimension)
        in
          myDimension
    This example builds a single test runner to reuse across releases:
        let
          overlay =
            testRunnerPkgs: self: super: {
              # ...
            };
          myProject =
            { nixpkgs,
              pkgs ? import nixpkgs { overlays = [ overlay ]; },
              testRunnerPkgs ? pkgs
            }: pkgs;
        in
        let
          latest = "nixpkgs-19_03";
          releases =
            dimensionWith "Nixpkgs release"
              {
                nixpkgs-18_09.nixpkgs = someSource
                nixpkgs-19_03.nixpkgs = someOtherSource
              }
              (_key: { nixpkgs }:
                myProject {
                  inherit nixpkgs;
                  testRunnerPkgs = releases."${latest}";
                }
              );
        in releases;
   */

  dimension = name: attrs:
    dimensionWith name attrs (_k: v: v);

  dimensionWith = name: attrs: f:
    recurseIntoAttrs
      ((builtins.mapAttrs
        (k: v:
          let o = f k v;
          in o // { recurseForDerivations = o.recurseForDerivations or true; })
        attrs)
      // { meta.dimension.name = name; });

  # These functions pull out from the Haskell package all the components of
  # a particular type - which ci will then build as top-level attributes.
  dimensionHaskell = name: project:
    let
      collectChecks = _: xs:
        recurseIntoAttrs (builtins.mapAttrs (_: x: x.checks) xs);

      collectComponents = type: xs:
        haskell-nix.haskellLib.collectComponents' type xs;

      packages =
        haskell-nix.haskellLib.selectProjectPackages project;

    # This computes the Haskell package set sliced by component type - these
    # are then displayed as the haskell build attributes in hercules ci.
    in recurseIntoAttrs (dimensionWith name {
      library = collectComponents;
      tests = collectComponents;
      benchmarks = collectComponents;
      exes = collectComponents;
      checks = collectChecks;
    } (type: selector: (selector type) packages));
}
