{ pkgs }:

{ src
, name ? null
, owner ? "urbit"
, repo ? "urbit"
, preferLocalBuild ? true 
}:

pkgs.stdenvNoCC.mkDerivation {
  # Doesn't work in a sandbox. You'll need to pass `--option sandbox relaxed`,
  # or add `sandbox = "relaxed"` to nix.conf.
  __noChroot = true;

  name = "lfs-" + (if name != null then name else baseNameOf src);
  builder = ./builder.sh;

  nativeBuildInputs = [
    pkgs.coreutils
    pkgs.curl
    pkgs.jq
  ];

  impureEnvVars = pkgs.stdenvNoCC.lib.fetchers.proxyImpureEnvVars;

  inherit src owner repo preferLocalBuild;
}
