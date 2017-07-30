{ nixpkgs }:

{
  recurseForDerivations = true;

  inherit nixpkgs;

  make_derivation = import ../make_derivation.nix nixpkgs null;

  pkgconf = import ./pkgconf { inherit nixpkgs; };

  wrappers = import ./wrappers { inherit nixpkgs; };

  gnu_config = nixpkgs.fetchgit {
    url = "https://git.savannah.gnu.org/git/config.git";
    rev = "81497f5aaf50a12a9fe0cba30ef18bda46b62959";
    sha256 = "1fq0nki2118zwbc8rdkqx5i04lbfw7gqbsyf5bscg5im6sfphq1d";
  };
}
