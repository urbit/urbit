{ nixpkgs }:

let
  native = {
    inherit nixpkgs;

    is_cross = false;

    default_native_inputs = [
      nixpkgs.bashInteractive
      nixpkgs.binutils
      nixpkgs.bzip2
      nixpkgs.cmake
      nixpkgs.coreutils
      nixpkgs.diffutils
      nixpkgs.findutils
      nixpkgs.gcc
      nixpkgs.gawk
      nixpkgs.gnumake
      nixpkgs.gnugrep
      nixpkgs.gnused
      nixpkgs.gnutar
      nixpkgs.gzip
      nixpkgs.ninja
      nixpkgs.patch
      nixpkgs.which
      nixpkgs.xz
    ];

    pkgconf = import ./pkgconf { inherit nixpkgs; };

    wrappers = import ./wrappers { inherit nixpkgs; };

    gnu_config = nixpkgs.fetchgit {
      url = "https://git.savannah.gnu.org/git/config.git";
      rev = "81497f5aaf50a12a9fe0cba30ef18bda46b62959";
      sha256 = "1fq0nki2118zwbc8rdkqx5i04lbfw7gqbsyf5bscg5im6sfphq1d";
    };

    make_derivation = import ../make_derivation.nix native;
  };

in native
