{ nixpkgs }:

let
  native_base = {
    inherit nixpkgs;

    is_cross = false;

    default_native_inputs = [
      nixpkgs.bashInteractive
      nixpkgs.binutils
      (nixpkgs.binutils-unwrapped or nixpkgs.binutils)
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

    make_derivation = import ../make_derivation.nix native_base;
  };

  pkgconf = import ./pkgconf { env = native_base; };

  wrappers = import ./wrappers { env = native_base; };

  gnu_config = nixpkgs.fetchgit {
    url = "https://git.savannah.gnu.org/git/config.git";
    rev = "81497f5aaf50a12a9fe0cba30ef18bda46b62959";
    sha256 = "1fq0nki2118zwbc8rdkqx5i04lbfw7gqbsyf5bscg5im6sfphq1d";
  };

  native = native_base // {
    default_native_inputs = native_base.default_native_inputs ++ [
      pkgconf
    ];

    inherit pkgconf wrappers gnu_config;

    make_derivation = import ../make_derivation.nix native;
  };

in native
