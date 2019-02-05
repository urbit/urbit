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

  # TODO: move this to its own directory in pkgs so it's easier to
  # remember to update it
  gnu_config = nixpkgs.fetchgit {
    url = "https://git.savannah.gnu.org/git/config.git";
    rev = "286a38db91ea2dce1749ab7d1d9ea5ae344a16c1";
    sha256 = "086z8298d6sy75kqj0f09dpqni347j1d7sjagdj926zhkc584wii";
  };

  native = native_base // {
    default_native_inputs = native_base.default_native_inputs ++ [
      pkgconf
    ];

    inherit pkgconf gnu_config;

    make_derivation = import ../make_derivation.nix native;
  };

in native
