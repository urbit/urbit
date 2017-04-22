crossenv: attrs:

# crossenv.nixpkgs.stdenv.mkDerivation attrs

let
  bash = crossenv.nixpkgs.bash;

  nixpkgs = crossenv.nixpkgs;

  drv_attrs = attrs // rec {
    system = builtins.currentSystem;
    builder = "${bash}/bin/bash";
    args = ["-ue" attrs.builder];
    setup = ./builder_setup.sh;
    SHELL = builder;
    INITIAL_PATH =
      "${crossenv.gcc}/bin:" +
      "${crossenv.binutils}/bin:" +
      "${crossenv.pkg-config}/bin:" +
      "${crossenv.pkg-config-cross}/bin:" +

      "${nixpkgs.cmake}/bin:" +
      "${nixpkgs.ninja}/bin:" +

      "${nixpkgs.gcc}/bin:" +
      "${nixpkgs.binutils}/bin:" +

      "${nixpkgs.coreutils}/bin:" +
      "${nixpkgs.findutils}/bin:" +
      "${nixpkgs.diffutils}/bin:" +
      "${nixpkgs.gnused}/bin:" +
      "${nixpkgs.gnugrep}/bin:" +
      "${nixpkgs.gawk}/bin:" +
      "${nixpkgs.gnutar}/bin:" +
      "${nixpkgs.gzip}/bin:" +
      "${nixpkgs.bzip2}/bin:" +
      "${nixpkgs.gnumake}/bin:" +
      "${nixpkgs.bash}/bin:" +
      "${nixpkgs.patch}/bin:" +
      "${nixpkgs.xz}/bin";
  };

in
  derivation drv_attrs
