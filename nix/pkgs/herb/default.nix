{ pkgs ? import ../../nixpkgs.nix { } }:

let

  python = pkgs.python2.withPackages (py: [ py.requests ]);

in

pkgs.stdenv.mkDerivation {
  name         = "herb";
  src          = ../../../pkg/herb/herb;
  builder      = ./builder.sh;
  buildInputs  = [ python ];
}
