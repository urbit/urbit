let

  nixpkgs = import ./nixpkgs.nix;
  osx_sdk = ../bin/MacOSX10.11.sdk.tar.xz;

in

import ./nixcrpkgs/top.nix { inherit osx_sdk nixpkgs; }
