let

  nixpkgs  = import ./nixpkgs.nix;
  sdk_file = ../bin/MacOSX10.11.sdk.tar.xz;

in

import ./nixcrpkgs/top.nix { inherit sdk_file nixpkgs; }
