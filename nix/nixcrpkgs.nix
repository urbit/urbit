let

  nixpkgs = import ./nixpkgs.nix;

  osx_sdk = builtins.fetchurl {
    sha256 = "89aa34dfe5bcbc7d53d3c55a84b35ac810ecfbcdd16a64c9667992b0c36c60c4";
    url = "https://github.com/phracker/MacOSX-SDKs/releases/download/10.13/MacOSX10.11.sdk.tar.xz";
  };

in

import ./nixcrpkgs/top.nix { inherit osx_sdk nixpkgs; }
