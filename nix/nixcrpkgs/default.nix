import ./top.nix {
  nixpkgs = import <nixpkgs> { };
  osx_sdk = ./macos/MacOSX.sdk.tar.xz;
}
