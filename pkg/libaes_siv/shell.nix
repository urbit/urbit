{ pkgs ? import <nixpkgs> {} }:
with pkgs;
stdenv.mkDerivation {
  name = "libaes_siv";
  buildInputs = [ cmake openssl ];
}
