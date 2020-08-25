{ pkgs }:

let

  osx =
    pkgs.lib.optionals pkgs.stdenv.isDarwin
      (with pkgs.darwin.apple_sdk.frameworks; [ Cocoa CoreServices ]);

in pkgs.stdenv.mkDerivation {
  name        = "uv";
  src         = pkgs.sources.libuv;
  buildInputs = osx ++ [ pkgs.autoconf pkgs.automake pkgs.libtool pkgs.m4 ];
  builder     = ./builder.sh;

  configureFlags = [ "--disable-shared" ];
  CFLAGS         = "-fPIC";
}
