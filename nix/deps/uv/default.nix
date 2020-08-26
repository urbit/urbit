{ pkgs }:

let

  osx =
    pkgs.lib.optionals pkgs.stdenv.isDarwin
      (with pkgs.darwin.apple_sdk.frameworks; [ Cocoa CoreServices ]);

in pkgs.stdenv.mkDerivation {
  name        = "uv";
  src         = pkgs.sources.libuv;
  builder     = ./builder.sh;

  nativeBuildInputs = osx ++ [
    pkgs.autoconf
    pkgs.automake
    pkgs.libtool
    pkgs.m4
  ];

  configureFlags = [ "--disable-shared" ];
  CFLAGS         = "-fPIC";
}
