{
  pkgs,
  name ? "urbit",
  debug ? false,
  argon2, ed25519, ent, h2o, murmur3, scrypt, secp256k1, sni, softfloat3, uv
}:

let

  deps =
    with pkgs;
    [ curl gmp libsigsegv ncurses openssl zlib ];

  util =
    [ pkgs.astyle ];

  vendor =
    [ argon2 softfloat3 ed25519 ent h2o scrypt uv murmur3 secp256k1 sni ];


  # osx =
    # with pkgs;
    # lib.optionals stdenv.isDarwin (
      # with darwin.apple_sdk.frameworks;
        # [ Cocoa CoreServices ]);

  # NIX_LDFLAGS =
    # pkgs.lib.optionalString pkgs.stdenv.isDarwin
      # "-framework CoreServices -framework CoreFoundation";

in

pkgs.stdenv.mkDerivation {
  name    = name;
  exename = name;
  src     = ../../../pkg/urbit;
  builder = ./builder.sh;

  nativeBuildInputs = deps ++ vendor ++ util;

  # See https://github.com/NixOS/nixpkgs/issues/18995
  hardeningDisable = if debug then [ "all" ] else [];

  CFLAGS           = if debug then "-O0 -g" else "-O3";
  MEMORY_DEBUG     = debug;
  CPU_DEBUG        = debug;
  EVENT_TIME_DEBUG = false;
}
