{
  pkgs,
  debug,
  ivory ? ../../../bin/ivory.pill,
  argon2, ed25519, ent, ge-additions, h2o, murmur3, scrypt, secp256k1, sni, softfloat3, uv
}:

let

  name =
    if debug then "urbit-debug" else "urbit";

  deps =
    with pkgs;
    [ curl gmp libsigsegv ncurses openssl zlib lmdb cacert xxd ];

  vendor =
    [ argon2 softfloat3 ed25519 ent ge-additions h2o scrypt uv murmur3 secp256k1 sni ];

in

pkgs.stdenv.mkDerivation {
  name    = name;
  exename = name;
  src     = ../../../pkg/urbit;
  builder = ./builder.sh;

  nativeBuildInputs = deps ++ vendor;

  # See https://github.com/NixOS/nixpkgs/issues/18995
  hardeningDisable = if debug then [ "all" ] else [];

  CFLAGS           = if debug then "-O3 -g -Werror" else "-O3 -Werror";
  IVORY            = ivory;
  MEMORY_DEBUG     = debug;
  CPU_DEBUG        = debug;
  EVENT_TIME_DEBUG = false;
}
