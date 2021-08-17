{ lib, stdenv, coreutils, pkgconfig, argon2u, cacert, ca-bundle, curl
, ed25519, ent, ge-additions, gmp, h2o, herb, ivory, libaes_siv, libscrypt
, libsigsegv, libuv, lmdb, murmur3, openssl, secp256k1, softfloat3, zlib
, enableStatic ? stdenv.hostPlatform.isStatic, enableDebug ? false
, doCheck ? true, enableParallelBuilding ? true, dontStrip ? true }:

let

  src = lib.cleanSource ../../../pkg/urbit;

  version = builtins.readFile "${src}/version";

in stdenv.mkDerivation {
  inherit src version;

  pname = "urbit" + lib.optionalString enableDebug "-debug"
    + lib.optionalString enableStatic "-static";

  nativeBuildInputs = [ pkgconfig ];

  buildInputs = [
    argon2u
    cacert
    ca-bundle
    curl
    ed25519
    ent
    ge-additions
    gmp
    h2o
    ivory.header
    libaes_siv
    libscrypt
    libsigsegv
    libuv
    lmdb
    murmur3
    openssl
    secp256k1
    softfloat3
    zlib
  ];

  checkInputs = [ herb ];

  # Ensure any `/usr/bin/env bash` shebang is patched.
  postPatch = ''
    patchShebangs ./configure
  '';

  checkTarget = "test";

  installPhase = ''
    mkdir -p $out/bin
    cp ./build/urbit $out/bin/urbit
    cp ./build/urbit-worker $out/bin/urbit-worker
  '';

  CFLAGS = [ (if enableDebug then "-O0" else "-O3") "-g" ]
    ++ lib.optionals (!enableDebug) [ "-Werror" ]
    ++ lib.optionals enableStatic [ "-static" ];

  MEMORY_DEBUG = enableDebug;
  CPU_DEBUG = enableDebug;
  EVENT_TIME_DEBUG = false;

  # See https://github.com/NixOS/nixpkgs/issues/18995
  hardeningDisable = lib.optionals enableDebug [ "all" ];

  inherit enableParallelBuilding doCheck dontStrip;

  meta = {
    debug = enableDebug;
    arguments = lib.optionals enableDebug [ "-g" ];
  };
}
