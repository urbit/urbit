{ lib
, stdenv
, coreutils
, pkgconfig
, argon2u
, cacert
, ca-bundle
, curl
, ed25519
, ent
, ge-additions
, gmp
, h2o
, herb
, ivory
, libaes-siv
, libscrypt
, libsigsegv
, libssh2
, libuv
, lmdb
, murmur3
, nghttp2
, openssl
, secp256k1
, softfloat3
, zlib
, enableParallelBuilding ? true
, doCheck ? true
, withStatic ? stdenv.hostPlatform.isStatic
, withDebug ? false
}:

let

  src = lib.cleanSource ./.;

in stdenv.mkDerivation {
  inherit src;

  pname = "urbit"
    + lib.optionalString withDebug "-debug"
    + lib.optionalString withStatic "-static";

  version = builtins.readFile "${src}/version";

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
    libaes-siv
    libscrypt
    libsigsegv
    libssh2
    libuv
    lmdb
    murmur3
    nghttp2
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

  CFLAGS =
    [ (if withDebug then "-O0" else "-O3")
      "-g"
    ] ++ lib.optionals (!withDebug) [ "-Werror" ]
      ++ lib.optionals withStatic [ "-static" ];

  MEMORY_DEBUG = withDebug;
  CPU_DEBUG = withDebug;
  EVENT_TIME_DEBUG = false;

  # See https://github.com/NixOS/nixpkgs/issues/18995
  hardeningDisable = lib.optionals withDebug [ "all" ];

  inherit enableParallelBuilding doCheck;

  meta = {
    debug = withDebug;
  };
}
