{
  pkgs,
  debug,
  argon2, ed25519, ent, ge-additions, h2o, murmur3, scrypt, secp256k1, sni, softfloat3, uv, ivory-header, ca-header
}:

let

  name =
    if debug then "urbit-debug" else "urbit";

  meta = rec {
    inherit debug;
    bin   = "${urbit}/bin/${name}";
    flags = if debug then [ "-g" ] else [];
    exe   = ''${meta.bin} ${pkgs.lib.strings.concatStringsSep " " meta.flags}'';
  };

  deps =
    with pkgs;
    [ curl gmp libsigsegv ncurses openssl zlib lmdb ];

  vendor =
    [ argon2 softfloat3 ed25519 ent ge-additions h2o scrypt uv murmur3 secp256k1 sni ivory-header ca-header ];

  urbit = pkgs.stdenv.mkDerivation {
    inherit name meta;
    exename = name;
    src     = ../../../pkg/urbit;
    nativeBuildInputs = deps ++ vendor;

    configurePhase = ''
      bash ./configure
    '';

    installPhase = ''
      make all -j8
      make test

      mkdir -p $out/bin
      cp ./build/urbit $out/bin/$exename
      cp ./build/urbit-worker $out/bin/$exename-worker
    '';

    # See https://github.com/NixOS/nixpkgs/issues/18995
    hardeningDisable = if debug then [ "all" ] else [];

    CFLAGS           = "-O3 -g -Werror";
    MEMORY_DEBUG     = debug;
    CPU_DEBUG        = debug;
    EVENT_TIME_DEBUG = false;
  };

in

urbit
