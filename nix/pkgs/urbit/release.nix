{ env_name, env, deps }:

{ ent, ge-additions, name ? "urbit", debug ? false }:

let

  crossdeps =
    with env;
    [ curl libgmp libsigsegv ncurses openssl zlib lmdb ];

  vendor =
    with deps;
    [ argon2 softfloat3 ed25519 ge-additions h2o scrypt uv murmur3 secp256k1 sni ];

in

env.make_derivation {
  CFLAGS           = if debug then "-O0 -g" else "-O3";
  LDFLAGS          = if debug then "" else "-s";
  MEMORY_DEBUG     = debug;
  CPU_DEBUG        = debug;
  EVENT_TIME_DEBUG = false;
  NCURSES          = env.ncurses;

  name          = "${name}-${env_name}";
  exename       = name;
  src           = ../../../pkg/urbit;
  cross_inputs  = crossdeps ++ vendor ++ [ ent ];
  builder       = ./release.sh;
}
