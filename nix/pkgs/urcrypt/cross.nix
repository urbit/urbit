{ env_name, env, deps }:

env.make_derivation rec {
  name    = "urcrypt";
  builder = ./release.sh;
  src     = ../../../pkg/urcrypt;

  cross_inputs = [ deps.ed25519 deps.ge-additions ];

  CC = "${env.host}-gcc";
  AR = "${env.host}-ar";
}
