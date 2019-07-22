{ env_name, env, deps }:

env.make_derivation rec {
  name    = "ge-additions";
  builder = ./release.sh;
  src     = ../../../pkg/ge-additions;

  cross_inputs = [ deps.ed25519 ];

  CC = "${env.host}-gcc";
  AR = "${env.host}-ar";
}
