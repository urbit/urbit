{ env_name, env, deps }:

env.make_derivation rec {
  name    = "ge-additions";
  src     = ../../../pkg/ge-additions;
  builder = ./builder.sh;

  buildInputs = [ deps.ed25519 ];

  CC = "${env.host}-gcc";
  AR = "${env.host}-ar";
}
