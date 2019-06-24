{ env_name, env, deps }:

env.make_derivation rec {
  name    = "ge-additions";
  builder = ./builder.sh;
  src     = ../../../pkg/ge-additions;

  nativeBuildInputs = [ deps.ed25519 ];

  CC = "${env.host}-gcc";
  AR = "${env.host}-ar";
}
