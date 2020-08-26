{ env_name, env, deps }:

env.make_derivation {
  name    = "ent";
  src     = ../../../pkg/ent;
  builder = ./builder.sh

  CC = "${env.host}-gcc";
  AR = "${env.host}-ar";
}
