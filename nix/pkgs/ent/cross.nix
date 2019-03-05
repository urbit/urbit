{ env_name, env, deps }:

env.make_derivation rec {
  name    = "ent-7506f";
  builder = ./builder.sh;
  src     = ../../../pkg/ent;

  CC = "${env.host}-gcc";
  AR = "${env.host}-ar";
}
