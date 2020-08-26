{ env_name, env, ... }:

env.make_derivation {
  name    = "libaes_siv";
  src     = ../../../pkg/libaes_siv;
  builder = ./release.sh;

  cross_inputs = [ env.openssl ];

  CC = "${env.host}-gcc";
  AR = "${env.host}-ar";
}
