{ env_name, env, deps }:

env.make_derivation rec {
  name    = "libaes_siv";
  builder = ./release.sh;
  src     = ../../../pkg/libaes_siv;

  cross_inputs = [ env.openssl ];

  CC = "${env.host}-gcc";
  AR = "${env.host}-ar";
}
