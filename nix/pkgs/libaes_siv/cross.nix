{ env_name, env}:

env.make_derivation rec {
  name = "libaes_siv";
  src  = ../../../pkg/libaes_siv;

  buildInputs = [ env.openssl ];

  CC = "${env.host}-gcc";
  AR = "${env.host}-ar";
}
