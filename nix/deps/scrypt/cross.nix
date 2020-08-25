{ crossenv, sources }:

crossenv.make_derivation {
  name    = "scrypt";
  src     = sources.libscrypt;
  builder = ./builder.sh;

  CC = "${crossenv.host}-gcc";
  AR = "${crossenv.host}-ar";
}
