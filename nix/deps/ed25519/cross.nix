{ crossenv, sources }:

crossenv.make_derivation {
  name    = "ed25519";
  src     = sources.ed25519;
  builder = ./builder.sh;

  CC = "${crossenv.host}-gcc";
  AR = "${crossenv.host}-ar";
}
