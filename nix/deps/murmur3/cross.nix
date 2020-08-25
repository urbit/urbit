{ crossenv, sources }:

crossenv.make_derivation {
  name    = "murmur3";
  src     = sources.murmur3;
  builder = ./builder.sh;

  CC = "${crossenv.host}-gcc";
  AR = "${crossenv.host}-ar";
}
