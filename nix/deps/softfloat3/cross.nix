{ crossenv, sources }:

crossenv.make_derivation {
  name    = "softfloat3";
  src     = sources.softfloat3;
  builder = ./builder.sh;

  CC = "${crossenv.host}-gcc";
  AR = "${crossenv.host}-ar";
}
