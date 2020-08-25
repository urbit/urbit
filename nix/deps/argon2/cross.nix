{ crossenv, sources }:

crossenv.make_derivation {
  name    = "argon2";
  src     = sources.argon2;
  builder = ./builder.sh;

  CC         = "${crossenv.host}-gcc";
  AR         = "${crossenv.host}-ar";
  NO_THREADS = true;
}
