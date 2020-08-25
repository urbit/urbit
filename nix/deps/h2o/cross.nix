{ crossenv, sources, uv }:

crossenv.make_derivation {
  inherit (crossenv) openssl zlib;
  inherit uv;

  name         = "h2o";
  src          = sources.h2o;
  builder      = ./cross.sh;
  cross_inputs = [ uv crossenv.openssl crossenv.zlib ];
}
