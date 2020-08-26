{ crossenv, sources }:

crossenv.make_derivation {
  name    = "secp256k1";
  src     = sources.secp256k1;
  builder = ./release.sh;

  cross_inputs  = [ crossenv.libgmp ];
  native_inputs =
    with crossenv.nixpkgs;
    [ autoconf automake libtool m4 ];
  
  configureFlags = ["--disable-shared" "--enable-module-recovery" ];
  CFLAGS         = "-fPIC";
}
