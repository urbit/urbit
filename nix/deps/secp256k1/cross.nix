{ crossenv, sources }:

crossenv.make_derivation {
  name    = "secp256k1";
  src     = sources.secp256k1;
  builder = ./builder.sh;
  buildInputs = [crossenv.libgmp];
  nativeBuildInputs = with crossenv.nixpkgs; [
    autoconf
    automake
    libtool
    m4
  ];

  configureFlags = [
    "--disable-shared"
    "--enable-module-recovery"
  ];

  CFLAGS = "-fPIC";
}
