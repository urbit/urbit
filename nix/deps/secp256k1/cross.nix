{ crossenv, sources }:

crossenv.make_derivation {
  name = "secp256k1";
  src  = sources.secp256k1;

  depsBuildBuild = with crossenv.nixpkgs; [
    autoconf
    automake
    libtool
    m4
  ];

  depsBuildHost = [
    crossenv.libgmp
  ];

  preConfigure = ''
    ./autogen.sh
  '';
   
  configureFlags = ["--disable-shared" "--enable-module-recovery" ];
  CFLAGS         = "-fPIC";
}
