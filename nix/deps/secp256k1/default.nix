{ pkgs }:

pkgs.stdenv.mkDerivation {
  name = "secp256k1";
  src  = pkgs.sources.secp256k1;

  depsBuildBuild = [
    pkgs.buildPackages.stdenv.cc
    pkgs.autoconf
    pkgs.automake
    pkgs.libtool
    pkgs.m4
  ];

  depsBuildHost = [
    pkgs.gmp
  ];
 
  preConfigure = ''
    ./autogen.sh
  '';
   
  configureFlags = ["--disable-shared" "--enable-module-recovery" ];
  CFLAGS         = "-fPIC";
}
