{ crossenv }:

crossenv.make_derivation rec {
  name = "avrdude-${version}";

  version = "6.3";  # February 2016

  src = crossenv.nixpkgs.fetchurl {
    url = "http://download.savannah.gnu.org/releases/avrdude/avrdude-${version}.tar.gz";
    sha256 = "15m1w1qad3dj7r8n5ng1qqcaiyx1gyd6hnc3p2apgjllccdp77qg";
  };

  native_inputs = [
    #crossenv.nixpkgs.autoconf
    #crossenv.nixpkgs.automake
    #crossenv.nixpkgs.m4
    #crossenv.nixpkgs.libtool
    crossenv.nixpkgs.yacc
    crossenv.nixpkgs.flex
    #crossenv.nixpkgs.bison
  ];

  cross_inputs = [
    # TODO: libusb
    # TODO: libftdi
    # TODO: libelf
    # TODO: libhid
  ];

  config_dot_sub = ./config.sub;

  builder = ./builder.sh;
}
