# TODO: remove giveio.sys and any other sketchy drivers or binaries from the source

# Note: There are no patches to help AVRDUDE find its configuration
# file, so it will expect that file to be at
# /nix/store/...-avrdude/etc/avrdude.conf

{ crossenv }:

crossenv.make_derivation rec {
  name = "avrdude-${version}";

  version = "6.3";  # February 2016

  src = crossenv.nixpkgs.fetchurl {
    url = "http://download.savannah.gnu.org/releases/avrdude/avrdude-${version}.tar.gz";
    sha256 = "15m1w1qad3dj7r8n5ng1qqcaiyx1gyd6hnc3p2apgjllccdp77qg";
  };

  patches = [ ./faster.patch ];

  native_inputs = [
    crossenv.nixpkgs.yacc
    crossenv.nixpkgs.flex
  ];

  cross_inputs = [
    # TODO: libusb
    # TODO: libftdi
    # TODO: libelf
    # TODO: libhid
  ];

  config_dot_sub = ./config.sub;
  extra_conf = ./extra.conf;

  builder = ./builder.sh;
}
