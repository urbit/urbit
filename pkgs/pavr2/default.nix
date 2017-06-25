{ crossenv, qt, libusbp }:

crossenv.make_derivation rec {
  name = "pavr2-${version}";

  version = "1.0.2ish";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/pololu-usb-avr-programmer-v2/archive/053fb01.tar.gz";
    sha256 = "0mh3qyvk4k5i82zcf4q0anvm8aq1b684v91ss2kqhz80fafm6gy3";
  };

  patches = [];

  builder = ./builder.sh;

  cross_inputs = [ libusbp ];

  # TODO: this pattern is bad because projects building native apps will find Qt
  CMAKE_PREFIX_PATH = "${qt}";
}
