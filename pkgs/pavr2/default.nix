{ crossenv, qt, libusbp }:

crossenv.make_derivation rec {
  name = "pavr2-${version}";

  version = "a113a3b";  # 1.0.2ish

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/pololu-usb-avr-programmer-v2/archive/${version}.tar.gz";
    sha256 = "1mg467jx7mpcn01vh8rq80w7p8mbj7l69dmpyni0nik44ggsj7ij";
  };

  builder = ./builder.sh;

  cross_inputs = [ libusbp qt ];
}
