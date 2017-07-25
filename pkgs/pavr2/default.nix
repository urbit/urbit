{ crossenv, qt, libusbp }:

crossenv.make_derivation rec {
  name = "pavr2-${version}";

  version = "4d40ae6";  # 1.0.2ish

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/pololu-usb-avr-programmer-v2/archive/${version}.tar.gz";
    sha256 = "0819klcqrss8zdqb26w8bpwzdfrk3g2nn7li2a7k87lqks3pl6lj";
  };

  builder = ./builder.sh;

  cross_inputs = [ libusbp qt ];
}
