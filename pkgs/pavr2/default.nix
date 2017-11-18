{ crossenv, qt, libusbp }:

crossenv.make_derivation rec {
  name = "pavr2-${version}";

  version = "b28eb0f";  # 1.0.2ish

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/pololu-usb-avr-programmer-v2/archive/${version}.tar.gz";
    sha256 = "0spry41fh3p3hjdd0ipl2y5prc5pqg5rz8rnw55c68wgd8hvnjiq";
  };

  builder = ./builder.sh;

  cross_inputs = [ libusbp qt ];
}
