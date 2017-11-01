{ crossenv, qt, libusbp }:

crossenv.make_derivation rec {
  name = "pavr2-${version}";

  version = "2d11125";  # 1.0.2ish

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/pololu-usb-avr-programmer-v2/archive/${version}.tar.gz";
    sha256 = "1k8mbl60cvrx6b7i6kph3w8lnf6mspn1rh5j73ndp7x0dw0m92vy";
  };

  builder = ./builder.sh;

  cross_inputs = [ libusbp qt ];
}
