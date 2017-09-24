{ crossenv, libusbp }:

crossenv.make_derivation rec {
  name = "p-load-${version}";

  version = "d063267";  # 1.0.2ish

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/p-load/archive/${version}.tar.gz";
    sha256 = "0yy6wzxcbjnn39dsvdrza893jjlvs45w7nclwpr17y6bjdn25i0g";
  };

  builder = ./builder.sh;

  cross_inputs = [ libusbp ];
}
