{ crossenv, qt, libusbp }:

crossenv.make_derivation rec {
  name = "tic-${version}";

  version = "bf89ebd";  # 1.5.0

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/pololu-tic-software/archive/${version}.tar.gz";
    sha256 = "179nhsnv5xx31c4khafhdb3imld3lv2kmfb8156zl85i2y46kfzc";
  };

  builder = ./builder.sh;

  cross_inputs = [ libusbp qt ];
}
