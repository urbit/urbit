{ crossenv, qt, libusbp }:

crossenv.make_derivation rec {
  name = "tic-${version}";

  version = "ea6f03d";  # 1.4.0ish

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/pololu-tic-software/archive/${version}.tar.gz";
    sha256 = "1464mcymy2j87ghpl2jf6b1cl48k83kwa776s3qcz75h6axlhywk";
  };

  builder = ./builder.sh;

  cross_inputs = [ libusbp qt ];
}
