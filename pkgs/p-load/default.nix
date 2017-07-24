{ crossenv, libusbp }:

crossenv.make_derivation rec {
  name = "p-load-${version}";

  version = "cead22c";  # 1.0.2ish

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/p-load/archive/${version}.tar.gz";
    sha256 = "074wdrfxxpciiqzxfpyj90vqrb63hpxa58jg0dv1lc1lgs85x5gg";
  };

  builder = ./builder.sh;

  cross_inputs = [ libusbp ];
}
