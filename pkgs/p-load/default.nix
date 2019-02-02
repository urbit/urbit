{ crossenv, libusbp }:

crossenv.make_derivation rec {
  name = "p-load-${version}";

  version = "2.4.0";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/p-load/archive/${version}.tar.gz";
    sha256 = "0z4acrm6spyz7dqzqdcba0shi6x01wpdnx8cin2pf29n28jwr2xc";
  };

  builder = ./builder.sh;

  cross_inputs = [ libusbp ];
}
