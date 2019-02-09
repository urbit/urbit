{ crossenv, qt, libusbp }:

crossenv.make_derivation rec {
  name = "tic-${version}";

  version = "1.7.0";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/pololu-tic-software/archive/${version}.tar.gz";
    sha256 = "1k091i6sz5j204xrq12x97ixj56bjwbb593lp7j3qf94msj7sff9";
  };

  builder = ./builder.sh;

  cross_inputs = [ libusbp qt ];
}
