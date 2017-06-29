{ crossenv }:

crossenv.make_derivation rec {
  name = "libusbp-${version}-${crossenv.host}";

  version = "1.0.2";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/libusbp/archive/${version}.tar.gz";
    sha256 = "04r2b5v226j4mvc60m0hsl2w20x4c5h0qh0619nn21kkkv15sirb";
  };

  patches = [
  ];

  builder = ./builder.sh;
}
