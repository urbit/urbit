{ crossenv }:

crossenv.make_derivation rec {
  name = "devcon-${version}";

  version = "2017-03-21";

  src = crossenv.nixpkgs.fetchFromGitHub {
    owner = "Microsoft";
    repo = "Windows-driver-samples";
    rev = "d0ff69d56d030d96767ee51e1e3a74b6f6eeaf4a";
    sha256 = "0ii27rl544sah4p1j8qa2vhqvs6gpd24lk5vg9fpghzyc54z756h";
  };

  patches = [ ./megapatch.patch ];

  builder = ./builder.sh;
}
