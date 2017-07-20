{ crossenv }:

crossenv.make_derivation rec {
  name = "libudev-${version}";
  version = "234";
  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/systemd/systemd/archive/v${version}.tar.gz";
    sha256 = "0shbv3hrmryfr22v07s2mh8v8dwhjba2ldrk739q7jd11b8njgns";
  };
  builder = ./builder.sh;
  patches = [
    # Fix some compile-time errors caused by not using glibc.
    ./megapatch.patch
  ];
  fill = ./fill;
}
