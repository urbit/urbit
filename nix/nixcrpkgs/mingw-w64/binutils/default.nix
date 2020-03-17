{ native, host }:

native.make_derivation rec {
  name = "binutils-${version}-${host}";

  version = "2.31";

  src = native.nixpkgs.fetchurl {
    url = "mirror://gnu/binutils/binutils-${version}.tar.xz";
    sha256 = "1gqn887nqfyd5l6gfv08m1pg4wg5fm2iys7hpz6lj87hgvgkc413";
  };

  patches = [
    ./deterministic.patch
  ];

  configure_flags =
    "--target=${host} " +
    "--enable-shared " +
    "--enable-deterministic-archives " +
    "--disable-werror ";

  builder = ./builder.sh;
}
