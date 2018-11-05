# We cannot use binutils 2.31 because then we get a segmentation fault in our
# hello world program, which comes from static_init_tls() in Musl 1.1.20.

{ native, host }:

native.make_derivation rec {
  name = "binutils-${version}-${host}";

  version = "2.30";

  src = native.nixpkgs.fetchurl {
    url = "mirror://gnu/binutils/binutils-${version}.tar.xz";
    sha256 = "1rhshw4m5m2pjz8g15hpiwhp52kn0pj0b5dxy0v7lwigmspbhikf";
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
