{ crossenv }:

crossenv.make_derivation rec {
  name = "ncurses-${version}";
  version = "6.1-20181027";
  builder = ./builder.sh;

  # Needs to be the same version.
  native_inputs = [ crossenv.nixpkgs.ncurses ];

  configureFlags = [
    "--without-debug"
    "--enable-pc-files"
    "--enable-symlinks"
    # "--with-manpage-format=normal"
    "--without-cxx"
    # "--enable-widec"
  ];

  src = crossenv.nixpkgs.fetchurl {
    urls = [
      "https://invisible-mirror.net/archives/ncurses/current/ncurses-${version}.tgz"
      "ftp://ftp.invisible-island.net/ncurses/current/ncurses-${version}.tgz"
    ];
    sha256 = "1xn6wpi22jc61158w4ifq6s1fvilhmsy1in2srn3plk8pm0d4902";
  };
}
