{ crossenv, xproto, xextproto, inputproto, libx11, libxext, libxfixes }:

crossenv.make_derivation rec {
  name = "libxi-${version}";
  version = "1.7.9";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://www.x.org/releases/individual/lib/libXi-${version}.tar.bz2";
    sha256 = "0idg1wc01hndvaa820fvfs7phvd1ymf0lldmq6386i7rhkzvirn2";
  };

  builder = ./builder.sh;

  configure_flags =
    "--host=${crossenv.host} " +
    "--disable-malloc0returnsnull " +
    "--enable-static " +
    "--disable-shared";

  cross_inputs = [ xproto xextproto inputproto libx11 libxext libxfixes ];

  inherit inputproto libx11 libxext libxfixes;
}
