{ crossenv, xproto, xextproto, libx11, fixesproto }:

crossenv.make_derivation rec {
  name = "libxfixes-${version}";
  version = "5.0.3";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://www.x.org/releases/individual/lib/libXfixes-${version}.tar.bz2";
    sha256 = "1miana3y4hwdqdparsccmygqr3ic3hs5jrqfzp70hvi2zwxd676y";
  };

  builder = ./builder.sh;

  configure_flags =
    "--host=${crossenv.host} " +
    "--enable-static " +
    "--disable-shared";

  cross_inputs = [ xproto xextproto libx11 fixesproto ];

  inherit xproto libx11 fixesproto;
}
