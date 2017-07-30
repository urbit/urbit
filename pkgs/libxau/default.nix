{ crossenv, xorg-macros, xproto }:

crossenv.make_derivation rec {
  name = "libxau-${version}";
  version = "1.0.8";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://www.x.org/archive/individual/lib/libXau-${version}.tar.bz2";
    sha256 = "1wm4pv12f36cwzhldpp7vy3lhm3xdcnp4f184xkxsp7b18r7gm7x";
  };

  builder = ./builder.sh;

  configure_flags =
    "--host=${crossenv.host} " +
    "--enable-static " +
    "--disable-shared";

  cross_inputs = [ xorg-macros xproto ];

  inherit xproto;
}
