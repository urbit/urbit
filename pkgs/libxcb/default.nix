{ crossenv, xcb-proto, xorg-macros }:

crossenv.make_derivation rec {
  name = "libxcb-${version}";
  version = "1.12";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://xcb.freedesktop.org/dist/libxcb-${version}.tar.bz2";
    sha256 = "0nvv0la91cf8p5qqlb3r5xnmg1jn2wphn4fb5jfbr6byqsvv3psa";
  };

  patches = [ ./no-pthread-stubs.patch ];

  builder = ./builder.sh;

  configure_flags =
    "--host=${crossenv.host} " +
    "--enable-static " +
    "--disable-shared " +
    "--enable-xinput " +
    "--enable-xkb";

  cross_inputs = [ xcb-proto xorg-macros ];

  native_inputs = [ crossenv.nixpkgs.python2 ];
}
