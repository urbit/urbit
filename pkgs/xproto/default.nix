{ crossenv, xorg-macros }:

crossenv.make_derivation rec {
  name = "xproto-${version}";
  version = "7.0.31";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://www.x.org/releases/individual/proto/xproto-${version}.tar.gz";
    sha256 = "1is3xl0zjk4l0d8d0zinkfbfapgdby2i56jjfp6caibvwam5wxbd";
  };

  builder = ./builder.sh;

  configure_flags =
    "--host=${crossenv.host} " +
    "--enable-static " +
    "--disable-shared";

  cross_inputs = [ xorg-macros ];

  # Need the latest version of config.sub so we can support musl.
  gnu_config = crossenv.native.gnu_config;
}
