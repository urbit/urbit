{ crossenv, xorg-macros, xproto, libxcb, xtrans,
  xextproto, inputproto, kbproto }:

# TODO: fix missing pkg-config packages:
# Package 'kbproto', required by 'virtual:world', not found

crossenv.make_derivation rec {
  name = "libx11-${version}";
  version = "1.6.5";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://xorg.freedesktop.org/releases/individual/libX11-${version}.tar.bz2";
    sha256 = "0pa3cfp6h9rl2vxmkph65250gfqyki0ccqyaan6bl9d25gdr0f2d";
  };

  builder = ./builder.sh;

  configure_flags =
    "--host=${crossenv.host} " +
    "--disable-malloc0returnsnull " +
    "--enable-static " +
    "--disable-shared";

  cross_inputs = [
    xorg-macros
    xproto
    libxcb
    xtrans
    xextproto
    inputproto
    kbproto
  ];

  inherit kbproto xproto libxcb;
}
