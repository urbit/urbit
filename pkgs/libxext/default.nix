{ crossenv, xproto, libx11, xextproto }:

crossenv.make_derivation rec {
  name = "libxext-${version}";
  version = "1.3.3";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://www.x.org/releases/individual/lib/libXext-${version}.tar.bz2";
    sha256 = "0dbfn5bznnrhqzvkrcmw4c44yvvpwdcsrvzxf4rk27r36b9x865m";
  };

  builder = ./builder.sh;

  configure_flags =
    "--host=${crossenv.host} " +
    "--disable-malloc0returnsnull " +
    "--enable-static " +
    "--disable-shared";

  cross_inputs = [ xproto libx11 xextproto ];

  inherit xextproto libx11;
}
