{ crossenv, libusbp }:

crossenv.make_derivation rec {
  name = "p-load-${version}";

  version = "2.0.1ish";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/p-load/archive/31126da.tar.gz";
    sha256 = "1rha46s1jq6060c9mkki3rra9s4hsg2vz4cakcq91d98zb6i63ln";
  };

  builder = ./builder.sh;

  I686_W64_MINGW32_PKG_CONFIG_PATH = "${libusbp}/lib/pkgconfig";  # TODO
  X86_64_W64_MINGW32_PKG_CONFIG_PATH = "${libusbp}/lib/pkgconfig";  # TODO
}
