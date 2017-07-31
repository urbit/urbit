{ crossenv }:

crossenv.native.make_derivation rec {
  name = "kbproto-${version}";
  version = "1.0.7";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://xorg.freedesktop.org/releases/individual/proto/kbproto-${version}.tar.bz2";
    sha256 = "0mxqj1pzhjpz9495vrjnpi10kv2n1s4vs7di0sh3yvipfq5j30pq";
  };

  builder = ./builder.sh;
}
