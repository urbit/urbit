{ env }:

env.make_derivation rec {
  name = "pkgconf-${version}";

  version = "1.0.1";

  src = env.nixpkgs.fetchurl {
    url = "https://github.com/pkgconf/pkgconf/releases/download/pkgconf-${version}/pkgconf-${version}.tar.gz";
    sha256 = "1w9wb2z7zz6s4mifbllvhx0401bwsynhp02v312i6i9jn1m2zkj5";
  };

  builder = ./builder.sh;
}
