{ crossenv }:

crossenv.native.make_derivation rec {
  name = "dejavu-fonts-${version}";
  version = "2.37";

  src = crossenv.nixpkgs.fetchurl {
    url = "http://sourceforge.net/projects/dejavu/files/dejavu/${version}/dejavu-fonts-ttf-${version}.tar.bz2";
    sha256 = "1mqpds24wfs5cmfhj57fsfs07mji2z8812i5c4pi5pbi738s977s";
  };

  builder = ./builder.sh;
}
