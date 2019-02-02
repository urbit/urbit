{ crossenv }:

let
  version = "2.37";

  name = "dejavu-fonts-${version}";

  src = crossenv.nixpkgs.fetchurl {
    # The original URL was:
    # http://sourceforge.net/projects/dejavu/files/dejavu/${version}/dejavu-fonts-ttf-${version}.tar.bz2";
    url = "https://files.tmphax.com/repo1/dejavu-fonts-ttf-${version}.tar.bz2";
    sha256 = "1mqpds24wfs5cmfhj57fsfs07mji2z8812i5c4pi5pbi738s977s";
  };

  fonts = crossenv.native.make_derivation {
    inherit version name src;
    builder = ./builder.sh;
  };

  license = crossenv.native.make_derivation {
    name = "${name}-license";
    inherit src;
    builder = ./license_builder.sh;
  };

  license_set = { "${name}" = license; };

in
  fonts // { inherit license_set; }
