{ crossenv }:

crossenv.make_derivation rec {
  name = "pdcurses-${version}";

  version = "3.4";

  src = crossenv.nixpkgs.fetchurl {
    # Sourceforge went down.  The original URL was:
    # url = "mirror://sourceforge/pdcurses/PDCurses-${version}.tar.gz";
    url = "https://files.tmphax.com/repo1/pdcurses-${version}.tar.gz";
    sha256 = "0jz6l8552fnf1j542yhzifgknrdzrisxg158ks0l87g777a8zba6";
  };

  builder = ./builder.sh;
}
