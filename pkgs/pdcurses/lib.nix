{ crossenv }:

crossenv.make_derivation rec {
  name = "pdcurses-${version}";

  version = "3.4";

  src = crossenv.nixpkgs.fetchurl {
    # TODO: fix broken sourceforge download
    url = "mirror://sourceforge/pdcurses/PDCurses-${version}.tar.gz";
    sha256 = "0jz6l8552fnf1j542yhzifgknrdzrisxg158ks0l87g777a8zba6";
  };

  builder = ./builder.sh;
}
