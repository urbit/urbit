{ crossenv, pdcurses }:

crossenv.make_derivation rec {
  name = "pdcurses_demos-${version}";

  inherit pdcurses;
  inherit (pdcurses) src version;

  builder = ./demos_builder.sh;
}

