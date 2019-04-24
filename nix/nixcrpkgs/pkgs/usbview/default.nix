{ crossenv }:

if crossenv.os != "windows" then "windows only" else

crossenv.make_derivation rec {
  name = "usbview-${version}";

  version = "2017-05-01";

  src = crossenv.nixpkgs.fetchFromGitHub {
    owner = "Microsoft";
    repo = "Windows-driver-samples";
    rev = "4c5c5e0297c7a61e151f92af702cdac650a14489";
    sha256 = "1drq26bnad98xqn805qx0b6g4y65lmrdj7v40b3jhhzdsp8993pf";
  };

  patches = [ ./megapatch.patch ];

  my_xmlhelper_c = ./my_xmlhelper.c;

  builder = ./builder.sh;
}
