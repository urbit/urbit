{ crossenv }:

crossenv.make_derivation rec {
  name = "usbview-${version}";

  version = "2017-03-21";

  src = crossenv.nixpkgs.fetchFromGitHub {
    owner = "Microsoft";
    repo = "Windows-driver-samples";
    rev = "ba5bfb111cf7e5689cdff405ace332781c2b5b76";
    sha256 = "16rapc27bnwgfss5z1d30smbckqqbnlqzkvd2i2syf00sf7gx6n4";
  };

  patches = [ ./megapatch.patch ];

  my_xmlhelper_c = ./my_xmlhelper.c;

  builder = ./builder.sh;
}
