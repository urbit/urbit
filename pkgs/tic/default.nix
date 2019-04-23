{ crossenv, qt, libusbp }:

crossenv.make_derivation rec {
  name = "tic-${version}";

  version = "e1693cd";  # 1.5.0ish

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/pololu-tic-software/archive/${version}.tar.gz";
    sha256 = "07m75w0walr61yqki7h1ipzbfz7x417g7qnx0p1l6qdz89fyc7i8";
  };

  builder = ./builder.sh;

  cross_inputs = [ libusbp qt ];
}
