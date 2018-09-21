{ crossenv, libusb }:

let
  version = "2018-08-16";

  name = "openocd-${version}";

  nixpkgs = crossenv.nixpkgs;

  src = nixpkgs.fetchgit {
    url = "git://repo.or.cz/openocd";  # official mirror
    rev = "b2d259f67cc3ee4b689e704228d97943bae94064";
    sha256 = "0c5zpjplwp0ivl4mpiix628j0iad9gkmg9f7lidgqjr5a80cr6hg";
    deepClone = true;
  };

  drv = crossenv.make_derivation {
    inherit version name src;
    builder = ./builder.sh;

    native_inputs = [
      nixpkgs.autoconf
      nixpkgs.automake
      nixpkgs.libtool
      nixpkgs.m4
    ];

    ACLOCAL_PATH =
      "${nixpkgs.libtool}/share/aclocal:" +
      "${crossenv.native.pkgconf}/share/aclocal";

    # Avoid a name conflict: get_home_dir is also defined in libudev.
    CFLAGS = "-Dget_home_dir=openocd_get_home_dir";

    cross_inputs = [ libusb ];
  };

in
  drv
