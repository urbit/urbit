{ crossenv, libusb }:

let
  version = "2018-09-19";

  name = "openocd-${version}";

  nixpkgs = crossenv.nixpkgs;

  src = nixpkgs.fetchgit {
    url = "git://repo.or.cz/openocd";  # official mirror
    rev = "b2d259f67cc3ee4b689e704228d97943bae94064";
    sha256 = "126zcgq3dplhzmy28rqp0y0df92xgb2qkh9nfc70mka7jwj994yx";
    fetchSubmodules = true;
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
      "${nixpkgs.pkgconfig}/share/aclocal";

    # Avoid a name conflict: get_home_dir is also defined in libudev.
    CFLAGS = "-Dget_home_dir=openocd_get_home_dir";

    patches = [
      # Make sure the linker arguments needed by our static build of libusb
      # get used when compiling the openocd executable.
      ./ldflags.patch
    ];

    cross_inputs = [ libusb ];
  };

in
  drv
