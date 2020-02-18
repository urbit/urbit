{ crossenv, xorgproto, libx11, libxext, libxfixes }:

let
  version = "1.7.9";

  name = "libxi-${version}";

  src = crossenv.nixpkgs.fetchurl {
    url = "https://www.x.org/releases/individual/lib/libXi-${version}.tar.bz2";
    sha256 = "0idg1wc01hndvaa820fvfs7phvd1ymf0lldmq6386i7rhkzvirn2";
  };

  lib = crossenv.make_derivation rec {
    inherit version name src;

    builder = ./builder.sh;

    configure_flags =
      "--host=${crossenv.host} " +
      "--disable-malloc0returnsnull " +
      "--enable-static " +
      "--disable-shared";

    cross_inputs = [ xorgproto libx11 libxext libxfixes ];

    inherit xorgproto libx11 libxext libxfixes;
  };

  license = crossenv.native.make_derivation {
    name = "${name}-license";
    inherit src;
    builder = ./license_builder.sh;
  };

  license_set =
    xorgproto.license_set //
    libx11.license_set //
    libxext.license_set //
    libxfixes.license_set //
    { "${name}" = license; };

in
  lib // { inherit license_set; }
