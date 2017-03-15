{ crossenv }:

let
  version = "5.8.0";

  src = crossenv.nixpkgs.fetchurl {
    url = https://download.qt.io/official_releases/qt/5.8/5.8.0/single/qt-everywhere-opensource-src-5.8.0.tar.xz;
    sha256 = "1di492c455m43q0bqa9draq98yxvrr2p2slkd9hc1fixdlw58k0g";
  };

  # TODO: add the patch from martchus for UIViewSettingsInterop.h

  base = crossenv.nixpkgs.stdenv.mkDerivation rec {
    name = "qt-${version}-${crossenv.host}";

    inherit version;

    inherit src;

    inherit (crossenv) host;

    buildInputs = [ crossenv.gcc crossenv.binutils ];

    builder = ./builder.sh;
  };
in
{
  recurseForDerivations = true;
  inherit src;
  inherit base;
}
