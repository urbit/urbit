{ nixpkgs, arch }:
nixpkgs.stdenv.mkDerivation rec {
  name = "binutils-${version}-${arch}-w64-mingw32";

  version = "2.27";

  src = nixpkgs.fetchurl {
    url = "mirror://gnu/binutils/binutils-${version}.tar.bz2";
    sha256 = "125clslv17xh1sab74343fg6v31msavpmaa1c1394zsqa773g5rn";
  };

  patches = [
    # Make binutils output deterministic by default.
    ./deterministic.patch
  ];

  outputs = [ "out" "info" ];

  buildInputs = [ nixpkgs.bison nixpkgs.zlib ];

  preConfigure = ''
    # Clear the default library search path (noSysDirs)
    echo 'NATIVE_LIB_DIRS=' >> ld/configure.tgt

    # Use symlinks instead of hard links to save space ("strip" in the
    # fixup phase strips each hard link separately).
    for i in binutils/Makefile.in gas/Makefile.in ld/Makefile.in gold/Makefile.in; do
        sed -i "$i" -e 's|ln |ln -s |'
    done
  '';

  configureFlags = [
    "--enable-shared"
    "--enable-deterministic-archives"
    "--disable-werror"
    "--target=${arch}-w64-mingw32"
  ];

  meta = with nixpkgs.stdenv.lib; {
    homepage = https://www.gnu.org/software/binutils/;
    license = licenses.gpl3Plus;

    # Give binutils a lower priority than gcc-wrapper to prevent a
    # collision due to the ld/as wrappers/symlinks in the latter.
    priority = "10";
  };
}
