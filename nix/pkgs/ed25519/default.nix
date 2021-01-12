{ stdenv, sources }:

stdenv.mkDerivation {
  pname = "ed25519";
  version = sources.ed25519.rev;
  src = sources.ed25519;

  buildPhase = ''
    CFLAGS="-O3 -Wall -I$src/src"

    for f in $(find src -type f -name '*.c'); do
      $CC $CFLAGS -c $f -o "''${f//\//_}.o"
    done
  '';

  installPhase = ''
    mkdir -p $out/{lib,include}
    $AR rcs $out/lib/libed25519.a *.o
    cp $src/src/*.h $out/include/
  '';
}
