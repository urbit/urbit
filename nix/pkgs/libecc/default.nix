{ stdenv, sources, bc, rsync, enableParallelBuilding ? true }:

stdenv.mkDerivation {
  pname = "libecc";
  version = sources.libecc.rev;
  src = sources.libecc;

  preBuild = ''
    buildFlagsArray+=(WNOERROR=1);
  '';

  installPhase = ''
    mkdir -p $out/lib $out/include
    cp build/*.a $out/lib/
    rsync -rv --prune-empty-dirs --include '*/' --include '**/*.h' --exclude '**/*' src/ $out/include/
  '';

  nativeBuildInputs =
    [ bc rsync ];

  inherit enableParallelBuilding;
}
