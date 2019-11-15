{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "hello";
  version = "0.1.0.0";
  src = ../../../pkg/hello-cabal-nix;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/githubuser/hello#readme";
  license = stdenv.lib.licenses.bsd3;
}
