# Build "hello" for 32-bit Windows just using the nixpkgs cross-compiling support.
rec {
  nixpkgsFun = import <nixpkgs>;

  winpkgs = nixpkgsFun {
    crossSystem = {
      config = "i686-w64-mingw32";
      libc = "msvcrt";
    };
  };

  gcc = winpkgs.gccCrossStageFinal;

  hello = winpkgs.hello.crossDrv;
}
