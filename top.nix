{ nixpkgs }:

rec {
  pkgconf = import ./pkgconf { inherit nixpkgs; };

  crossenvs = {
    recurseForDerivations = true;
    i686-w64-mingw32 = import ./mingw-w64 { inherit nixpkgs; arch = "i686"; };
    x86_64-w64-mingw32 = import ./mingw-w64 { inherit nixpkgs; arch = "x86_64"; };
  };

  pkgFun = crossenv: import ./pkgs.nix { inherit crossenv; } // crossenv;

  pkgs = {
    recurseForDerivations = true;
    i686-w64-mingw32 = pkgFun crossenvs.i686-w64-mingw32;
    x86_64-w64-mingw32 = pkgFun crossenvs.x86_64-w64-mingw32;
  };
}
