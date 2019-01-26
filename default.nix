let

  pkgs = import ./nix/nixpkgs.nix;

in

  with pkgs;

let

  osxdeps = lib.optionals stdenv.isDarwin (
    with darwin.apple_sdk.frameworks;
      [ Cocoa CoreServices ]);

  deps = [ cmark curl gcc gmp libsigsegv meson ncurses ninja pkgconfig zlib
           re2c openssl SDL2 ];

  isGitDir = (path: type: type != "directory" || baseNameOf path != ".git");

in

  stdenv.mkDerivation {
    name = "urbit";

    src = builtins.filterSource isGitDir ./.;

    buildInputs = osxdeps ++ deps;

    mesonFlags = "-Dnix=true";

    NIX_LDFLAGS = lib.optionalString stdenv.isDarwin "-framework CoreServices";
  }
