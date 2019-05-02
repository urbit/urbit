let

  pkgs =
    builtins.fetchGit {
      name = "nixpkgs-2018-11-13";
      url = https://github.com/nixos/nixpkgs/;
      rev = "695a3d4254545968fc3015142c5299c0da5ca0a9";
    };

in

  with (import pkgs {});

let

  osxdeps = lib.optionals stdenv.isDarwin (
    with darwin.apple_sdk.frameworks;
      [ Cocoa CoreServices ]);

  deps = [ cmark curl gcc git gmp libsigsegv meson ncurses ninja pkgconfig zlib
           re2c openssl ];

  isGitDir = (path: type: type != "directory" || baseNameOf path != ".git");

in

  stdenv.mkDerivation {
    name = "urbit";

    src = builtins.filterSource isGitDir ./.;

    buildInputs = osxdeps ++ deps;

    mesonFlags = "-Dnix=true";

    NIX_LDFLAGS = lib.optionalString stdenv.isDarwin "-framework CoreServices";
  }
