let

  # In order to get the hash of another tarball, run: `nix-prefetch-url --unpack <url>`
  pkgs =
    builtins.fetchTarball {
      name = "nixpkgs-2018-10-07";
      url = https://github.com/nixos/nixpkgs/archive/865cbd380d2568806c6e46c9a4bc746af7ec13c9.tar.gz;
      sha256 = "0r3gj1rci2ag6k8g5wcz1m4l6brvzgbfz92kpl74mrh7kjdy1m3p";
    };

in

  with (import pkgs {});

let

  osxdeps = lib.optionals stdenv.isDarwin (
    with darwin.apple_sdk.frameworks;
      [ Cocoa CoreServices ]);

  deps = [ cmark curl gcc gmp libsigsegv meson ncurses ninja pkgconfig zlib
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
