with (import <nixpkgs> {});

let

  osxdeps = lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
    Cocoa
    CoreServices
  ]);

  deps = [ cmark curl gcc gmp libsigsegv meson ncurses ninja pkgconfig zlib re2c openssl ];

in stdenv.mkDerivation {
  name = "urbit";
  buildInputs = osxdeps ++ deps;
  NIX_LDFLAGS = lib.optionalString stdenv.isDarwin "-framework CoreServices";
}
