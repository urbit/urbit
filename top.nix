{ nixpkgs }:

rec {
  # Some native build tools.
  native = {
    recurseForDerivations = true;
    make_derivation = import ./make_derivation.nix nixpkgs null;
    pkgconf = import ./pkgconf { inherit nixpkgs; };
  };

  # Cross-compiling environments for each target system.
  crossenvs = {
    i686-w64-mingw32 = import ./mingw-w64 { inherit nixpkgs; arch = "i686"; };
    x86_64-w64-mingw32 = import ./mingw-w64 { inherit nixpkgs; arch = "x86_64"; };
  };

  pkgFun = crossenv: import ./pkgs.nix { inherit crossenv; } // crossenv;

  # Sets of packages for each target system.
  i686-w64-mingw32 = pkgFun crossenvs.i686-w64-mingw32;
  x86_64-w64-mingw32 = pkgFun crossenvs.x86_64-w64-mingw32;

  # Handy aliases.
  win32 = i686-w64-mingw32;
  win64 = x86_64-w64-mingw32;

  # filter is a function that can be applied to a local directory to filter out
  # files that are likely to change frequently without affecting the build,
  # causing unneeded rebuilds.
  filter_func = name: type: let bn = baseNameOf (toString name); in !(
    (type == "directory" && bn == ".git") ||
    (type == "symlink" && nixpkgs.lib.hasPrefix "result" bn) ||
    (type == "directory" && bn == "nix") ||
    nixpkgs.lib.hasSuffix ".nix" bn ||
    nixpkgs.lib.hasSuffix "~" bn
  );
  filter = builtins.filterSource filter_func;

  # bundle is a function that can take a set of derivations
  bundle = drvs: native.make_derivation rec {
    name = "bundle";
    builder = ./bundle_builder.sh;
    names = builtins.attrNames drvs;
    dirs = builtins.attrValues drvs;
  };
}
