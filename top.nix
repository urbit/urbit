{ nixpkgs }:

rec {
  inherit nixpkgs;

  # Some native build tools.
  native = import ./native { inherit nixpkgs; };

  # Cross-compiling environments for each target system.
  crossenvs = {
    i686-w64-mingw32 = import ./mingw-w64 { inherit native; arch = "i686"; };
    x86_64-w64-mingw32 = import ./mingw-w64 { inherit native; arch = "x86_64"; };
    i686-linux-musl = import ./linux { inherit native; arch = "i686"; };
    x86_64-linux-musl = import ./linux { inherit native; arch = "x86_64"; };
    armv6-linux-musl = import ./linux {
      inherit native;
      arch = "armv6";
      gcc_options = "--with-fpu=vfp --with-float=hard ";
    };
    macos = import ./macos { inherit native; };
  };

  pkgFun = crossenv: import ./pkgs.nix { inherit crossenv; } // crossenv;

  # Sets of packages for each target system.
  i686-w64-mingw32 = pkgFun crossenvs.i686-w64-mingw32;
  x86_64-w64-mingw32 = pkgFun crossenvs.x86_64-w64-mingw32;
  i686-linux-musl = pkgFun crossenvs.i686-linux-musl;
  x86_64-linux-musl = pkgFun crossenvs.x86_64-linux-musl;
  armv6-linux-musl = pkgFun crossenvs.armv6-linux-musl;
  macos = pkgFun crossenvs.macos;

  # Handy aliases.
  win32 = i686-w64-mingw32;
  win64 = x86_64-w64-mingw32;
  linux32 = i686-linux-musl;
  linux-x86 = i686-linux-musl;
  linux-i686 = i686-linux-musl;
  linux64 = x86_64-linux-musl;
  linux-x86_64 = x86_64-linux-musl;
  linux-rpi = armv6-linux-musl;
  rpi = armv6-linux-musl;
  mac = macos;

  # filter is a function that can be applied to a local directory to filter out
  # files that are likely to change frequently without affecting the build,
  # causing unneeded rebuilds.
  filter_func = name: type: let bn = baseNameOf (toString name); in !(
    (type == "directory" && bn == ".git") ||
    (type == "symlink" && nixpkgs.lib.hasPrefix "result" bn) ||
    (type == "directory" && bn == "nix") ||
    (type == "directory" && bn == "build") ||
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
