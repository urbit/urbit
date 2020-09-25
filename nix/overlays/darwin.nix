final: prev:

let

  isDarwin = prev.stdenv.hostPlatform.isDarwin;

  optionalsNull = xs:
    prev.lib.optionals (xs != null) xs;

  overrideWhen = cond: pkg: args:
    if cond then pkg.override args else pkg;

  overrideAttrsWhen = cond: pkg: f:
    if cond then pkg.overrideAttrs f else pkg;

in {
  bootstrap_cmds = overrideWhen isDarwin prev.bootstrap_cmds {
    stdenv = prev.llvmPackages_X.stdenv;
  };

  gmp6 = overrideAttrsWhen isDarwin prev.gmp6 (old: {
    doCheck = false;
  });

  libiconv = overrideAttrsWhen isDarwin prev.libiconv (old: {
    nativeBuildInputs =
      (old.nativeBuildInputs or []) ++ [
        prev.buildPackages.stdenv.cc
      ];
  });
}
