final: prev:

let

  # isStatic = prev.stdenv.hostPlatform.isStatic;
  isMusl = prev.stdenv.hostPlatform.isMusl;

  optionalsNull = xs:
    prev.lib.optionals (xs != null) xs;

  overrideWhen = cond: pkg: args:
    if cond then pkg.override args else pkg;

  overrideAttrsWhen = cond: pkg: f:
    if cond then pkg.overrideAttrs f else pkg;

in {
  libsigsegv = overrideAttrsWhen isMusl prev.libsigsegv (old: {
    patches = optionalsNull old.patches ++ [
      ../../pkg/libsigsegv/sigcontext-redefined-fix.patch
      ../../pkg/libsigsegv/stack_top-undefined-fix.patch
    ];
  });

  secp256k1 = overrideAttrsWhen isMusl prev.secp256k1 (old: {
    nativeBuildInputs =
      optionalsNull old.nativeBuildInputs ++ [
        prev.buildPackages.stdenv.cc
      ];
  });

  # keyutils = overrideWhen isMusl prev.keyutils { stdenv = prev.gcc9Stdenv; };

  rhash = overrideWhen isMusl prev.rhash { stdenv = prev.gcc9Stdenv; };

  numactl = overrideWhen isMusl prev.numactl { stdenv = prev.gcc9Stdenv; };

  lmdb = overrideWhen isMusl prev.lmdb { stdenv = prev.gcc9Stdenv; };
}
