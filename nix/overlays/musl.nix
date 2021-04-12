final: prev:

let

  isMusl = prev.stdenv.hostPlatform.isMusl;

  optionalList = xs: if xs == null then [ ] else xs;

  overrideStdenv = pkg: pkg.override { stdenv = prev.gcc9Stdenv; };

in prev.lib.optionalAttrs isMusl {
  libsigsegv = prev.libsigsegv.overrideAttrs (attrs: {
    preConfigure = (attrs.preConfigure or "") + ''
      sed -i 's/^CFG_FAULT=$/CFG_FAULT=fault-linux-i386.h/' configure
    '';
  });

  secp256k1 = prev.secp256k1.overrideAttrs (attrs: {
    nativeBuildInputs = (attrs.nativeBuildInputs or [ ])
      ++ [ prev.buildPackages.stdenv.cc ];
  });

  rhash = overrideStdenv prev.rhash;

  numactl = overrideStdenv prev.numactl;

  lmdb = overrideStdenv prev.lmdb;
}
