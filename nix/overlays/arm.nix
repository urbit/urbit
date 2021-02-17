final: prev:

let

  isAarch64 = prev.stdenv.hostPlatform.isAarch64;

in prev.lib.optionalAttrs isAarch64 {
  libsigsegv = prev.libsigsegv.overrideAttrs (attrs: {
    preConfigure = (old.preConfigure or "") + ''
      sed -i 's/^CFG_FAULT=$/CFG_FAULT=fault-linux-arm.h/' configure
    '';
  });
}
