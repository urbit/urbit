final: prev:

let

  # https://github.com/NixOS/nixpkgs/pull/97047/files
  # Will make pkgs.stdenv.isStatic available indepedent of the platform.
  # isStatic = prev.stdenv.hostPlatform.isStatic;

  configureFlags = attrs: {
    configureFlags = (attrs.configureFlags or [ ])
      ++ [ "--disable-shared" "--enable-static" ];
  };

  enableStatic = pkg: pkg.overrideAttrs configureFlags;

in {
  gmp = enableStatic prev.gmp;

  curlUrbit = enableStatic (prev.curlUrbit.override { openssl = final.openssl-static-osx; zlib = final.zlib-static-osx; });

  libuv = enableStatic prev.libuv;

  libffi = enableStatic prev.libffi;

  openssl-static-osx = prev.openssl.override {
    static   = true;
    withPerl = false;
  };

  zlib-static-osx = if final.stdenv.isDarwin then prev.zlib.static else prev.zlib;

  secp256k1 = enableStatic prev.secp256k1;

  lmdb = prev.lmdb.overrideAttrs (old:
    configureFlags old // {
      postPatch = ''
        sed '/^ILIBS\t/s/liblmdb\$(SOEXT)//' -i Makefile
      '';
    });
}
