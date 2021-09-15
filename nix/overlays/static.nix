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

  # curl = enableStatic prev.curl;

  libuv = enableStatic prev.libuv;

  libffi = enableStatic prev.libffi;

  secp256k1 = enableStatic prev.secp256k1;

  libiconv = prev.libiconv.overrideAttrs (o: {
          postInstall = "rm $out/include/libcharset.h $out/include/localcharset.h";
          configureFlags = ["--disable-shared" "--enable-static"];
  });

  lmdb = prev.lmdb.overrideAttrs (old:
    configureFlags old // {
      # Why remove the so version? It's easier than preventing it from being
      # built with lmdb's custom Makefiles, and it can't exist in the output
      # because otherwise the linker will preferentially choose the .so over
      # the .a.
      postInstall = ''
        rm $out/lib/liblmdb.so
      '';
    });
}
