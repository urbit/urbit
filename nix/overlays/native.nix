final: prev:

let

  optionalList = xs: if xs == null then [ ] else xs;

in {
  h2o = prev.h2o.overrideAttrs (_attrs: {
    version = final.sources.h2o.rev;
    src = final.sources.h2o;
    outputs = [ "out" "dev" "lib" ];
  });

  libsigsegv = prev.libsigsegv.overrideAttrs (attrs: {
    patches = optionalList attrs.patches ++ [
      ../pkgs/libsigsegv/disable-stackvma_fault-linux-arm.patch
      ../pkgs/libsigsegv/disable-stackvma_fault-linux-i386.patch
    ];
  });

  lmdb = prev.lmdb.overrideAttrs (attrs: {
    patches =
      optionalList attrs.patches ++ prev.lib.optional prev.stdenv.isDarwin [
        ../pkgs/lmdb/darwin-fsync.patch
      ];
  });
}
