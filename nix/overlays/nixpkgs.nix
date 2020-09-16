final: prev:

{
  h2o = prev.h2o.overrideAttrs (_old: {
    version = final.sources.h2o.rev;
    src = final.sources.h2o;
    outputs = [ "out" "dev" "lib" ];
  });

  libsigsegv = prev.libsigsegv.overrideAttrs (old: {
    postConfigure = prev.lib.optionalString prev.stdenv.isLinux ''
      header "patching config.h to disable stackvma on linux"

      substituteInPlace config.h \
        --replace '#define HAVE_STACKVMA 1' \
                  '#define HAVE_STACKVMA 0'
    '';
  });
} 
