{ pkgs ? import ../nixpkgs.nix { }, lib ? pkgs.lib }:

#
# Builds semi-static versions of libraries out of nixpkgs so that we can build
# an OSX binary without any dependencies on nix libraries.
#

let
  ourzlib = pkgs.zlib.override {
    shared = false;
    static = true;
    splitStaticOutput = false;
  };

  ouropenssl = (pkgs.openssl.override {
    static = true;
  }).overrideAttrs (oldAttrs: rec {
    dontDisableStatic = true;
    configureFlats = lib.remove "shared" oldAttrs.configureFlags;
  });

  ourcurl = (pkgs.curl.override {
    http2Support = false;
    scpSupport = false;
    gssSupport = false;
    zlib = ourzlib;
    openssl = ouropenssl;
  }).overrideAttrs (oldAttrs: rec {
    dontDisableStatic = true;
    configureFlags = oldAttrs.configureFlags ++ [
      "--disable-ldap"
      "--disable-gssapi"
      "--disable-scp"
      "--disable-shared"
      "--disable-manual"
    ];
  });

  ourgmp = pkgs.gmp.overrideAttrs (oldAttrs: rec {
    dontDisableStatic = true;
    configureFlags = oldAttrs.configureFlags ++ [
      "--disable-shared"
    ];
  });

  ourlmdb = pkgs.lmdb.overrideAttrs (oldAttrs: rec {
    makeFlags = [ "prefix=$(out)" "CC=cc" ];
    # Why remove the so version? It's easier than preventing it from being
    # built with lmdb's custom Makefiles, and it can't exist in the output
    # because otherwise the linker will preferentially choose the .so over the
    # .a.
    postInstall = ''
      rm $out/lib/liblmdb.so
    '';
  });

in rec {
  zlib = ourzlib;
  openssl = ouropenssl;
  curl = ourcurl;
  gmp = ourgmp;
  lmdb = ourlmdb;
}
