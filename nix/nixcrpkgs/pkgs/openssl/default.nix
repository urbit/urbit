{ crossenv }:

crossenv.make_derivation rec {
    name = "openssl-${version}";
    version = "1.1.1";

    native_inputs = [ crossenv.nixpkgs.perl ];

    coreutils = crossenv.nixpkgs.coreutils;

    src = crossenv.nixpkgs.fetchurl {
      url = "https://www.openssl.org/source/${name}.tar.gz";
      sha256 = "0gbab2fjgms1kx5xjvqx8bxhr98k4r8l2fa8vw7kvh491xd8fdi8";
    };

  builder = ./builder.sh;
}

# let
#
#   coreutils = crossenv.nixpkgs.coreutils;
#
# # with stdenv.lib;
#
# in
#
# {
#
#     patches = [ ./nix-ssl-cert-file.patch ];
#
#     native_inputs = [ crossenv.nixpkgs.perl ];
#
#     postPatch = ''
#       patchShebangs Configure
#     '' + optionalString (versionAtLeast version "1.1.1") ''
#       substituteInPlace config --replace '/usr/bin/env' '${coreutils}/bin/env'
#     '' + optionalString (versionAtLeast version "1.1.0" && stdenv.hostPlatform.isMusl) ''
#       substituteInPlace crypto/async/arch/async_posix.h \
#         --replace '!defined(__ANDROID__) && !defined(__OpenBSD__)' \
#                   '!defined(__ANDROID__) && !defined(__OpenBSD__) && 0'
#     '';
#
#     configureScript = {
#         "x86_64-darwin"  = "./Configure darwin64-x86_64-cc";
#         "x86_64-solaris" = "./Configure solaris64-x86_64-gcc";
#         "armv6l-linux" = "./Configure linux-armv4 -march=armv6";
#         "armv7l-linux" = "./Configure linux-armv4 -march=armv7-a";
#       }.${stdenv.hostPlatform.system} or (
#         if stdenv.hostPlatform == stdenv.buildPlatform
#           then "./config"
#         else if stdenv.hostPlatform.isMinGW
#           then "./Configure mingw${optionalString
#                                      (stdenv.hostPlatform.parsed.cpu.bits != 32)
#                                      (toString stdenv.hostPlatform.parsed.cpu.bits)}"
#         else if stdenv.hostPlatform.isLinux
#           then "./Configure linux-generic${toString stdenv.hostPlatform.parsed.cpu.bits}"
#         else if stdenv.hostPlatform.isiOS
#           then "./Configure ios${toString stdenv.hostPlatform.parsed.cpu.bits}-cross"
#         else
#           throw "Not sure what configuration to use for ${stdenv.hostPlatform.config}"
#       );
#
#     configureFlags = [
#       "shared" # "shared" builds both shared and static libraries
#       "--libdir=lib"
#       "--openssldir=etc/ssl"
#     ] ++ stdenv.lib.optionals withCryptodev [
#       "-DHAVE_CRYPTODEV"
#       "-DUSE_CRYPTODEV_DIGESTS"
#     ]
#       ++ stdenv.lib.optional (versionAtLeast version "1.1.0" && stdenv.hostPlatform.isAarch64) "no-afalgeng";
#
#     postInstall = ''
#       mkdir -p $bin
#       mv $out/bin $bin/
#
#       mkdir $dev
#       mv $out/include $dev/
#
#       # remove dependency on Perl at runtime
#       rm -r $out/etc/ssl/misc
#
#       rmdir $out/etc/ssl/{certs,private}
#     '';
#   };
