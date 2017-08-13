{ crossenv }:

if crossenv.os != "linux" then "linux only" else

crossenv.make_derivation rec {
  name = "libudev-${version}";
  version = "234";
  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/systemd/systemd/archive/v${version}.tar.gz";
    sha256 = "0shbv3hrmryfr22v07s2mh8v8dwhjba2ldrk739q7jd11b8njgns";
  };
  builder = ./builder.sh;
  patches = [
    # Fix some compile-time errors caused by not using glibc.
    ./megapatch.patch
  ];
  fill = ./fill;

  size_flags = let
    ptr_size =
      if crossenv.arch == "x86_64" then "8"
      else "4";
    in
      "-DSIZEOF_PID_T=4 " +
      "-DSIZEOF_UID_T=4 " +
      "-DSIZEOF_GID_T=4 " +
      "-DSIZEOF_TIME_T=${ptr_size} " +
      "-DSIZEOF_RLIM_T=8 " +
      "-DSIZEOF_INO_T=8 " +
      "-DSIZEOF_DEV_T=8";

 CFLAGS = "-Werror -DHAVE_DECL_SETNS -D_GNU_SOURCE ${size_flags}";
}
