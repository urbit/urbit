{ crossenv, libusbp }:

crossenv.make_derivation rec {
  name = "p-load-${version}";

  version = "2041b02";  # 2.1.0ish

  src = crossenv.nixpkgs.fetchurl {
    url = "https://github.com/pololu/p-load/archive/${version}.tar.gz";
    sha256 = "07xn0k96pkvirsh45zn9976lwliiqkfx76vy1yrbx6kp55ssp2zp";
  };

  builder = ./builder.sh;

  cross_inputs = [ libusbp ];
}
