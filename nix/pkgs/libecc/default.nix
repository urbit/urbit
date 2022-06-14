{ stdenv, sources, python3, pkgconfig }:

stdenv.mkDerivation {
  pname = "libecc";
  version = sources.libecc.rev;
  src = sources.libecc;

  patches = [ ./makefile_static.patch ];

  configurePhase = ''
   python scripts/expand_libecc.py --name="STARK" \
     --prime=3618502788666131213697322783095070105623107215331596699973092056135872020481 \
     --a=1 \
     --b=3141592653589793238462643383279502884197169399375105820974944592307816406665 \
     --gx=874739451078007766457464989774322083649278607533249481151382481072868806602 \
     --gy=152666792071518830868575557812948353041420400780739481342941381225525861407 \
     --order=3618502788666131213697322783095070105526743751716087489154079457884512865583 \
     --cofactor=1 
  '';

  buildPhase = ''
    NIX_CFLAGS_COMPILE="-DWITH_STDLIB $NIX_CFLAGS_COMPILE"
    make WARNING_CFLAGS='-Wno-error'
  '';

  installPhase = ''
    mkdir -p $out/lib
    mkdir -p $out/include
    cp build/libarith.a build/libec.a build/libsign.a $out/lib
    cd src
    cp --parents `find -name \*.h` $out/include
  '';

  buildInputs = [ python3 ];
  nativeBuildInputs = [ pkgconfig ];
}
