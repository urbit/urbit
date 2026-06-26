::::  /tests/lib/math-ainv -- bit-exact asin/acos (PR #18).
::  fdlibm rational P/Q kernel (shared), sqrt head/tail split on [0.5,1).
::  Expected bits from libmath/tools/cheb_check.py.
::
/+  *test, math
|%
++  sad  |=(x=@rd ^-(@ `@`(~(asin rd:math [%n .~1e-10]) x)))
++  cad  |=(x=@rd ^-(@ `@`(~(acos rd:math [%n .~1e-10]) x)))
++  sas  |=(x=@rs ^-(@ `@`(~(asin rs:math [%n .1e-5]) x)))
++  cas  |=(x=@rs ^-(@ `@`(~(acos rs:math [%n .1e-5]) x)))
::  ==== @rd asin ====
++  test-asin-0     (expect-eq !>(`@`0x0) !>((sad `@rd`0x0)))
++  test-asin-half  (expect-eq !>(`@`0x3fe0.c152.382d.7366) !>((sad `@rd`0x3fe0.0000.0000.0000)))
++  test-asin-1     (expect-eq !>(`@`0x3ff9.21fb.5444.2d18) !>((sad `@rd`0x3ff0.0000.0000.0000)))
++  test-asin-n1    (expect-eq !>(`@`0xbff9.21fb.5444.2d18) !>((sad `@rd`0xbff0.0000.0000.0000)))
++  test-asin-75    (expect-eq !>(`@`0x3feb.2353.15c6.80dc) !>((sad `@rd`0x3fe8.0000.0000.0000)))
++  test-asin-9     (expect-eq !>(`@`0x3ff1.ea93.705f.a172) !>((sad `@rd`0x3fec.cccc.cccc.cccd)))
++  test-asin-99    (expect-eq !>(`@`0x3ff6.de3c.6f33.d51d) !>((sad `@rd`0x3fef.ae14.7ae1.47ae)))
++  test-asin-n6    (expect-eq !>(`@`0xbfe4.978f.a326.9ee1) !>((sad `@rd`0xbfe3.3333.3333.3333)))
++  test-asin-nan   (expect-eq !>(`@`0x7ff8.0000.0000.0000) !>((sad `@rd`0x7ff8.0000.0000.0000)))
++  test-asin-big   (expect-eq !>(`@`0x7ff8.0000.0000.0000) !>((sad `@rd`0x3ff8.0000.0000.0000)))
::  ==== @rd acos ====
++  test-acos-0     (expect-eq !>(`@`0x3ff9.21fb.5444.2d18) !>((cad `@rd`0x0)))
++  test-acos-half  (expect-eq !>(`@`0x3ff0.c152.382d.7366) !>((cad `@rd`0x3fe0.0000.0000.0000)))
++  test-acos-1     (expect-eq !>(`@`0x0) !>((cad `@rd`0x3ff0.0000.0000.0000)))
++  test-acos-n1    (expect-eq !>(`@`0x4009.21fb.5444.2d18) !>((cad `@rd`0xbff0.0000.0000.0000)))
++  test-acos-9     (expect-eq !>(`@`0x3fdc.dd9f.8f92.2e98) !>((cad `@rd`0x3fec.cccc.cccc.cccd)))
++  test-acos-n9    (expect-eq !>(`@`0x4005.8647.6251.e745) !>((cad `@rd`0xbfec.cccc.cccc.cccd)))
++  test-acos-nan   (expect-eq !>(`@`0x7ff8.0000.0000.0000) !>((cad `@rd`0x7ff8.0000.0000.0000)))
::  ==== @rs ====
++  test-asin-s-half  (expect-eq !>(`@`0x3f06.0a92) !>((sas `@rs`0x3f00.0000)))
++  test-asin-s-1     (expect-eq !>(`@`0x3fc9.0fdb) !>((sas `@rs`0x3f80.0000)))
++  test-asin-s-75    (expect-eq !>(`@`0x3f59.1a99) !>((sas `@rs`0x3f40.0000)))
++  test-asin-s-9     (expect-eq !>(`@`0x3f8f.549b) !>((sas `@rs`0x3f66.6666)))
++  test-asin-s-nan   (expect-eq !>(`@`0x7fc0.0000) !>((sas `@rs`0x7fc0.0000)))
++  test-acos-s-0     (expect-eq !>(`@`0x3fc9.0fdb) !>((cas `@rs`0x0)))
++  test-acos-s-1     (expect-eq !>(`@`0x0) !>((cas `@rs`0x3f80.0000)))
++  test-acos-s-n1    (expect-eq !>(`@`0x4049.0fdb) !>((cas `@rs`0xbf80.0000)))
++  test-acos-s-n6    (expect-eq !>(`@`0x400d.b70d) !>((cas `@rs`0xbf19.999a)))
--
