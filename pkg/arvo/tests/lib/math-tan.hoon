::::  /tests/lib/math-tan -- bit-exact @rd tan (dedicated fdlibm kernel, PR #18).
::  @rd tan now uses __kernel_tan (faithful <=1 ULP); @rs stays as the sin/cos
::  ratio (~1.2 ULP -- a pure-f32 tan kernel is worse near the poles).
::  Expected bits from libmath/tools/cheb_check.py.
::
/+  *test, math
|%
++  td  |=(x=@rd ^-(@ `@`(~(tan rd:math [%n .~1e-10]) x)))
++  test-tan-0     (expect-eq !>(`@`0x0) !>((td `@rd`0x0)))
++  test-tan-half  (expect-eq !>(`@`0x3fe1.7b4f.5bf3.474a) !>((td `@rd`0x3fe0.0000.0000.0000)))
++  test-tan-1     (expect-eq !>(`@`0x3ff8.eb24.5cbe.e3a6) !>((td `@rd`0x3ff0.0000.0000.0000)))
++  test-tan-n1    (expect-eq !>(`@`0xbff8.eb24.5cbe.e3a6) !>((td `@rd`0xbff0.0000.0000.0000)))
++  test-tan-pio4  (expect-eq !>(`@`0x3fef.ffff.ffff.ffff) !>((td `@rd`0x3fe9.21fb.5444.2d18)))
++  test-tan-2     (expect-eq !>(`@`0xc001.7af6.2e09.50f8) !>((td `@rd`0x4000.0000.0000.0000)))
++  test-tan-10    (expect-eq !>(`@`0x3fe4.bf5f.34be.3782) !>((td `@rd`0x4024.0000.0000.0000)))
++  test-tan-100   (expect-eq !>(`@`0xbfe2.ca74.d62b.5d38) !>((td `@rd`0x4059.0000.0000.0000)))
++  test-tan-inf   (expect-eq !>(`@`0x7ff8.0000.0000.0000) !>((td `@rd`0x7ff0.0000.0000.0000)))
++  test-tan-nan   (expect-eq !>(`@`0x7ff8.0000.0000.0000) !>((td `@rd`0x7ff8.0000.0000.0000)))
++  test-tan-n0    (expect-eq !>(`@`0x8000.0000.0000.0000) !>((td `@rd`0x8000.0000.0000.0000)))
--
