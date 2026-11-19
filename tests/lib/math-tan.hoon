::::  /tests/lib/math-tan -- bit-exact @rd and @rs tan (dedicated kernels).
::  @rd uses fdlibm's own __kernel_tan (faithful <=1 ULP).  @rs (2026-07-03)
::  now ALSO has a dedicated kernel (~0.94 ULP) -- a prior attempt had
::  concluded "a pure-f32 tan kernel is worse near the poles" (~1.2 ULP
::  ratio beat a ~9.68 ULP draft kernel), but that draft's bad result traced
::  to evaluation order (multiplying the dominant term through the whole
::  polynomial), not to f32 precision itself; keeping the dominant term
::  separate and adding it last (mirroring fdlibm's own w2=x+r structure)
::  fixes it.  See NEXT-STEPS.md.  @rh/@rq stay on the sin/cos ratio: @rh's
::  11-bit mantissa can't support a kernel beating the ratio, and @rq's
::  series needs 45+ growing-magnitude terms that destabilize native
::  chained rounding.  Expected bits from libmath/tools/cheb_check.py.
::
/+  *test, math
|%
++  td  |=(x=@rd ^-(@ `@`(~(tan rd:math [%n .~1e-10 .~0]) x)))
++  ts  |=(x=@rs ^-(@ `@`(~(tan rs:math [%n .1e-5 .0]) x)))
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
++  test-tan-rs-0     (expect-eq !>(`@`0x0) !>((ts `@rs`0x0)))
++  test-tan-rs-half  (expect-eq !>(`@`0x3f0b.da7b) !>((ts `@rs`0x3f00.0000)))
++  test-tan-rs-1     (expect-eq !>(`@`0x3fc7.5922) !>((ts `@rs`0x3f80.0000)))
++  test-tan-rs-n1    (expect-eq !>(`@`0xbfc7.5922) !>((ts `@rs`0xbf80.0000)))
::  0x3f800000, not 0x3f800001: at this exact tie (ax*2/pi lands on .5),
::  q=0 (round-to-even) gives the mpmath-verified closer answer -- an
::  earlier version of this expectation came from cheb_check.py's Python
::  reduce_pio2_32, whose own q rounding disagreed with Hoon's at this one
::  boundary point; harmless for the measured ~0.94 ULP figure (a
::  continuous 200k-point sweep essentially never lands exactly on a tie),
::  but wrong as a hardcoded test vector for this landmark input.
++  test-tan-rs-pio4  (expect-eq !>(`@`0x3f80.0000) !>((ts `@rs`0x3f49.0fdb)))
++  test-tan-rs-2     (expect-eq !>(`@`0xc00b.d7b2) !>((ts `@rs`0x4000.0000)))
++  test-tan-rs-10    (expect-eq !>(`@`0x3f25.fafa) !>((ts `@rs`0x4120.0000)))
++  test-tan-rs-100   (expect-eq !>(`@`0xbf16.53a7) !>((ts `@rs`0x42c8.0000)))
++  test-tan-rs-inf   (expect-eq !>(`@`0x7fc0.0000) !>((ts `@rs`0x7f80.0000)))
++  test-tan-rs-nan   (expect-eq !>(`@`0x7fc0.0000) !>((ts `@rs`0x7fc0.0000)))
++  test-tan-rs-n0    (expect-eq !>(`@`0x8000.0000) !>((ts `@rs`0x8000.0000)))
--
