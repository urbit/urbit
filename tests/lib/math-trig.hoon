::::  /tests/lib/math-trig -- bit-exact sin/cos for the Chebyshev rewrite (#18).
::  Expected bit patterns from libmath/tools/cheb_check.py.  Covers the normal
::  range, large arguments, and the NaN / +-inf / +-0 edges.
::
/+  *test, math
|%
++  sd  |=(x=@rd ^-(@ `@`(~(sin rd:math [%n .~1e-10 .~0]) x)))
++  cd  |=(x=@rd ^-(@ `@`(~(cos rd:math [%n .~1e-10 .~0]) x)))
++  ss  |=(x=@rs ^-(@ `@`(~(sin rs:math [%n .1e-5 .0]) x)))
++  cs  |=(x=@rs ^-(@ `@`(~(cos rs:math [%n .1e-5 .0]) x)))
::  ==== @rd ====
++  test-sin-0     (expect-eq !>(`@`0x0) !>((sd `@rd`0x0)))
++  test-sin-half  (expect-eq !>(`@`0x3fde.aee8.744b.05f0) !>((sd `@rd`0x3fe0.0000.0000.0000)))
++  test-sin-1     (expect-eq !>(`@`0x3fea.ed54.8f09.0cee) !>((sd `@rd`0x3ff0.0000.0000.0000)))
++  test-sin-n1    (expect-eq !>(`@`0xbfea.ed54.8f09.0cee) !>((sd `@rd`0xbff0.0000.0000.0000)))
++  test-sin-pio2  (expect-eq !>(`@`0x3ff0.0000.0000.0000) !>((sd `@rd`0x3ff9.21fb.5444.2d18)))
++  test-sin-10    (expect-eq !>(`@`0xbfe1.689e.f5f3.4f52) !>((sd `@rd`0x4024.0000.0000.0000)))
++  test-sin-100   (expect-eq !>(`@`0xbfe0.3425.b78c.4db8) !>((sd `@rd`0x4059.0000.0000.0000)))
++  test-sin-inf   (expect-eq !>(`@`0x7ff8.0000.0000.0000) !>((sd `@rd`0x7ff0.0000.0000.0000)))
++  test-sin-nan   (expect-eq !>(`@`0x7ff8.0000.0000.0000) !>((sd `@rd`0x7ff8.0000.0000.0000)))
++  test-sin-n0    (expect-eq !>(`@`0x8000.0000.0000.0000) !>((sd `@rd`0x8000.0000.0000.0000)))
++  test-cos-0     (expect-eq !>(`@`0x3ff0.0000.0000.0000) !>((cd `@rd`0x0)))
++  test-cos-half  (expect-eq !>(`@`0x3fec.1528.065b.7d50) !>((cd `@rd`0x3fe0.0000.0000.0000)))
++  test-cos-1     (expect-eq !>(`@`0x3fe1.4a28.0fb5.068c) !>((cd `@rd`0x3ff0.0000.0000.0000)))
++  test-cos-pi    (expect-eq !>(`@`0xbff0.0000.0000.0000) !>((cd `@rd`0x4009.21fb.5444.2d18)))
++  test-cos-10    (expect-eq !>(`@`0xbfea.d9ac.890c.6b1f) !>((cd `@rd`0x4024.0000.0000.0000)))
++  test-cos-100   (expect-eq !>(`@`0x3feb.981d.bf66.5fdf) !>((cd `@rd`0x4059.0000.0000.0000)))
++  test-cos-inf   (expect-eq !>(`@`0x7ff8.0000.0000.0000) !>((cd `@rd`0xfff0.0000.0000.0000)))
++  test-cos-n0    (expect-eq !>(`@`0x3ff0.0000.0000.0000) !>((cd `@rd`0x8000.0000.0000.0000)))
::  ==== @rs ====
++  test-sin-s-0     (expect-eq !>(`@`0x0) !>((ss `@rs`0x0)))
++  test-sin-s-half  (expect-eq !>(`@`0x3ef5.7744) !>((ss `@rs`0x3f00.0000)))
++  test-sin-s-1     (expect-eq !>(`@`0x3f57.6aa4) !>((ss `@rs`0x3f80.0000)))
++  test-sin-s-n1    (expect-eq !>(`@`0xbf57.6aa4) !>((ss `@rs`0xbf80.0000)))
++  test-sin-s-10    (expect-eq !>(`@`0xbf0b.44f8) !>((ss `@rs`0x4120.0000)))
++  test-sin-s-100   (expect-eq !>(`@`0xbf01.a12e) !>((ss `@rs`0x42c8.0000)))
++  test-sin-s-inf   (expect-eq !>(`@`0x7fc0.0000) !>((ss `@rs`0x7f80.0000)))
++  test-sin-s-n0    (expect-eq !>(`@`0x8000.0000) !>((ss `@rs`0x8000.0000)))
++  test-cos-s-0     (expect-eq !>(`@`0x3f80.0000) !>((cs `@rs`0x0)))
++  test-cos-s-half  (expect-eq !>(`@`0x3f60.a940) !>((cs `@rs`0x3f00.0000)))
++  test-cos-s-1     (expect-eq !>(`@`0x3f0a.5140) !>((cs `@rs`0x3f80.0000)))
++  test-cos-s-10    (expect-eq !>(`@`0xbf56.cd64) !>((cs `@rs`0x4120.0000)))
++  test-cos-s-100   (expect-eq !>(`@`0x3f5c.c0ee) !>((cs `@rs`0x42c8.0000)))
++  test-cos-s-inf   (expect-eq !>(`@`0x7fc0.0000) !>((cs `@rs`0xff80.0000)))
++  test-cos-s-n0    (expect-eq !>(`@`0x3f80.0000) !>((cs `@rs`0x8000.0000)))
--
