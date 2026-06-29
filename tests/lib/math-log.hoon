::::  /tests/lib/math-log -- bit-exact log for the Chebyshev rewrite (PR #18).
::  Expected bit patterns from libmath/tools/cheb_check.py (strict f64/f32 ref,
::  bit-identical to SoftFloat for the primitives used).  Covers the normal
::  range plus NaN / +inf / -inf / +-0 / negative / subnormal inputs.
::
/+  *test, math
|%
++  ls  |=(x=@rs ^-(@ `@`(~(log rs:math [%n .1e-5]) x)))
++  ld  |=(x=@rd ^-(@ `@`(~(log rd:math [%n .~1e-10]) x)))
::  ==== @rd ====
++  test-rd-1      (expect-eq !>(`@`0x0) !>((ld `@rd`0x3ff0.0000.0000.0000)))
++  test-rd-2      (expect-eq !>(`@`0x3fe6.2e42.fefa.39ef) !>((ld `@rd`0x4000.0000.0000.0000)))
++  test-rd-half   (expect-eq !>(`@`0xbfe6.2e42.fefa.39ef) !>((ld `@rd`0x3fe0.0000.0000.0000)))
++  test-rd-10     (expect-eq !>(`@`0x4002.6bb1.bbb5.5516) !>((ld `@rd`0x4024.0000.0000.0000)))
++  test-rd-100    (expect-eq !>(`@`0x4012.6bb1.bbb5.5516) !>((ld `@rd`0x4059.0000.0000.0000)))
++  test-rd-tenth  (expect-eq !>(`@`0xc002.6bb1.bbb5.5515) !>((ld `@rd`0x3fb9.9999.9999.999a)))
++  test-rd-e2     (expect-eq !>(`@`0x4000.0000.0000.0000) !>((ld `@rd`0x401d.8e64.b8d4.ddae)))
++  test-rd-sub    (expect-eq !>(`@`0xc086.4e69.394d.9508) !>((ld `@rd`0x1268.8b70.e62b)))
++  test-rd-pinf   (expect-eq !>(`@`0x7ff0.0000.0000.0000) !>((ld `@rd`0x7ff0.0000.0000.0000)))
++  test-rd-ninf   (expect-eq !>(`@`0x7ff8.0000.0000.0000) !>((ld `@rd`0xfff0.0000.0000.0000)))
++  test-rd-nan    (expect-eq !>(`@`0x7ff8.0000.0000.0000) !>((ld `@rd`0x7ff8.0000.0000.0000)))
++  test-rd-zero   (expect-eq !>(`@`0xfff0.0000.0000.0000) !>((ld `@rd`0x0)))
++  test-rd-neg    (expect-eq !>(`@`0x7ff8.0000.0000.0000) !>((ld `@rd`0xbff0.0000.0000.0000)))
::  ==== @rs ====
++  test-rs-1      (expect-eq !>(`@`0x0) !>((ls `@rs`0x3f80.0000)))
++  test-rs-2      (expect-eq !>(`@`0x3f31.7218) !>((ls `@rs`0x4000.0000)))
++  test-rs-half   (expect-eq !>(`@`0xbf31.7218) !>((ls `@rs`0x3f00.0000)))
++  test-rs-10     (expect-eq !>(`@`0x4013.5d8e) !>((ls `@rs`0x4120.0000)))
++  test-rs-100    (expect-eq !>(`@`0x4093.5d8e) !>((ls `@rs`0x42c8.0000)))
++  test-rs-tenth  (expect-eq !>(`@`0xc013.5d8e) !>((ls `@rs`0x3dcc.cccd)))
++  test-rs-e2     (expect-eq !>(`@`0x4000.0000) !>((ls `@rs`0x40ec.7326)))
++  test-rs-sub    (expect-eq !>(`@`0xc2b8.34f2) !>((ls `@rs`0x1.16c2)))
++  test-rs-pinf   (expect-eq !>(`@`0x7f80.0000) !>((ls `@rs`0x7f80.0000)))
++  test-rs-ninf   (expect-eq !>(`@`0x7fc0.0000) !>((ls `@rs`0xff80.0000)))
++  test-rs-nan    (expect-eq !>(`@`0x7fc0.0000) !>((ls `@rs`0x7fc0.0000)))
++  test-rs-zero   (expect-eq !>(`@`0xff80.0000) !>((ls `@rs`0x0)))
++  test-rs-neg    (expect-eq !>(`@`0x7fc0.0000) !>((ls `@rs`0xbf80.0000)))
--
