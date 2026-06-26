::::  /tests/lib/math-exp -- bit-exact exp for the Chebyshev rewrite (PR #18).
::  Expected bit patterns are produced by libmath/tools/cheb_check.py, whose
::  strict-f64/f32 reference is bit-identical to SoftFloat for the algorithm's
::  primitives (+ - * /, round-to-int, ldexp).  Covers the normal range plus
::  the NaN / +-inf / overflow / underflow / subnormal tails.
::
/+  *test, math
|%
++  es  |=(x=@rs ^-(@ `@`(~(exp rs:math [%n .1e-5]) x)))
++  ed  |=(x=@rd ^-(@ `@`(~(exp rd:math [%n .~1e-10]) x)))
::  ==== @rd core ====
++  test-rd-0      (expect-eq !>(`@`0x3ff0.0000.0000.0000) !>((ed `@rd`0x0)))
++  test-rd-half   (expect-eq !>(`@`0x3ffa.6129.8e1e.069c) !>((ed `@rd`0x3fe0.0000.0000.0000)))
++  test-rd-1      (expect-eq !>(`@`0x4005.bf0a.8b14.576a) !>((ed `@rd`0x3ff0.0000.0000.0000)))
++  test-rd-n1     (expect-eq !>(`@`0x3fd7.8b56.362c.ef38) !>((ed `@rd`0xbff0.0000.0000.0000)))
++  test-rd-2      (expect-eq !>(`@`0x401d.8e64.b8d4.ddae) !>((ed `@rd`0x4000.0000.0000.0000)))
++  test-rd-10     (expect-eq !>(`@`0x40d5.829d.cf95.0560) !>((ed `@rd`0x4024.0000.0000.0000)))
++  test-rd-n5     (expect-eq !>(`@`0x3f7b.993f.e00d.5376) !>((ed `@rd`0xc014.0000.0000.0000)))
++  test-rd-tenth  (expect-eq !>(`@`0x3ff1.aec7.b35a.00d4) !>((ed `@rd`0x3fb9.9999.9999.999a)))
::  ==== @rd edges ====
++  test-rd-pinf   (expect-eq !>(`@`0x7ff0.0000.0000.0000) !>((ed `@rd`0x7ff0.0000.0000.0000)))
++  test-rd-ninf   (expect-eq !>(`@`0x0) !>((ed `@rd`0xfff0.0000.0000.0000)))
++  test-rd-nan    (expect-eq !>(`@`0x7ff8.0000.0000.0000) !>((ed `@rd`0x7ff8.0000.0000.0000)))
++  test-rd-big    (expect-eq !>(`@`0x7fe8.1e9b.4b52.d0c9) !>((ed `@rd`0x4086.2c00.0000.0000)))
++  test-rd-ovf    (expect-eq !>(`@`0x7ff0.0000.0000.0000) !>((ed `@rd`0x4086.3000.0000.0000)))
++  test-rd-ovf2   (expect-eq !>(`@`0x7ff0.0000.0000.0000) !>((ed `@rd`0x4086.8000.0000.0000)))
++  test-rd-sub    (expect-eq !>(`@`0x2) !>((ed `@rd`0xc087.4000.0000.0000)))
++  test-rd-udf    (expect-eq !>(`@`0x0) !>((ed `@rd`0xc087.4999.9999.999a)))
++  test-rd-udf2   (expect-eq !>(`@`0x0) !>((ed `@rd`0xc087.7000.0000.0000)))
::  ==== @rs core ====
++  test-rs-0      (expect-eq !>(`@`0x3f80.0000) !>((es `@rs`0x0)))
++  test-rs-half   (expect-eq !>(`@`0x3fd3.094d) !>((es `@rs`0x3f00.0000)))
++  test-rs-1      (expect-eq !>(`@`0x402d.f854) !>((es `@rs`0x3f80.0000)))
++  test-rs-n1     (expect-eq !>(`@`0x3ebc.5ab2) !>((es `@rs`0xbf80.0000)))
++  test-rs-2      (expect-eq !>(`@`0x40ec.7326) !>((es `@rs`0x4000.0000)))
++  test-rs-10     (expect-eq !>(`@`0x46ac.14ee) !>((es `@rs`0x4120.0000)))
++  test-rs-n5     (expect-eq !>(`@`0x3bdc.c9ff) !>((es `@rs`0xc0a0.0000)))
++  test-rs-tenth  (expect-eq !>(`@`0x3f8d.763e) !>((es `@rs`0x3dcc.cccd)))
::  ==== @rs edges ====
++  test-rs-pinf   (expect-eq !>(`@`0x7f80.0000) !>((es `@rs`0x7f80.0000)))
++  test-rs-ninf   (expect-eq !>(`@`0x0) !>((es `@rs`0xff80.0000)))
++  test-rs-nan    (expect-eq !>(`@`0x7fc0.0000) !>((es `@rs`0x7fc0.0000)))
++  test-rs-big    (expect-eq !>(`@`0x7ef8.82b7) !>((es `@rs`0x42b0.0000)))
++  test-rs-ovf    (expect-eq !>(`@`0x7f80.0000) !>((es `@rs`0x42b2.0000)))
++  test-rs-ovf2   (expect-eq !>(`@`0x7f80.0000) !>((es `@rs`0x42c8.0000)))
++  test-rs-sub    (expect-eq !>(`@`0x1) !>((es `@rs`0xc2ce.0000)))
++  test-rs-udf    (expect-eq !>(`@`0x0) !>((es `@rs`0xc2d0.0000)))
++  test-rs-udf2   (expect-eq !>(`@`0x0) !>((es `@rs`0xc2dc.0000)))
--
