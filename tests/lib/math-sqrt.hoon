::::  /tests/lib/math-sqrt -- correctly-rounded sqrt (PR #18).
::  @rs delegates to the SoftFloat f32 root; @rd seeds with the (faithful)
::  stdlib f64 root and applies one Markstein FMA correction.  Expected values
::  are the correctly-rounded roots (mpmath).
::
/+  *test, math
|%
++  qd  |=(x=@rd ^-(@ `@`(~(sqt rd:math [%n .~1e-10 .~0]) x)))
++  qs  |=(x=@rs ^-(@ `@`(~(sqt rs:math [%n .1e-5 .0]) x)))
::  ==== @rd ====
++  test-rd-2    (expect-eq !>(`@`0x3ff6.a09e.667f.3bcd) !>((qd `@rd`0x4000.0000.0000.0000)))
++  test-rd-half  (expect-eq !>(`@`0x3fe6.a09e.667f.3bcd) !>((qd `@rd`0x3fe0.0000.0000.0000)))
++  test-rd-4    (expect-eq !>(`@`0x4000.0000.0000.0000) !>((qd `@rd`0x4010.0000.0000.0000)))
++  test-rd-1e5  (expect-eq !>(`@`0x4073.c3a4.edfa.9759) !>((qd `@rd`0x40f8.6a00.0000.0000)))
++  test-rd-tenth  (expect-eq !>(`@`0x3fd4.3d13.6248.490f) !>((qd `@rd`0x3fb9.9999.9999.999a)))
++  test-rd-sub  (expect-eq !>(`@`0x20ca.2fe7.6a3f.9475) !>((qd `@rd`0x1a5.6e1f.c2f8.f359)))
++  test-rd-9    (expect-eq !>(`@`0x4008.0000.0000.0000) !>((qd `@rd`0x4022.0000.0000.0000)))
++  test-rd-0    (expect-eq !>(`@`0x0) !>((qd `@rd`0x0)))
++  test-rd-n0   (expect-eq !>(`@`0x8000.0000.0000.0000) !>((qd `@rd`0x8000.0000.0000.0000)))
++  test-rd-neg  (expect-eq !>(`@`0x7ff8.0000.0000.0000) !>((qd `@rd`0xbff0.0000.0000.0000)))
++  test-rd-inf  (expect-eq !>(`@`0x7ff0.0000.0000.0000) !>((qd `@rd`0x7ff0.0000.0000.0000)))
++  test-rd-nan  (expect-eq !>(`@`0x7ff8.0000.0000.0000) !>((qd `@rd`0x7ff8.0000.0000.0000)))
::  ==== @rs ====
++  test-rs-2    (expect-eq !>(`@`0x3fb5.04f3) !>((qs `@rs`0x4000.0000)))
++  test-rs-half  (expect-eq !>(`@`0x3f35.04f3) !>((qs `@rs`0x3f00.0000)))
++  test-rs-4    (expect-eq !>(`@`0x4000.0000) !>((qs `@rs`0x4080.0000)))
++  test-rs-1e5  (expect-eq !>(`@`0x439e.1d27) !>((qs `@rs`0x47c3.5000)))
++  test-rs-tenth  (expect-eq !>(`@`0x3ea1.e89b) !>((qs `@rs`0x3dcc.cccd)))
++  test-rs-9    (expect-eq !>(`@`0x4040.0000) !>((qs `@rs`0x4110.0000)))
--
