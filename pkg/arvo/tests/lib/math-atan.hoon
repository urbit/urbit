::::  /tests/lib/math-atan -- bit-exact atan + atan2 (PR #18).
::  atan: fdlibm breakpoint reduction + minimax poly.  atan2 derives from atan
::  with quadrant logic.  Expected bits from libmath/tools/cheb_check.py.
::
/+  *test, math
|%
++  ad   |=(x=@rd ^-(@ `@`(~(atan rd:math [%n .~1e-10]) x)))
++  as   |=(x=@rs ^-(@ `@`(~(atan rs:math [%n .1e-5]) x)))
++  a2d  |=([y=@rd x=@rd] ^-(@ `@`(~(atan2 rd:math [%n .~1e-10]) y x)))
::  ==== @rd ====
++  test-atan-0      (expect-eq !>(`@`0x0) !>((ad `@rd`0x0)))
++  test-atan-half   (expect-eq !>(`@`0x3fdd.ac67.0561.bb4f) !>((ad `@rd`0x3fe0.0000.0000.0000)))
++  test-atan-1      (expect-eq !>(`@`0x3fe9.21fb.5444.2d18) !>((ad `@rd`0x3ff0.0000.0000.0000)))
++  test-atan-n1     (expect-eq !>(`@`0xbfe9.21fb.5444.2d18) !>((ad `@rd`0xbff0.0000.0000.0000)))
++  test-atan-1h     (expect-eq !>(`@`0x3fef.730b.d281.f69b) !>((ad `@rd`0x3ff8.0000.0000.0000)))
++  test-atan-2      (expect-eq !>(`@`0x3ff1.b6e1.92eb.be44) !>((ad `@rd`0x4000.0000.0000.0000)))
++  test-atan-10     (expect-eq !>(`@`0x3ff7.89bd.2c16.0054) !>((ad `@rd`0x4024.0000.0000.0000)))
++  test-atan-tenth  (expect-eq !>(`@`0x3fb9.83e2.82e2.cc4d) !>((ad `@rd`0x3fb9.9999.9999.999a)))
++  test-atan-n07    (expect-eq !>(`@`0xbfe3.8b11.2d7b.d4ad) !>((ad `@rd`0xbfe6.6666.6666.6666)))
++  test-atan-inf    (expect-eq !>(`@`0x3ff9.21fb.5444.2d18) !>((ad `@rd`0x7ff0.0000.0000.0000)))
++  test-atan-ninf   (expect-eq !>(`@`0xbff9.21fb.5444.2d18) !>((ad `@rd`0xfff0.0000.0000.0000)))
++  test-atan-nan    (expect-eq !>(`@`0x7ff8.0000.0000.0000) !>((ad `@rd`0x7ff8.0000.0000.0000)))
++  test-atan-n0     (expect-eq !>(`@`0x8000.0000.0000.0000) !>((ad `@rd`0x8000.0000.0000.0000)))
::  ==== @rs ====
++  test-atan-s-half  (expect-eq !>(`@`0x3eed.6338) !>((as `@rs`0x3f00.0000)))
++  test-atan-s-1     (expect-eq !>(`@`0x3f49.0fdb) !>((as `@rs`0x3f80.0000)))
++  test-atan-s-n1    (expect-eq !>(`@`0xbf49.0fdb) !>((as `@rs`0xbf80.0000)))
++  test-atan-s-2     (expect-eq !>(`@`0x3f8d.b70d) !>((as `@rs`0x4000.0000)))
++  test-atan-s-10    (expect-eq !>(`@`0x3fbc.4de9) !>((as `@rs`0x4120.0000)))
++  test-atan-s-inf   (expect-eq !>(`@`0x3fc9.0fdb) !>((as `@rs`0x7f80.0000)))
++  test-atan-s-nan   (expect-eq !>(`@`0x7fc0.0000) !>((as `@rs`0x7fc0.0000)))
::  ==== atan2 (derives from atan): quadrant + axis sanity ====
++  test-atan2-q1     (expect-eq !>(`@`0x3fe9.21fb.5444.2d18) !>((a2d `@rd`0x3ff0.0000.0000.0000 `@rd`0x3ff0.0000.0000.0000)))
++  test-atan2-yaxis  (expect-eq !>(`@`0x3ff9.21fb.5444.2d18) !>((a2d `@rd`0x3ff0.0000.0000.0000 `@rd`0x0)))
--
