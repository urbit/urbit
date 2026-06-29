::::  /tests/lib/math-rh -- @rh (f16) transcendentals via the @rs door (PR #18).
::  Each @rh function widens to f32, runs the (faithful) @rs kernel, and rounds
::  the result to f16 -- correctly-rounded, since an f32 result is ~2^13 finer
::  than an f16 ULP.  Expected = numpy float16 of the true value.
::
/+  *test, math
|%
++  eh   |=(x=@rh ^-(@ `@`(~(exp rh:math [%n .~~1e-2]) x)))
++  lh   |=(x=@rh ^-(@ `@`(~(log rh:math [%n .~~1e-2]) x)))
++  sh   |=(x=@rh ^-(@ `@`(~(sin rh:math [%n .~~1e-2]) x)))
++  ch   |=(x=@rh ^-(@ `@`(~(cos rh:math [%n .~~1e-2]) x)))
++  th   |=(x=@rh ^-(@ `@`(~(tan rh:math [%n .~~1e-2]) x)))
++  ath  |=(x=@rh ^-(@ `@`(~(atan rh:math [%n .~~1e-2]) x)))
++  ash  |=(x=@rh ^-(@ `@`(~(asin rh:math [%n .~~1e-2]) x)))
++  ach  |=(x=@rh ^-(@ `@`(~(acos rh:math [%n .~~1e-2]) x)))
++  qh   |=(x=@rh ^-(@ `@`(~(sqt rh:math [%n .~~1e-2]) x)))
++  cbh  |=(x=@rh ^-(@ `@`(~(cbt rh:math [%n .~~1e-2]) x)))
++  l2h  |=(x=@rh ^-(@ `@`(~(log-2 rh:math [%n .~~1e-2]) x)))
++  l10h  |=(x=@rh ^-(@ `@`(~(log-10 rh:math [%n .~~1e-2]) x)))
++  ph   |=([x=@rh n=@rh] ^-(@ `@`(~(pow rh:math [%n .~~1e-2]) x n)))
++  test-exp-half  (expect-eq !>(`@`0x3e98) !>((eh `@rh`0x3800)))
++  test-exp-1     (expect-eq !>(`@`0x4170) !>((eh `@rh`0x3c00)))
++  test-exp-n2    (expect-eq !>(`@`0x3055) !>((eh `@rh`0xc000)))
++  test-exp-inf   (expect-eq !>(`@`0x7c00) !>((eh `@rh`0x7c00)))
++  test-log-2     (expect-eq !>(`@`0x398c) !>((lh `@rh`0x4000)))
++  test-log-half  (expect-eq !>(`@`0xb98c) !>((lh `@rh`0x3800)))
++  test-sin-1     (expect-eq !>(`@`0x3abb) !>((sh `@rh`0x3c00)))
++  test-sin-pi    (expect-eq !>(`@`0x13ed) !>((sh `@rh`0x4248)))
++  test-cos-1     (expect-eq !>(`@`0x3853) !>((ch `@rh`0x3c00)))
++  test-tan-1     (expect-eq !>(`@`0x3e3a) !>((th `@rh`0x3c00)))
++  test-atan-1    (expect-eq !>(`@`0x3a48) !>((ath `@rh`0x3c00)))
++  test-atan-2    (expect-eq !>(`@`0x3c6e) !>((ath `@rh`0x4000)))
++  test-asin-half  (expect-eq !>(`@`0x3830) !>((ash `@rh`0x3800)))
++  test-acos-half  (expect-eq !>(`@`0x3c30) !>((ach `@rh`0x3800)))
++  test-sqt-2     (expect-eq !>(`@`0x3da8) !>((qh `@rh`0x4000)))
++  test-sqt-10    (expect-eq !>(`@`0x4253) !>((qh `@rh`0x4900)))
++  test-cbt-8     (expect-eq !>(`@`0x4000) !>((cbh `@rh`0x4800)))
++  test-log2-8    (expect-eq !>(`@`0x4200) !>((l2h `@rh`0x4800)))
++  test-log10-1k  (expect-eq !>(`@`0x4205) !>((l10h `@rh`0x6400)))
++  test-pow-2-h   (expect-eq !>(`@`0x3da8) !>((ph `@rh`0x4000 `@rh`0x3800)))
--
