::::  /tests/lib/math-derived -- log-2, log-10, pow, cbrt (PR #18).
::  These derive from the now-accurate exp/log: log-2 = log/ln2, log-10 =
::  log/ln10, pow = exp(n*log x), cbrt = sign*exp(log|x|/3).  They are
::  deterministic (so bit-exact-jettable via the same composition) but only
::  faithful to a few ULP (pow especially -- a notoriously hard function).
::  Expected bits from libmath/tools/cheb_check.py composing exp/log.
::
/+  *test, math
|%
++  l2d   |=(x=@rd ^-(@ `@`(~(log-2 rd:math [%n .~1e-10]) x)))
++  l10d  |=(x=@rd ^-(@ `@`(~(log-10 rd:math [%n .~1e-10]) x)))
++  pd    |=([x=@rd n=@rd] ^-(@ `@`(~(pow rd:math [%n .~1e-10]) x n)))
++  cbd   |=(x=@rd ^-(@ `@`(~(cbt rd:math [%n .~1e-10]) x)))
++  l2s   |=(x=@rs ^-(@ `@`(~(log-2 rs:math [%n .1e-5]) x)))
++  l10s  |=(x=@rs ^-(@ `@`(~(log-10 rs:math [%n .1e-5]) x)))
++  ps    |=([x=@rs n=@rs] ^-(@ `@`(~(pow rs:math [%n .1e-5]) x n)))
++  cbs   |=(x=@rs ^-(@ `@`(~(cbt rs:math [%n .1e-5]) x)))
::  ==== @rd ====
++  test-log2-8     (expect-eq !>(`@`0x4008.0000.0000.0000) !>((l2d `@rd`0x4020.0000.0000.0000)))
++  test-log10-1e3  (expect-eq !>(`@`0x4008.0000.0000.0000) !>((l10d `@rd`0x408f.4000.0000.0000)))
++  test-pow-2-half  (expect-eq !>(`@`0x3ff6.a09e.667f.3bcc) !>((pd `@rd`0x4000.0000.0000.0000 `@rd`0x3fe0.0000.0000.0000)))
++  test-pow-3-2h   (expect-eq !>(`@`0x402f.2d4a.4563.5643) !>((pd `@rd`0x4008.0000.0000.0000 `@rd`0x4004.0000.0000.0000)))
++  test-cbrt-27    (expect-eq !>(`@`0x4007.ffff.ffff.ffff) !>((cbd `@rd`0x403b.0000.0000.0000)))
++  test-cbrt-n8    (expect-eq !>(`@`0xbfff.ffff.ffff.ffff) !>((cbd `@rd`0xc020.0000.0000.0000)))
::  ==== @rs ====
++  test-log2-8-s     (expect-eq !>(`@`0x4040.0000) !>((l2s `@rs`0x4100.0000)))
++  test-log10-1e3-s  (expect-eq !>(`@`0x4040.0001) !>((l10s `@rs`0x447a.0000)))
++  test-pow-2-half-s  (expect-eq !>(`@`0x3fb5.04f3) !>((ps `@rs`0x4000.0000 `@rs`0x3f00.0000)))
++  test-cbrt-n8-s    (expect-eq !>(`@`0xc000.0000) !>((cbs `@rs`0xc100.0000)))
--
