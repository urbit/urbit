::  /tests/lib/complex-edge -- %cplx transcendental edge cases (@cs).
::
::  Exercises the origin, the real and imaginary axes, the csqrt/clog branch cut
::  (-1, +-i), the csqrt sign branch, the clog(0) singularity, and the naive-
::  series breakdown at large arguments.  All values validated against numpy
::  complex64.
::
/+  *test, complex
|%
++  s    cs:complex
++  z00  `@`0x0                    ::  0+0i
++  z10  `@`0x3f80.0000            ::  1+0i
++  zt0  `@`0x4120.0000            ::  10+0i
++  z0i  `@`0x3f80.0000.0000.0000  ::  0+1i
++  z0n  `@`0xbf80.0000.0000.0000  ::  0-1i
++  zn1  `@`0xbf80.0000            ::  -1+0i
++  z11  `@`0x3f80.0000.3f80.0000  ::  1+1i
++  z34  `@`0x4080.0000.4040.0000  ::  3+4i
++  z0t  `@`0x4120.0000.0000.0000  ::  0+10i
::  cexp: origin -> 1, real axis (e, 1/e), imaginary axis (cos1 + i sin1), off-axis.
++  test-cexp-origin  (expect-eq !>(`@`0x3f80.0000) !>((~(cexp s %n) z00)))
++  test-cexp-1       (expect-eq !>(`@`0x402d.f855) !>((~(cexp s %n) z10)))
++  test-cexp-neg1    (expect-eq !>(`@`0x3ebc.5ab0) !>((~(cexp s %n) zn1)))
++  test-cexp-i       (expect-eq !>(`@`0x3f57.6aa4.3f0a.5140) !>((~(cexp s %n) z0i)))
++  test-cexp-1p1i    (expect-eq !>(`@`0x4012.6408.3fbb.fe2a) !>((~(cexp s %n) z11)))
++  test-cexp-3p4i    (expect-eq !>(`@`0xc173.366f.c152.0f81) !>((~(cexp s %n) z34)))
::  clog: the singularity (log 0 = -inf) and the branch cut at -1 (iπ) and +-i.
++  test-clog-zero    (expect-eq !>(`@`0xff80.0000) !>((~(clog s %n) z00)))
++  test-clog-neg1    (expect-eq !>(`@`0x4049.0fdb.0000.0000) !>((~(clog s %n) zn1)))
++  test-clog-i       (expect-eq !>(`@`0x3fc9.0fdb.0000.0000) !>((~(clog s %n) z0i)))
++  test-clog-neg-i   (expect-eq !>(`@`0xbfc9.0fdb.0000.0000) !>((~(clog s %n) z0n)))
::  csqrt: branch cut sqrt(-1) = i, and the sign branch sqrt(-i) = 0.707 - 0.707i.
++  test-csqrt-neg1   (expect-eq !>(`@`0x3f80.0000.0000.0000) !>((~(csqrt s %n) zn1)))
++  test-csqrt-neg-i  (expect-eq !>(`@`0xbf35.04f3.3f35.04f3) !>((~(csqrt s %n) z0n)))
::  csin/ccos on the imaginary axis -> real sinh/cosh.
++  test-csin-i       (expect-eq !>(`@`0x3f96.6cff.0000.0000) !>((~(csin s %n) z0i)))
++  test-ccos-i       (expect-eq !>(`@`0x3fc5.83ab) !>((~(ccos s %n) z0i)))
::  KNOWN LIMITATION: the naive Taylor/AGM series diverges from the true value
::  far from the origin -- exp(10) is ~21991 here vs the true ~22026 (~0.16%
::  low), csin(0+10i) ~ i*10989 vs true ~ i*11013.  Locked as regression (NOT as
::  correctness); #18's Chebyshev rewrite would tighten these.
++  test-cexp-large   (expect-eq !>(`@`0x46ab.cef6) !>((~(cexp s %n) zt0)))
++  test-csin-large   (expect-eq !>(`@`0x462b.b42b.0000.0000) !>((~(csin s %n) z0t)))
::
::  Same edges across the other widths: @cd (double), @ch (half), @cq (quad).
::  Origin, the csqrt/clog branch cut at -1, the clog(0)=-inf singularity, the
::  csqrt sign branch at -i, and the imaginary axis (csin -> sinh).
++  d   cd:complex
++  h   ch:complex
++  q   cq:complex
++  d00  `@`0x0
++  dn1  `@`0xbff0.0000.0000.0000
++  d0i  `@`0x3ff0.0000.0000.0000.0000.0000.0000.0000
++  d0n  `@`0xbff0.0000.0000.0000.0000.0000.0000.0000
++  h00  `@`0x0
++  hn1  `@`0xbc00
++  h0i  `@`0x3c00.0000
++  h0n  `@`0xbc00.0000
++  q00  `@`0x0
++  qn1  `@`0xbfff.0000.0000.0000.0000.0000.0000.0000
++  test-cd-cexp-origin  (expect-eq !>(`@`0x3ff0.0000.0000.0000) !>((~(cexp d %n) d00)))
++  test-cd-csqrt-neg1   (expect-eq !>(`@`0x3ff0.0000.0000.0000.0000.0000.0000.0000) !>((~(csqrt d %n) dn1)))
++  test-cd-clog-neg1    (expect-eq !>(`@`0x4009.21fb.5444.2d11.0000.0000.0000.0000) !>((~(clog d %n) dn1)))
++  test-cd-clog-zero    (expect-eq !>(`@`0xfff0.0000.0000.0000) !>((~(clog d %n) d00)))
++  test-cd-csqrt-neg-i  (expect-eq !>(`@`0xbfe6.a09e.667f.3bcd.3fe6.a09e.667f.3bcd) !>((~(csqrt d %n) d0n)))
++  test-cd-csin-i       (expect-eq !>(`@`0x3ff2.cd9f.c44e.b983.0000.0000.0000.0000) !>((~(csin d %n) d0i)))
++  test-ch-cexp-origin  (expect-eq !>(`@`0x3c00) !>((~(cexp h %n) h00)))
++  test-ch-csqrt-neg1   (expect-eq !>(`@`0x3c00.0000) !>((~(csqrt h %n) hn1)))
++  test-ch-clog-zero    (expect-eq !>(`@`0xfc00) !>((~(clog h %n) h00)))
++  test-ch-csqrt-neg-i  (expect-eq !>(`@`0xb9a8.39a8) !>((~(csqrt h %n) h0n)))
++  test-ch-csin-i       (expect-eq !>(`@`0x3cb2.0000) !>((~(csin h %n) h0i)))
++  test-cq-cexp-origin  (expect-eq !>(`@`0x3fff.0000.0000.0000.0000.0000.0000.0000) !>((~(cexp q %n) q00)))
++  test-cq-csqrt-neg1
  (expect-eq !>(`@`0x3fff.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000) !>((~(csqrt q %n) qn1)))
++  test-cq-clog-zero    (expect-eq !>(`@`0xffff.0000.0000.0000.0000.0000.0000.0000) !>((~(clog q %n) q00)))
::
::  cpow edge cases (@cs): z^0 = 1, integer powers, the branch sqrt via ^0.5,
::  and the 0^w corner.
++  cs2   `@`0x4000.0000               ::  2+0i
++  csi   `@`0x3f80.0000.0000.0000     ::  0+1i
++  csn1  `@`0xbf80.0000               ::  -1+0i
++  cwh   `@`0x3f00.0000               ::  0.5+0i
++  test-cpow-pow0      (expect-eq !>(`@`0x3f80.0000) !>((~(cpow s %n) cs2 z00)))         ::  2^0 = 1
++  test-cpow-pow2      (expect-eq !>(`@`0x4080.0000) !>((~(cpow s %n) cs2 cs2)))         ::  2^2 = 4
++  test-cpow-i2        (expect-eq !>(`@`0xb182.92c0.bf80.0000) !>((~(cpow s %n) csi cs2)))   ::  i^2 = -1 (+~0i)
++  test-cpow-neg1-half  (expect-eq !>(`@`0x3f7f.ffff.b386.2919) !>((~(cpow s %n) csn1 cwh)))  ::  (-1)^0.5 = i
::  KNOWN LIMITATION: 0^2 should be 0, but cpow = exp(w*clog z) routes through
::  clog(0)=-inf and exp(-inf) (which the naive series cannot represent), so it
::  yields NaN.  Locked as a documented corner, not as correctness.
++  test-cpow-zero-pow2  (expect-eq !>(`@`0x7fc0.0000.7fc0.0000) !>((~(cpow s %n) z00 cs2)))
::
::  Rounding modes propagate through the complex ops: cexp(1+2i) gives four
::  distinct bit patterns under %n / %u / %d / %z.
++  z12  `@`0x4000.0000.3f80.0000      ::  1+2i
++  test-cexp-rnd-n  (expect-eq !>(`@`0x401e.30c5.bf90.cb4e) !>((~(cexp s %n) z12)))
++  test-cexp-rnd-u  (expect-eq !>(`@`0x401e.30d9.bf90.cb53) !>((~(cexp s %u) z12)))
++  test-cexp-rnd-d  (expect-eq !>(`@`0x401e.30bd.bf90.cb53) !>((~(cexp s %d) z12)))
++  test-cexp-rnd-z  (expect-eq !>(`@`0x401e.30bd.bf90.cb46) !>((~(cexp s %z) z12)))
--
