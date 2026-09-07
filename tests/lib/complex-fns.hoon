::  /tests/lib/complex-fns -- complex elementary functions.
::
::  Expected values are the lib's own self-contained series outputs, validated
::  against numpy (complex64 / complex128) to display precision, e.g.
::  cexp(1+2i) = -1.1312 + 2.4717i, clog = 0.8047 + 1.1071i, csqrt = 1.2720 +
::  0.7862i, csin = 3.1658 + 1.9596i, ccos = 2.0327 - 3.0519i, ctan = 0.0338 +
::  1.0148i, (1+2i)^2 = -3 + 4i.  Inputs: z = 1+2i at each width.
::
/+  *test, complex
|%
++  s   cs:complex
++  d   cd:complex
++  h   ch:complex
++  q   cq:complex
++  zs  `@`0x4000.0000.3f80.0000
++  zd  `@`0x4000.0000.0000.0000.3ff0.0000.0000.0000
++  zh  `@`0x4000.3c00
++  zq  `@`0x4000.0000.0000.0000.0000.0000.0000.0000.3fff.0000.0000.0000.0000.0000.0000.0000
::  @cs (single): the full set; cpow uses w = 2+0i, so (1+2i)^2 = -3+4i.
++  test-cs-cexp   (expect-eq !>(`@`0x401e.30c5.bf90.cb4e) !>((~(cexp s %n) zs)))
++  test-cs-clog   (expect-eq !>(`@`0x3f8d.b70a.3f4e.0210) !>((~(clog s %n) zs)))
++  test-cs-csqrt  (expect-eq !>(`@`0x3f49.4138.3fa2.d18a) !>((~(csqrt s %n) zs)))
++  test-cs-csin   (expect-eq !>(`@`0x3ffa.d435.404a.9c1e) !>((~(csin s %n) zs)))
++  test-cs-ccos   (expect-eq !>(`@`0xc043.524b.4002.1823) !>((~(ccos s %n) zs)))
++  test-cs-ctan   (expect-eq !>(`@`0x3f81.e4c1.3d0a.7f5c) !>((~(ctan s %n) zs)))
++  test-cs-cpow   (expect-eq !>(`@`0x4080.0004.c03f.fff5) !>((~(cpow s %n) zs `@`0x4000.0000)))
::  @cd (double): exp/log/sqrt -- matches numpy complex128 to ~15 digits.
++  test-cd-cexp   (expect-eq !>(`@`0x4003.c618.a227.4afe.bff2.1969.c495.3cd3) !>((~(cexp d %n) zd)))
++  test-cd-clog   (expect-eq !>(`@`0x3ff1.b6e1.92eb.be45.3fe9.c041.f7ed.8d33) !>((~(clog d %n) zd)))
++  test-cd-csqrt  (expect-eq !>(`@`0x3fe9.2826.ef25.8d1b.3ff4.5a31.46a8.8456) !>((~(csqrt d %n) zd)))
::  @ch (half) + @cq (quad): cexp spot check across the remaining widths.
++  test-ch-cexp   (expect-eq !>(`@`0x40f1.bc86) !>((~(cexp h %n) zh)))
++  test-cq-cexp
  (expect-eq !>(`@`0x4000.3c61.8a22.74af.d5ad.4589.2de9.748d.bfff.2196.9c49.53cd.175c.75fb.0c1e.697d) !>((~(cexp q %n) zq)))
--
