::  /tests/lib/unum-edge -- posit (%unum) transcendental edge cases.
::
::  Exact identities, sign, NaR propagation, domain -> NaR, and correctness at
::  large arguments (range-reduced exp/sin/log, see unum_cheb_check.py).
::  Mostly at posit32 (rps), with a posit8 (rpb) cross-width spot.  Posits
::  round-to-nearest-even, so there is no rounding-mode variation to sweep.
::
/+  *test, unum
|%
++  u    rps:unum
++  b    rpb:unum
++  nar  `@`0x8000.0000
++  s1   (sun:rps:unum 1)
++  s4   (sun:rps:unum 4)
++  s5   (sun:rps:unum 5)
++  sa   (sun:rps:unum 10)
++  sb   (sun:rps:unum 50)
++  sc   (sun:rps:unum 100)
++  n1   (neg:rps:unum (sun:rps:unum 1))
::  exact identities (correctly rounded; series accurate near 0)
++  test-exp0    (expect-eq !>(`@`0x4000.0000) !>((exp:u 0x0)))         ::  exp 0 = 1
++  test-cos0    (expect-eq !>(`@`0x4000.0000) !>((cos:u 0x0)))         ::  cos 0 = 1
++  test-sin0    (expect-eq !>(`@`0x0) !>((sin:u 0x0)))                 ::  sin 0 = 0
++  test-tan0    (expect-eq !>(`@`0x0) !>((tan:u 0x0)))
++  test-log1    (expect-eq !>(`@`0x0) !>((log:u s1)))                  ::  log 1 = 0
++  test-sqt0    (expect-eq !>(`@`0x0) !>((sqt:u 0x0)))
++  test-sqt1    (expect-eq !>(`@`0x4000.0000) !>((sqt:u s1)))
++  test-sqt4    (expect-eq !>(`@`0x4800.0000) !>((sqt:u s4)))          ::  sqrt 4 = 2
++  test-fact0   (expect-eq !>(`@`0x4000.0000) !>((factorial:u 0x0)))   ::  0! = 1
++  test-fact5   (expect-eq !>(`@`0x6b80.0000) !>((factorial:u s5)))    ::  5! = 120
++  test-atan0   (expect-eq !>(`@`0x0) !>((atan:u 0x0)))
++  test-asin0   (expect-eq !>(`@`0x0) !>((asin:u 0x0)))
++  test-acos1   (expect-eq !>(`@`0x0) !>((acos:u s1)))                 ::  acos 1 = 0
::  sign
++  test-exp-n1   (expect-eq !>(`@`0x33c5.ab1b) !>((exp:u n1)))         ::  1/e
++  test-atan-n1  (expect-eq !>(`@`0xc36f.0256) !>((atan:u n1)))        ::  -pi/4 (atan is odd)
::  NaR propagation
++  test-exp-nar   (expect-eq !>(`@`0x8000.0000) !>((exp:u nar)))
++  test-sin-nar   (expect-eq !>(`@`0x8000.0000) !>((sin:u nar)))
++  test-sqt-nar   (expect-eq !>(`@`0x8000.0000) !>((sqt:u nar)))
++  test-fact-nar  (expect-eq !>(`@`0x8000.0000) !>((factorial:u nar)))
::  domain -> NaR
++  test-sqt-neg  (expect-eq !>(`@`0x8000.0000) !>((sqt:u n1)))         ::  sqrt(-1) = NaR
::  posit8 cross-width spot: identity + NaR propagation (-1 @rpb = 0xc0)
++  test-rpb-exp0     (expect-eq !>(`@`0x40) !>((exp:b 0x0)))           ::  exp 0 = 1
++  test-rpb-exp-nar  (expect-eq !>(`@`0x80) !>((exp:b 0x80)))
++  test-rpb-sqt-neg  (expect-eq !>(`@`0x80) !>((sqt:b 0xc0)))          ::  sqrt(-1) = NaR
::  FIXED (numerics NEXT-STEPS.md item #4, the Chebyshev-basis rewrite): these
::  used to be a documented limitation -- the naive Taylor series diverges far
::  from the origin (exp(10) ~ 21991 vs true ~22026, exp(50)/exp(100) saturated
::  near maxpos instead of tracking the true value, sin(10) left [-1,1] with no
::  range reduction).  Range-reduced +exp/+sin are now correctly rounded at
::  posit32 across this whole range (verified vs mpmath, see
::  libmath/tools/unum_cheb_check.py) -- these are genuine correctness asserts
::  now, not regression locks on known-wrong output.
++  test-exp10   (expect-eq !>(`@`0x7a58.29dd) !>((exp:u sa)))          ::  e^10 ~ 22026.47
++  test-exp50   (expect-eq !>(`@`0x7fff.f032) !>((exp:u sb)))          ::  e^50, near maxpos
++  test-exp100  (expect-eq !>(`@`0x7fff.ffff) !>((exp:u sc)))          ::  e^100, saturates (correctly)
++  test-sin10   (expect-eq !>(`@`0xc74b.b085) !>((sin:u sa)))          ::  sin(10) ~ -0.5440
++  test-log100  (expect-eq !>(`@`0x5135.d8de) !>((log:u sc)))          ::  ln(100) ~ 4.6052
--
