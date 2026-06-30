/+  twoc
::::  /lib/fixed -- fixed-point arithmetic
::::  Version ~2024.1.15 by ~lagrev-nocfep and ~mopfel-winrux
::::  ~2026.6.1: rebuilt on /lib/twoc for correct signed mul/div/scale.
::
::  A fixed-point number is a two's-complement integer of N = a + b + 1 bits
::  interpreted as value / 2^b:
::
::    value = 1/2^b * ( -2^(N-1) x_(N-1) + sum_{n=0}^{N-2} 2^n x_n )
::
::  where a = integer bits, b = fractional bits, and the leading bit is the
::  sign.  (Yates2023, http://www.digitalsignallabs.com/downloads/fp.pdf)
::
::  Arithmetic is signed and MODULAR (wraps mod 2^N on overflow), delegating
::  to the width-keyed +twid door of /lib/twoc so the sign handling is shared
::  with the rest of numerics rather than reimplemented here.
::
|%
+$  prec  [a=@ b=@]   ::  fixed-point precision, a+b+1=bloq
::  +wid: total bit width N = a + b + 1 of a precision.
++  wid  |=(=prec ^-(@ :(^add a.prec b.prec 1)))
::  +ng: the /lib/twoc width-keyed door at a given precision's N.
++  ng   |=(=prec ~(. twid:twoc (wid prec)))
::    +add: [@ prec @ prec] -> @
::
::  Add two fixed-point numbers of equal precision (signed, wrapping).
::    Examples
::      > (add 0x180 [8 8] 0x240 [8 8])   :: 1.5 + 2.25
::      0x3c0                             :: 3.75
::  Source
++  add
  |=  [x=@ xprec=prec y=@ yprec=prec]
  ?>  =(xprec yprec)
  (add:(ng xprec) x y)
::    +sub: [@ prec @ prec] -> @
::
::  Subtract two fixed-point numbers of equal precision (signed, wrapping).
::    Examples
::      > (sub 0x100 [8 8] 0x200 [8 8])   :: 1.0 - 2.0
::      0x1.ff00                          :: -1.0 in N=17 bits
::  Source
++  sub
  |=  [x=@ xprec=prec y=@ yprec=prec]
  ?>  =(xprec yprec)
  (sub:(ng xprec) x y)
::    +mul: [@ prec @ prec] -> [@ prec]
::
::  Multiply two fixed-point numbers (signed).  The product precision widens:
::  integer bits a.x + a.y + 1, fractional bits b.x + b.y.
::    Examples
::      > (mul 0x180 [8 8] 0x200 [8 8])   :: 1.5 * 2.0
::      [0x3.0000 17 16]                  :: 3.0 in q17.16
::      > (mul (neg 0x180 [8 8]) [8 8] 0x200 [8 8])   :: -1.5 * 2.0
::      [0x3.fffd.0000 17 16]             :: -3.0
::  Source
++  mul
  |=  [x=@ xprec=prec y=@ yprec=prec]
  ^-  [@ prec]
  =/  prodp=prec  [:(^add a.xprec a.yprec 1) (^add b.xprec b.yprec)]
  ::  re-encode each operand at the product width, then signed multiply.
  =/  px  (s-to-twoc:(ng prodp) (to-s x xprec))
  =/  py  (s-to-twoc:(ng prodp) (to-s y yprec))
  [(mul:(ng prodp) px py) prodp]
::    +div: [@ prec @ prec] -> [@ prec]
::
::  Divide two fixed-point numbers of equal precision (signed), keeping the
::  input precision.  Division by zero crashes.
::    Examples
::      > (div (neg 0x300 [8 8]) [8 8] 0x200 [8 8])   :: -3.0 / 2.0
::      [0x1.fe80 8 8]                                :: -1.5
::  Source
++  div
  |=  [x=@ xprec=prec y=@ yprec=prec]
  ^-  [@ prec]
  ?>  ~|(%argument-precision-must-match =(xprec yprec))
  ::  scale the dividend up by 2^b before the signed divide so the quotient
  ::  lands back in xprec's fixed-point scale: (x * 2^b) / y.
  =/  sx   (to-s x xprec)
  =/  num  (new:si (syn:si sx) (lsh [0 b.xprec] (abs:si sx)))
  =/  qa   (fra:si num (to-s y yprec))
  [(s-to-twoc:(ng xprec) qa) xprec]
::    +mod: [@ prec @ prec] -> @
::
::  Signed remainder a - b*trunc(a/b), keeping the input precision.  The sign
::  follows the dividend (truncated division), via /lib/twoc's +rem.  Both
::  operands share precision, so the 2^b scale cancels in the remainder.
::    Examples
::      > (mod 0x380 [8 8] 0x200 [8 8])   :: 3.5 mod 2.0
::      0x180                             :: 1.5
::  Source
++  mod
  |=  [x=@ xprec=prec y=@ yprec=prec]
  ?>  =(xprec yprec)
  (rem:(ng xprec) x y)
::    +abs: [@ prec] -> @
::
::  Absolute value at the number's own width (abs of the most-negative value
::  wraps back to itself, per two's-complement).
::    Examples
::      > (abs (neg 0x180 [8 8]) [8 8])   :: |-1.5|
::      0x180
::  Source
++  abs
  |=  [x=@ =prec]
  ^-  @
  (abs:(ng prec) x)
::    +gth/+gte/+lth/+lte/+equ/+neq: [@ prec @ prec] -> ?
::
::  Signed comparisons of two equal-precision fixed-point numbers.  Because
::  both are stored at the same 2^b scale, comparing the two's-complement
::  values (via /lib/twoc) is order-preserving on the represented reals.
::  +equ is bit equality (two's-complement has no negative zero).
::  Source
++  gth
  |=  [x=@ xprec=prec y=@ yprec=prec]
  ?>  =(xprec yprec)
  (gth:(ng xprec) x y)
++  gte
  |=  [x=@ xprec=prec y=@ yprec=prec]
  ?>  =(xprec yprec)
  (gte:(ng xprec) x y)
++  lth
  |=  [x=@ xprec=prec y=@ yprec=prec]
  ?>  =(xprec yprec)
  (lth:(ng xprec) x y)
++  lte
  |=  [x=@ xprec=prec y=@ yprec=prec]
  ?>  =(xprec yprec)
  (lte:(ng xprec) x y)
++  equ
  |=  [x=@ xprec=prec y=@ yprec=prec]
  ?>  =(xprec yprec)
  .=(x y)
++  neq
  |=  [x=@ xprec=prec y=@ yprec=prec]
  ?>  =(xprec yprec)
  !.=(x y)
::    +to-s: [@ prec] -> @s
::
::  Decode a fixed-point bit pattern to a hoon signed integer (the stored
::  two's-complement value, fractional bits included).
::  Source
++  to-s
  |=  [x=@ =prec]
  ^-  @s
  (twoc-to-s:(ng prec) x)
::    +from-s: [@s prec] -> @
::
::  Encode a (whole) signed integer as a fixed-point number at the given
::  precision: stored value = n * 2^b.  Out-of-range n crashes (s-to-twoc).
::    Examples
::      > (from-s --3 [8 8])   :: 3.0
::      0x300
::      > (from-s -2 [8 8])    :: -2.0
::      0x1.fe00
::  Source
++  from-s
  |=  [n=@s =prec]
  ^-  @
  (s-to-twoc:(ng prec) (new:si (syn:si n) (lsh [0 b.prec] (abs:si n))))
::    +from-rs: [@rs prec] -> @
::
::  Quantize a single-precision IEEE float to fixed-point, rounding the
::  scaled value f * 2^b to the nearest integer (ties to even).  Out-of-range
::  results wrap (s-to-twoc).  Use this to construct fractional constants.
::    Examples
::      > (from-rs .1.5 [8 8])
::      0x180
::  Source
++  from-rs
  |=  [f=@rs =prec]
  ^-  @
  =/  i=@s  (need (~(toi rs %n) (~(mul rs %n) f (~(sun rs %n) (bex b.prec)))))
  (s-to-twoc:(ng prec) i)
::    +to-rs: [@ prec] -> @rs
::
::  Decode a fixed-point number to a single-precision IEEE float: value =
::  stored / 2^b.  Rounded to nearest (ties to even) where not exact.
::    Examples
::      > (to-rs 0x180 [8 8])
::      .1.5
::  Source
++  to-rs
  |=  [x=@ =prec]
  ^-  @rs
  =/  s=@s  (to-s x prec)
  =/  fs=@rs
    ?:  (syn:si s)
      (~(sun rs %n) (abs:si s))
    (~(mul rs %n) .-1 (~(sun rs %n) (abs:si s)))
  (~(div rs %n) fs (~(sun rs %n) (bex b.prec)))
::    +neg: [@ prec] -> @
::
::  Two's-complement negation of a fixed-point number at its own width.
::  (Renamed from the old +twoc, which shadowed the /lib/twoc import face
::  and duplicated a name; behavior unchanged: -x.)
::    Examples
::      > (neg 0x100 [8 8])   :: -(1.0)
::      0x1.ff00
::  Source
++  neg
  |=  [x=@ =prec]
  ^-  @
  (neg:(ng prec) x)
::    +hi: [@ prec @] -> @   (high n bits)
::  Source
++  hi
  |=  [x=@ =prec n=@]
  ?>  (^lte n (wid prec))
  (rsh [0 (^sub (wid prec) n)] x)
::    +lo: [@ prec @] -> @   (low n bits)
::  Source
++  lo
  |=  [x=@ =prec n=@]
  ?>  (^lte n (wid prec))
  (dis x (dec (bex n)))
::    +scale: [@ prec prec] -> @
::
::  Re-quantize x from xprec to yprec (signed): shift the fractional part to
::  the new b, then re-encode at the new width.  Saturation/rounding are not
::  applied; high bits beyond yprec's range are dropped (wrap).
::    Examples
::      > (scale 0x10 [4 4] [8 8])   :: 1.0 in q4.4 -> q8.8
::      0x100
::      > (scale 0x100 [8 8] [4 4])  :: 1.0 in q8.8 -> q4.4
::      0x10
::  Source
++  scale
  |=  [x=@ xprec=prec yprec=prec]
  ^-  @
  ::  decode to the signed value, rescale the fractional precision, re-encode.
  =/  sx  (to-s x xprec)
  =/  sg  (syn:si sx)
  =/  mg  (abs:si sx)
  =/  mg2  ?:  (^gth b.yprec b.xprec)
             (lsh [0 (^sub b.yprec b.xprec)] mg)
           (rsh [0 (^sub b.xprec b.yprec)] mg)
  (s-to-twoc:(ng yprec) (new:si sg mg2))
--
