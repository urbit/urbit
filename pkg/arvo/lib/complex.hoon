::::  /lib/complex -- complex-number arithmetic over IEEE-754 component floats
::::  ~2026.6.5
::
::  A complex value is a packed atom with the REAL component in the low
::  2^(bloq-1) bits and the IMAGINARY component in the high bits.  This matches
::  SoftBLAS `complexN_t { real; imag }` over little-endian bytes, so an array
::  of these packed elements IS a BLAS interleaved complex array (re0, im0,
::  re1, im1, ...) and a future jet can cast the data straight to complexN_t*.
::
::  Aura family @c (complex), parallel to @r (real); each @c? is a packed pair
::  of the corresponding @r? component float:
::
::    @ch  complex-half    = 2 x @rh (16b)  = 32-bit element
::    @cs  complex-single  = 2 x @rs (32b)  = 64-bit element   (BLAS  c )
::    @cd  complex-double  = 2 x @rd (64b)  = 128-bit element  (BLAS  z )
::    @cq  complex-quad    = 2 x @rq (128b) = 256-bit element
::
::  Each width is a door keyed on the rounding mode; arms take and return
::  PACKED complex atoms.  @cs and @cd ship first (BLAS c/z); @ch/@cq are
::  additive copies later.  Complex has no total order, so there is
::  intentionally no gth/gte/lth/lte -- only equ/neq.
::
::  Complex elementary functions (cexp/clog/csqrt/csin/ccos/ctan/cpow) compose
::  real component transcendentals.  To keep this lib self-contained -- like
::  /lib/unum, /lib/fixed, /lib/twoc, with no dependency on /lib/math -- each
::  width door carries its OWN real exp/sin/cos/log/atan as naive Taylor/AGM
::  series over the stdlib component float door, at a fixed term count (the same
::  approach as /lib/math and /lib/unum; #18's Chebyshev rewrite would upgrade
::  those in lockstep).  Closed forms:
::    cexp(a+bi) = e^a (cos b + i sin b)
::    clog(a+bi) = log|z| + i atan2(b, a)
::    csqrt(z)   = sqrt((|z|+a)/2) + i sgn(b) sqrt((|z|-a)/2)
::    csin/ccos via real sin/cos and cosh/sinh; ctan = csin/ccos; cpow = cexp(w clog z)
::
|%
+$  rounding-mode  ?(%n %u %d %z)
::    +cs:  complex-single (@cs), two @rs (32-bit) components.
::
::  Door keyed on the rounding mode.  In the examples below 1+2i packs to
::  0x4000.0000.3f80.0000 and 3+4i to 0x4080.0000.4040.0000 (real in the low
::  32 bits).
::
++  cs
  |_  rnd=rounding-mode
  ::
  ::  Components
  ::
  ::    +re:  @cs -> @rs
  ::
  ::  Returns the real component (low 32 bits) of a packed @cs value.
  ::    Examples
  ::      > (~(re cs:complex %n) 0x4000.0000.3f80.0000)  ::  re(1+2i)
  ::      .1
  ::  Source
  ++  re    |=(p=@ `@rs`(end [0 32] p))
  ::    +im:  @cs -> @rs
  ::
  ::  Returns the imaginary component (high 32 bits) of a packed @cs value.
  ::    Examples
  ::      > (~(im cs:complex %n) 0x4000.0000.3f80.0000)  ::  im(1+2i)
  ::      .2
  ::  Source
  ++  im    |=(p=@ `@rs`(rsh [0 32] p))
  ::    +pak:  [@rs @rs] -> @cs
  ::
  ::  Packs real and imaginary @rs components into one @cs atom (real low).
  ::    Examples
  ::      > (~(pak cs:complex %n) .1 .2)
  ::      0x4000.0000.3f80.0000
  ::  Source
  ++  pak   |=([r=@rs i=@rs] ^-(@ (con `@`r (lsh [0 32] `@`i))))
  ::  component helpers (internal)
  ++  fneg  |=(x=@rs (~(sub rs rnd) .0 x))
  ++  fabs  |=(x=@rs ?:((~(lth rs rnd) x .0) (~(sub rs rnd) .0 x) x))
  ::
  ::  Constants
  ::
  ::    +zero:  @cs
  ::
  ::  The additive identity 0+0i.
  ::    Examples
  ::      > ~(zero cs:complex %n)
  ::      0x0
  ::  Source
  ++  zero  ^-(@ 0)
  ::    +one:  @cs
  ::
  ::  The multiplicative identity 1+0i.
  ::    Examples
  ::      > ~(one cs:complex %n)
  ::      0x3f80.0000
  ::  Source
  ++  one   (pak .1 .0)
  ::
  ::  Arithmetic
  ::
  ::    +add:  [@cs @cs] -> @cs
  ::
  ::  Returns the sum of two complex values (component-wise).
  ::    Examples
  ::      > (~(add cs:complex %n) 0x4000.0000.3f80.0000 0x4080.0000.4040.0000)
  ::      0x40c0.0000.4080.0000
  ::  Source
  ++  add   |=([p=@ q=@] (pak (~(add rs rnd) (re p) (re q)) (~(add rs rnd) (im p) (im q))))
  ::    +sub:  [@cs @cs] -> @cs
  ::
  ::  Returns the difference of two complex values (component-wise).
  ::    Examples
  ::      > (~(sub cs:complex %n) 0x4000.0000.3f80.0000 0x4080.0000.4040.0000)
  ::      0xc000.0000.c000.0000
  ::  Source
  ++  sub   |=([p=@ q=@] (pak (~(sub rs rnd) (re p) (re q)) (~(sub rs rnd) (im p) (im q))))
  ::    +neg:  @cs -> @cs
  ::
  ::  Returns the additive inverse -z.
  ::    Examples
  ::      > (~(neg cs:complex %n) 0x4000.0000.3f80.0000)  ::  -(1+2i)
  ::      0xc000.0000.bf80.0000
  ::  Source
  ++  neg   |=(p=@ (pak (fneg (re p)) (fneg (im p))))
  ::    +conj:  @cs -> @cs
  ::
  ::  Returns the complex conjugate (negate the imaginary component).
  ::    Examples
  ::      > (~(conj cs:complex %n) 0x4000.0000.3f80.0000)  ::  conj(1+2i)=1-2i
  ::      0xc000.0000.3f80.0000
  ::  Source
  ++  conj  |=(p=@ (pak (re p) (fneg (im p))))
  ::    +mul:  [@cs @cs] -> @cs
  ::
  ::  Returns the complex product (ar+ai i)(br+bi i), rounded per component.
  ::    Examples
  ::      > (~(mul cs:complex %n) 0x4000.0000.3f80.0000 0x4080.0000.4040.0000)
  ::      0x4120.0000.c0a0.0000                                ::  (1+2i)(3+4i)=-5+10i
  ::  Source
  ++  mul
    |=  [p=@ q=@]
    =/  ar  (re p)  =/  ai  (im p)
    =/  br  (re q)  =/  bi  (im q)
    %+  pak
      (~(sub rs rnd) (~(mul rs rnd) ar br) (~(mul rs rnd) ai bi))
    (~(add rs rnd) (~(mul rs rnd) ar bi) (~(mul rs rnd) ai br))
  ::    +div:  [@cs @cs] -> @cs
  ::
  ::  Returns the complex quotient via Smith's algorithm (scale by the larger
  ::  denominator component for numerical stability).
  ::    Examples
  ::      > (~(div cs:complex %n) 0x4000.0000.4000.0000 0x3f80.0000.3f80.0000)
  ::      0x4000.0000                                          ::  (2+2i)/(1+1i)=2
  ::  Source
  ++  div
    |=  [p=@ q=@]
    =/  ar  (re p)  =/  ai  (im p)
    =/  br  (re q)  =/  bi  (im q)
    ?:  (~(gte rs rnd) (fabs br) (fabs bi))
      =/  r   (~(div rs rnd) bi br)
      =/  dn  (~(add rs rnd) br (~(mul rs rnd) bi r))
      %+  pak
        (~(div rs rnd) (~(add rs rnd) ar (~(mul rs rnd) ai r)) dn)
      (~(div rs rnd) (~(sub rs rnd) ai (~(mul rs rnd) ar r)) dn)
    =/  r   (~(div rs rnd) br bi)
    =/  dn  (~(add rs rnd) (~(mul rs rnd) br r) bi)
    %+  pak
      (~(div rs rnd) (~(add rs rnd) (~(mul rs rnd) ar r) ai) dn)
    (~(div rs rnd) (~(sub rs rnd) (~(mul rs rnd) ai r) ar) dn)
  ::    +abs:  @cs -> @cs
  ::
  ::  Returns the modulus |z| as a real-valued complex [|z|, 0], computed via
  ::  hypot (scale by the larger component to avoid overflow).
  ::    Examples
  ::      > (~(abs cs:complex %n) 0x4080.0000.4040.0000)       ::  |3+4i|=5
  ::      0x40a0.0000
  ::  Source
  ++  abs
    |=  p=@
    =/  xr  (fabs (re p))  =/  xi  (fabs (im p))
    ?:  &(=(`@rs`.0 xr) =(`@rs`.0 xi))  (pak .0 .0)
    ?:  (~(gte rs rnd) xr xi)
      =/  t  (~(div rs rnd) xi xr)
      (pak (~(mul rs rnd) xr (~(sqt rs rnd) (~(add rs rnd) .1 (~(mul rs rnd) t t)))) .0)
    =/  t  (~(div rs rnd) xr xi)
    (pak (~(mul rs rnd) xi (~(sqt rs rnd) (~(add rs rnd) .1 (~(mul rs rnd) t t)))) .0)
  ::
  ::  Comparison (no total order on complex -- only equality)
  ::
  ::    +equ:  [@cs @cs] -> ?
  ::
  ::  Loobean: bit-equality of both components (matches the %i754 convention).
  ::    Examples
  ::      > (~(equ cs:complex %n) 0x4000.0000.3f80.0000 0x4000.0000.3f80.0000)
  ::      %.y
  ::      > (~(equ cs:complex %n) 0x4000.0000.3f80.0000 0x4080.0000.4040.0000)
  ::      %.n
  ::  Source
  ++  equ   |=([p=@ q=@] &(=((re p) (re q)) =((im p) (im q))))
  ::    +neq:  [@cs @cs] -> ?
  ::
  ::  Loobean negation of +equ.
  ::    Examples
  ::      > (~(neq cs:complex %n) 0x4000.0000.3f80.0000 0x4080.0000.4040.0000)
  ::      %.y
  ::  Source
  ++  neq   |=([p=@ q=@] =(| (equ p q)))
  ::
  ::  Transcendental / elementary functions.  Self-contained real @rs series
  ::  (naive Taylor/AGM, fixed term counts -- accurate near the expansion point,
  ::  matching /lib/math and /lib/unum); component arithmetic stays on stdlib +rs.
  ::
  ++  rpi   `@rs`.3.1415927                  :: pi at @rs
  ++  rexp
    |=  x=@rs  ^-  @rs
    =/  s  .1  =/  t  .1  =/  n=@  1
    |-  ?:  (gth n 20)  s
    =.  t  (~(mul rs rnd) t (~(div rs rnd) x (~(sun rs rnd) n)))
    $(n +(n), s (~(add rs rnd) s t))
  ++  rsin
    |=  x=@rs  ^-  @rs
    =/  t  x  =/  s  x  =/  n=@  1
    |-  ?:  (gth n 20)  s
    =/  k  (^mul 2 n)
    =.  t  (~(sub rs rnd) .0 (~(mul rs rnd) t (~(div rs rnd) (~(mul rs rnd) x x) (~(mul rs rnd) (~(sun rs rnd) k) (~(sun rs rnd) +(k))))))
    $(n +(n), s (~(add rs rnd) s t))
  ++  rcos
    |=  x=@rs  ^-  @rs
    =/  t  .1  =/  s  .1  =/  n=@  1
    |-  ?:  (gth n 20)  s
    =/  k  (^mul 2 n)
    =.  t  (~(sub rs rnd) .0 (~(mul rs rnd) t (~(div rs rnd) (~(mul rs rnd) x x) (~(mul rs rnd) (~(sun rs rnd) (dec k)) (~(sun rs rnd) k)))))
    $(n +(n), s (~(add rs rnd) s t))
  ++  rlog
    |=  x=@rs  ^-  @rs
    ?:  =(`@rs`.0 x)  `@rs`0xff80.0000        :: log 0 = -inf
    =/  y   (~(div rs rnd) (~(sub rs rnd) x .1) (~(add rs rnd) x .1))
    =/  y2  (~(mul rs rnd) y y)  =/  s  y  =/  t  y  =/  n=@  1
    |-  ?:  (gth n 30)  (~(mul rs rnd) .2 s)
    =.  t  (~(mul rs rnd) t y2)
    =/  c  (~(div rs rnd) .1 (~(sun rs rnd) +((^mul 2 n))))
    $(n +(n), s (~(add rs rnd) s (~(mul rs rnd) c t)))
  ++  ratan                                  :: single-arg arctan (Gauss AGM)
    |=  x=@rs  ^-  @rs
    =/  rt  (~(sqt rs rnd) (~(add rs rnd) .1 (~(mul rs rnd) x x)))
    =/  a  (~(div rs rnd) .1 rt)  =/  b  .1  =/  n=@  0
    |-  ?:  (gth n 40)  (~(div rs rnd) x (~(mul rs rnd) rt b))
    =/  ai  (~(mul rs rnd) .0.5 (~(add rs rnd) a b))
    $(n +(n), a ai, b (~(sqt rs rnd) (~(mul rs rnd) ai b)))
  ++  ratn                                   :: atan2(y, x)
    |=  [y=@rs x=@rs]  ^-  @rs
    ?:  (~(gth rs rnd) x .0)                          (ratan (~(div rs rnd) y x))
    ?:  &((~(lth rs rnd) x .0) (~(gte rs rnd) y .0))  (~(add rs rnd) (ratan (~(div rs rnd) y x)) rpi)
    ?:  &((~(lth rs rnd) x .0) (~(lth rs rnd) y .0))  (~(sub rs rnd) (ratan (~(div rs rnd) y x)) rpi)
    ?:  (~(gth rs rnd) y .0)                          (~(div rs rnd) rpi .2)
    ?:  (~(lth rs rnd) y .0)                          (~(sub rs rnd) .0 (~(div rs rnd) rpi .2))
    .0
  ::    +cexp:  @cs -> @cs
  ::
  ::  Complex exponential e^z = e^a (cos b + i sin b) for z = a + bi.
  ::    Examples
  ::      > (~(cexp cs:complex %n) 0x4000.0000.3f80.0000)  ::  exp(1+2i)
  ::      0x401e.30c5.bf90.cb4e
  ::  Source
  ++  cexp
    |=  p=@
    ^-  @
    =/  ea  (rexp (re p))
    (pak (~(mul rs rnd) ea (rcos (im p))) (~(mul rs rnd) ea (rsin (im p))))
  ::    +clog:  @cs -> @cs
  ::
  ::  Principal complex logarithm ln(z) = ln|z| + i*atan2(b, a).
  ::  Source
  ++  clog
    |=  p=@
    ^-  @
    =/  a  (re p)
    =/  b  (im p)
    =/  mag  (~(sqt rs rnd) (~(add rs rnd) (~(mul rs rnd) a a) (~(mul rs rnd) b b)))
    (pak (rlog mag) (ratn b a))
  ::    +csqrt:  @cs -> @cs
  ::
  ::  Principal complex square root: sqrt((|z|+a)/2) + i*sgn(b)*sqrt((|z|-a)/2),
  ::  with the imaginary sign taken from b (the principal branch).
  ::  Source
  ++  csqrt
    |=  p=@
    ^-  @
    =/  a  (re p)
    =/  b  (im p)
    =/  mag  (~(sqt rs rnd) (~(add rs rnd) (~(mul rs rnd) a a) (~(mul rs rnd) b b)))
    =/  re-  (~(sqt rs rnd) (~(div rs rnd) (~(add rs rnd) mag a) .2))
    =/  im-  (~(sqt rs rnd) (~(div rs rnd) (~(sub rs rnd) mag a) .2))
    (pak re- ?:((~(lth rs rnd) b .0) (fneg im-) im-))
  ::    +csin:  @cs -> @cs
  ::
  ::  Complex sine sin(z) = sin(a)cosh(b) + i cos(a)sinh(b), with cosh/sinh built
  ::  from the real exponential.
  ::  Source
  ++  csin
    |=  p=@
    ^-  @
    =/  a  (re p)
    =/  b  (im p)
    =/  eb   (rexp b)
    =/  enb  (rexp (fneg b))
    =/  csh  (~(div rs rnd) (~(add rs rnd) eb enb) .2)
    =/  snh  (~(div rs rnd) (~(sub rs rnd) eb enb) .2)
    (pak (~(mul rs rnd) (rsin a) csh) (~(mul rs rnd) (rcos a) snh))
  ::    +ccos:  @cs -> @cs
  ::
  ::  Complex cosine cos(z) = cos(a)cosh(b) - i sin(a)sinh(b).
  ::  Source
  ++  ccos
    |=  p=@
    ^-  @
    =/  a  (re p)
    =/  b  (im p)
    =/  eb   (rexp b)
    =/  enb  (rexp (fneg b))
    =/  csh  (~(div rs rnd) (~(add rs rnd) eb enb) .2)
    =/  snh  (~(div rs rnd) (~(sub rs rnd) eb enb) .2)
    (pak (~(mul rs rnd) (rcos a) csh) (fneg (~(mul rs rnd) (rsin a) snh)))
  ::    +ctan:  @cs -> @cs
  ::
  ::  Complex tangent tan(z) = sin(z) / cos(z).
  ::  Source
  ++  ctan  |=(p=@ (div (csin p) (ccos p)))
  ::    +cpow:  [@cs @cs] -> @cs
  ::
  ::  Complex power z^w = exp(w * ln z) (principal branch).
  ::  Source
  ++  cpow  |=([p=@ q=@] (cexp (mul q (clog p))))
  --
::    +cd:  complex-double (@cd), two @rd (64-bit) components.
::
::  The @rd/@cd analogue of +cs over double-precision components.  Here 1+2i
::  packs to 0x4000.0000.0000.0000.3ff0.0000.0000.0000 and 3+4i to
::  0x4010.0000.0000.0000.4008.0000.0000.0000 (real in the low 64 bits).
::
++  cd
  |_  rnd=rounding-mode
  ::
  ::  Components
  ::
  ::    +re:  @cd -> @rd
  ::
  ::  Returns the real component (low 64 bits) of a packed @cd value.
  ::    Examples
  ::      > (~(re cd:complex %n) 0x4000.0000.0000.0000.3ff0.0000.0000.0000)  ::  re(1+2i)
  ::      .~1
  ::  Source
  ++  re    |=(p=@ `@rd`(end [0 64] p))
  ::    +im:  @cd -> @rd
  ::
  ::  Returns the imaginary component (high 64 bits) of a packed @cd value.
  ::    Examples
  ::      > (~(im cd:complex %n) 0x4000.0000.0000.0000.3ff0.0000.0000.0000)  ::  im(1+2i)
  ::      .~2
  ::  Source
  ++  im    |=(p=@ `@rd`(rsh [0 64] p))
  ::    +pak:  [@rd @rd] -> @cd
  ::
  ::  Packs real and imaginary @rd components into one @cd atom (real low).
  ::    Examples
  ::      > (~(pak cd:complex %n) .~1 .~2)
  ::      0x4000.0000.0000.0000.3ff0.0000.0000.0000
  ::  Source
  ++  pak   |=([r=@rd i=@rd] ^-(@ (con `@`r (lsh [0 64] `@`i))))
  ::  component helpers (internal)
  ++  fneg  |=(x=@rd (~(sub rd rnd) .~0 x))
  ++  fabs  |=(x=@rd ?:((~(lth rd rnd) x .~0) (~(sub rd rnd) .~0 x) x))
  ::
  ::  Constants
  ::
  ::    +zero:  @cd
  ::
  ::  The additive identity 0+0i.
  ::    Examples
  ::      > ~(zero cd:complex %n)
  ::      0x0
  ::  Source
  ++  zero  ^-(@ 0)
  ::    +one:  @cd
  ::
  ::  The multiplicative identity 1+0i.
  ::    Examples
  ::      > ~(one cd:complex %n)
  ::      0x3ff0.0000.0000.0000
  ::  Source
  ++  one   (pak .~1 .~0)
  ::
  ::  Arithmetic
  ::
  ::    +add:  [@cd @cd] -> @cd
  ::
  ::  Returns the sum of two complex values (component-wise).
  ::    Examples
  ::      > (~(add cd:complex %n) 0x4000.0000.0000.0000.3ff0.0000.0000.0000 0x4010.0000.0000.0000.4008.0000.0000.0000)
  ::      0x4018.0000.0000.0000.4010.0000.0000.0000
  ::  Source
  ++  add   |=([p=@ q=@] (pak (~(add rd rnd) (re p) (re q)) (~(add rd rnd) (im p) (im q))))
  ::    +sub:  [@cd @cd] -> @cd
  ::
  ::  Returns the difference of two complex values (component-wise).
  ::    Examples
  ::      > (~(sub cd:complex %n) 0x4000.0000.0000.0000.3ff0.0000.0000.0000 0x4010.0000.0000.0000.4008.0000.0000.0000)
  ::      0xc000.0000.0000.0000.c000.0000.0000.0000
  ::  Source
  ++  sub   |=([p=@ q=@] (pak (~(sub rd rnd) (re p) (re q)) (~(sub rd rnd) (im p) (im q))))
  ::    +neg:  @cd -> @cd
  ::
  ::  Returns the additive inverse -z.
  ::    Examples
  ::      > (~(neg cd:complex %n) 0x4000.0000.0000.0000.3ff0.0000.0000.0000)  ::  -(1+2i)
  ::      0xc000.0000.0000.0000.bff0.0000.0000.0000
  ::  Source
  ++  neg   |=(p=@ (pak (fneg (re p)) (fneg (im p))))
  ::    +conj:  @cd -> @cd
  ::
  ::  Returns the complex conjugate (negate the imaginary component).
  ::    Examples
  ::      > (~(conj cd:complex %n) 0x4000.0000.0000.0000.3ff0.0000.0000.0000)  ::  conj(1+2i)=1-2i
  ::      0xc000.0000.0000.0000.3ff0.0000.0000.0000
  ::  Source
  ++  conj  |=(p=@ (pak (re p) (fneg (im p))))
  ::    +mul:  [@cd @cd] -> @cd
  ::
  ::  Returns the complex product (ar+ai i)(br+bi i), rounded per component.
  ::    Examples
  ::      > (~(mul cd:complex %n) 0x4000.0000.0000.0000.3ff0.0000.0000.0000 0x4010.0000.0000.0000.4008.0000.0000.0000)
  ::      0x4024.0000.0000.0000.c014.0000.0000.0000               ::  (1+2i)(3+4i)=-5+10i
  ::  Source
  ++  mul
    |=  [p=@ q=@]
    =/  ar  (re p)  =/  ai  (im p)
    =/  br  (re q)  =/  bi  (im q)
    %+  pak
      (~(sub rd rnd) (~(mul rd rnd) ar br) (~(mul rd rnd) ai bi))
    (~(add rd rnd) (~(mul rd rnd) ar bi) (~(mul rd rnd) ai br))
  ::    +div:  [@cd @cd] -> @cd
  ::
  ::  Returns the complex quotient via Smith's algorithm (scale by the larger
  ::  denominator component for numerical stability).
  ::    Examples
  ::      > (~(div cd:complex %n) 0x4000.0000.0000.0000.4000.0000.0000.0000 0x3ff0.0000.0000.0000.3ff0.0000.0000.0000)
  ::      0x4000.0000.0000.0000                                   ::  (2+2i)/(1+1i)=2
  ::  Source
  ++  div
    |=  [p=@ q=@]
    =/  ar  (re p)  =/  ai  (im p)
    =/  br  (re q)  =/  bi  (im q)
    ?:  (~(gte rd rnd) (fabs br) (fabs bi))
      =/  r   (~(div rd rnd) bi br)
      =/  dn  (~(add rd rnd) br (~(mul rd rnd) bi r))
      %+  pak
        (~(div rd rnd) (~(add rd rnd) ar (~(mul rd rnd) ai r)) dn)
      (~(div rd rnd) (~(sub rd rnd) ai (~(mul rd rnd) ar r)) dn)
    =/  r   (~(div rd rnd) br bi)
    =/  dn  (~(add rd rnd) (~(mul rd rnd) br r) bi)
    %+  pak
      (~(div rd rnd) (~(add rd rnd) (~(mul rd rnd) ar r) ai) dn)
    (~(div rd rnd) (~(sub rd rnd) (~(mul rd rnd) ai r) ar) dn)
  ::    +abs:  @cd -> @cd
  ::
  ::  Returns the modulus |z| as a real-valued complex [|z|, 0], computed via
  ::  hypot (scale by the larger component to avoid overflow).
  ::    Examples
  ::      > (~(abs cd:complex %n) 0x4010.0000.0000.0000.4008.0000.0000.0000)  ::  |3+4i|=5
  ::      0x4014.0000.0000.0000
  ::  Source
  ++  abs
    |=  p=@
    =/  xr  (fabs (re p))  =/  xi  (fabs (im p))
    ?:  &(=(`@rd`.~0 xr) =(`@rd`.~0 xi))  (pak .~0 .~0)
    ?:  (~(gte rd rnd) xr xi)
      =/  t  (~(div rd rnd) xi xr)
      (pak (~(mul rd rnd) xr (~(sqt rd rnd) (~(add rd rnd) .~1 (~(mul rd rnd) t t)))) .~0)
    =/  t  (~(div rd rnd) xr xi)
    (pak (~(mul rd rnd) xi (~(sqt rd rnd) (~(add rd rnd) .~1 (~(mul rd rnd) t t)))) .~0)
  ::
  ::  Comparison (no total order on complex -- only equality)
  ::
  ::    +equ:  [@cd @cd] -> ?
  ::
  ::  Loobean: bit-equality of both components (matches the %i754 convention).
  ::    Examples
  ::      > (~(equ cd:complex %n) 0x4000.0000.0000.0000.3ff0.0000.0000.0000 0x4000.0000.0000.0000.3ff0.0000.0000.0000)
  ::      %.y
  ::      > (~(equ cd:complex %n) 0x4000.0000.0000.0000.3ff0.0000.0000.0000 0x4010.0000.0000.0000.4008.0000.0000.0000)
  ::      %.n
  ::  Source
  ++  equ   |=([p=@ q=@] &(=((re p) (re q)) =((im p) (im q))))
  ::    +neq:  [@cd @cd] -> ?
  ::
  ::  Loobean negation of +equ.
  ::    Examples
  ::      > (~(neq cd:complex %n) 0x4000.0000.0000.0000.3ff0.0000.0000.0000 0x4010.0000.0000.0000.4008.0000.0000.0000)
  ::      %.y
  ::  Source
  ++  neq   |=([p=@ q=@] =(| (equ p q)))
  ::
  ::  Transcendental / elementary functions.  Self-contained real @rd series
  ::  (naive Taylor/AGM, fixed term counts -- accurate near the expansion point,
  ::  matching /lib/math and /lib/unum); component arithmetic stays on stdlib +rs.
  ::
  ++  rpi   `@rd`.~3.14159265358979                  :: pi at @rd
  ++  rexp
    |=  x=@rd  ^-  @rd
    =/  s  .~1  =/  t  .~1  =/  n=@  1
    |-  ?:  (gth n 20)  s
    =.  t  (~(mul rd rnd) t (~(div rd rnd) x (~(sun rd rnd) n)))
    $(n +(n), s (~(add rd rnd) s t))
  ++  rsin
    |=  x=@rd  ^-  @rd
    =/  t  x  =/  s  x  =/  n=@  1
    |-  ?:  (gth n 20)  s
    =/  k  (^mul 2 n)
    =.  t  (~(sub rd rnd) .~0 (~(mul rd rnd) t (~(div rd rnd) (~(mul rd rnd) x x) (~(mul rd rnd) (~(sun rd rnd) k) (~(sun rd rnd) +(k))))))
    $(n +(n), s (~(add rd rnd) s t))
  ++  rcos
    |=  x=@rd  ^-  @rd
    =/  t  .~1  =/  s  .~1  =/  n=@  1
    |-  ?:  (gth n 20)  s
    =/  k  (^mul 2 n)
    =.  t  (~(sub rd rnd) .~0 (~(mul rd rnd) t (~(div rd rnd) (~(mul rd rnd) x x) (~(mul rd rnd) (~(sun rd rnd) (dec k)) (~(sun rd rnd) k)))))
    $(n +(n), s (~(add rd rnd) s t))
  ++  rlog
    |=  x=@rd  ^-  @rd
    ?:  =(`@rd`.~0 x)  `@rd`0xfff0.0000.0000.0000
    =/  y   (~(div rd rnd) (~(sub rd rnd) x .~1) (~(add rd rnd) x .~1))
    =/  y2  (~(mul rd rnd) y y)  =/  s  y  =/  t  y  =/  n=@  1
    |-  ?:  (gth n 30)  (~(mul rd rnd) .~2 s)
    =.  t  (~(mul rd rnd) t y2)
    =/  c  (~(div rd rnd) .~1 (~(sun rd rnd) +((^mul 2 n))))
    $(n +(n), s (~(add rd rnd) s (~(mul rd rnd) c t)))
  ++  ratan                                  :: single-arg arctan (Gauss AGM)
    |=  x=@rd  ^-  @rd
    =/  rt  (~(sqt rd rnd) (~(add rd rnd) .~1 (~(mul rd rnd) x x)))
    =/  a  (~(div rd rnd) .~1 rt)  =/  b  .~1  =/  n=@  0
    |-  ?:  (gth n 40)  (~(div rd rnd) x (~(mul rd rnd) rt b))
    =/  ai  (~(mul rd rnd) .~0.5 (~(add rd rnd) a b))
    $(n +(n), a ai, b (~(sqt rd rnd) (~(mul rd rnd) ai b)))
  ++  ratn                                   :: atan2(y, x)
    |=  [y=@rd x=@rd]  ^-  @rd
    ?:  (~(gth rd rnd) x .~0)                          (ratan (~(div rd rnd) y x))
    ?:  &((~(lth rd rnd) x .~0) (~(gte rd rnd) y .~0))  (~(add rd rnd) (ratan (~(div rd rnd) y x)) rpi)
    ?:  &((~(lth rd rnd) x .~0) (~(lth rd rnd) y .~0))  (~(sub rd rnd) (ratan (~(div rd rnd) y x)) rpi)
    ?:  (~(gth rd rnd) y .~0)                          (~(div rd rnd) rpi .~2)
    ?:  (~(lth rd rnd) y .~0)                          (~(sub rd rnd) .~0 (~(div rd rnd) rpi .~2))
    .~0
  ::    +cexp:  @cd -> @cd
  ::
  ::  Complex exponential e^z = e^a (cos b + i sin b) for z = a + bi.
  ::  Source
  ++  cexp
    |=  p=@
    ^-  @
    =/  ea  (rexp (re p))
    (pak (~(mul rd rnd) ea (rcos (im p))) (~(mul rd rnd) ea (rsin (im p))))
  ::    +clog:  @cd -> @cd
  ::
  ::  Principal complex logarithm ln(z) = ln|z| + i*atan2(b, a).
  ::  Source
  ++  clog
    |=  p=@
    ^-  @
    =/  a  (re p)
    =/  b  (im p)
    =/  mag  (~(sqt rd rnd) (~(add rd rnd) (~(mul rd rnd) a a) (~(mul rd rnd) b b)))
    (pak (rlog mag) (ratn b a))
  ::    +csqrt:  @cd -> @cd
  ::
  ::  Principal complex square root: sqrt((|z|+a)/2) + i*sgn(b)*sqrt((|z|-a)/2),
  ::  with the imaginary sign taken from b (the principal branch).
  ::  Source
  ++  csqrt
    |=  p=@
    ^-  @
    =/  a  (re p)
    =/  b  (im p)
    =/  mag  (~(sqt rd rnd) (~(add rd rnd) (~(mul rd rnd) a a) (~(mul rd rnd) b b)))
    =/  re-  (~(sqt rd rnd) (~(div rd rnd) (~(add rd rnd) mag a) .~2))
    =/  im-  (~(sqt rd rnd) (~(div rd rnd) (~(sub rd rnd) mag a) .~2))
    (pak re- ?:((~(lth rd rnd) b .~0) (fneg im-) im-))
  ::    +csin:  @cd -> @cd
  ::
  ::  Complex sine sin(z) = sin(a)cosh(b) + i cos(a)sinh(b), with cosh/sinh built
  ::  from the real exponential.
  ::  Source
  ++  csin
    |=  p=@
    ^-  @
    =/  a  (re p)
    =/  b  (im p)
    =/  eb   (rexp b)
    =/  enb  (rexp (fneg b))
    =/  csh  (~(div rd rnd) (~(add rd rnd) eb enb) .~2)
    =/  snh  (~(div rd rnd) (~(sub rd rnd) eb enb) .~2)
    (pak (~(mul rd rnd) (rsin a) csh) (~(mul rd rnd) (rcos a) snh))
  ::    +ccos:  @cd -> @cd
  ::
  ::  Complex cosine cos(z) = cos(a)cosh(b) - i sin(a)sinh(b).
  ::  Source
  ++  ccos
    |=  p=@
    ^-  @
    =/  a  (re p)
    =/  b  (im p)
    =/  eb   (rexp b)
    =/  enb  (rexp (fneg b))
    =/  csh  (~(div rd rnd) (~(add rd rnd) eb enb) .~2)
    =/  snh  (~(div rd rnd) (~(sub rd rnd) eb enb) .~2)
    (pak (~(mul rd rnd) (rcos a) csh) (fneg (~(mul rd rnd) (rsin a) snh)))
  ::    +ctan:  @cd -> @cd
  ::
  ::  Complex tangent tan(z) = sin(z) / cos(z).
  ::  Source
  ++  ctan  |=(p=@ (div (csin p) (ccos p)))
  ::    +cpow:  [@cd @cd] -> @cd
  ::
  ::  Complex power z^w = exp(w * ln z) (principal branch).
  ::  Source
  ++  cpow  |=([p=@ q=@] (cexp (mul q (clog p))))
  --
::    +ch:  complex-half (@ch), two @rh (16-bit) components.
::
::  The @rh/@ch analogue of +cs over half-precision components (32-bit element).
::  Here 1+2i packs to 0x4000.3c00 and 3+4i to 0x4400.4200.
::
++  ch
  |_  rnd=rounding-mode
  ::
  ::  Components
  ::
  ::    +re:  @ch -> @rh
  ::
  ::  Returns the real component (low 16 bits) of a packed @ch value.
  ::    Examples
  ::      > (~(re ch:complex %n) 0x4000.3c00)  ::  re(1+2i)
  ::      .~~1
  ::  Source
  ++  re    |=(p=@ `@rh`(end [0 16] p))
  ::    +im:  @ch -> @rh
  ::
  ::  Returns the imaginary component (high 16 bits) of a packed @ch value.
  ::    Examples
  ::      > (~(im ch:complex %n) 0x4000.3c00)  ::  im(1+2i)
  ::      .~~2
  ::  Source
  ++  im    |=(p=@ `@rh`(rsh [0 16] p))
  ::    +pak:  [@rh @rh] -> @ch
  ::
  ::  Packs real and imaginary @rh components into one @ch atom (real low).
  ::    Examples
  ::      > (~(pak ch:complex %n) .~~1 .~~2)
  ::      0x4000.3c00
  ::  Source
  ++  pak   |=([r=@rh i=@rh] ^-(@ (con `@`r (lsh [0 16] `@`i))))
  ::  component helpers (internal)
  ++  fneg  |=(x=@rh (~(sub rh rnd) .~~0 x))
  ++  fabs  |=(x=@rh ?:((~(lth rh rnd) x .~~0) (~(sub rh rnd) .~~0 x) x))
  ::
  ::  Constants
  ::
  ::    +zero:  @ch
  ::
  ::  The additive identity 0+0i.
  ::    Examples
  ::      > ~(zero ch:complex %n)
  ::      0x0
  ::  Source
  ++  zero  ^-(@ 0)
  ::    +one:  @ch
  ::
  ::  The multiplicative identity 1+0i.
  ::    Examples
  ::      > ~(one ch:complex %n)
  ::      0x3c00
  ::  Source
  ++  one   (pak .~~1 .~~0)
  ::
  ::  Arithmetic
  ::
  ::    +add:  [@ch @ch] -> @ch
  ::
  ::  Returns the sum of two complex values (component-wise).
  ::    Examples
  ::      > (~(add ch:complex %n) 0x4000.3c00 0x4400.4200)  ::  (1+2i)+(3+4i)=4+6i
  ::      0x4600.4400
  ::  Source
  ++  add   |=([p=@ q=@] (pak (~(add rh rnd) (re p) (re q)) (~(add rh rnd) (im p) (im q))))
  ::    +sub:  [@ch @ch] -> @ch
  ::
  ::  Returns the difference of two complex values (component-wise).
  ::    Examples
  ::      > (~(sub ch:complex %n) 0x4000.3c00 0x4400.4200)  ::  =-2-2i
  ::      0xc000.c000
  ::  Source
  ++  sub   |=([p=@ q=@] (pak (~(sub rh rnd) (re p) (re q)) (~(sub rh rnd) (im p) (im q))))
  ::    +neg:  @ch -> @ch
  ::
  ::  Returns the additive inverse -z.
  ::    Examples
  ::      > (~(neg ch:complex %n) 0x4000.3c00)  ::  -(1+2i)
  ::      0xc000.bc00
  ::  Source
  ++  neg   |=(p=@ (pak (fneg (re p)) (fneg (im p))))
  ::    +conj:  @ch -> @ch
  ::
  ::  Returns the complex conjugate (negate the imaginary component).
  ::    Examples
  ::      > (~(conj ch:complex %n) 0x4000.3c00)  ::  conj(1+2i)=1-2i
  ::      0xc000.3c00
  ::  Source
  ++  conj  |=(p=@ (pak (re p) (fneg (im p))))
  ::    +mul:  [@ch @ch] -> @ch
  ::
  ::  Returns the complex product (ar+ai i)(br+bi i), rounded per component.
  ::    Examples
  ::      > (~(mul ch:complex %n) 0x4000.3c00 0x4400.4200)  ::  (1+2i)(3+4i)=-5+10i
  ::      0x4900.c500
  ::  Source
  ++  mul
    |=  [p=@ q=@]
    =/  ar  (re p)  =/  ai  (im p)
    =/  br  (re q)  =/  bi  (im q)
    %+  pak
      (~(sub rh rnd) (~(mul rh rnd) ar br) (~(mul rh rnd) ai bi))
    (~(add rh rnd) (~(mul rh rnd) ar bi) (~(mul rh rnd) ai br))
  ::    +div:  [@ch @ch] -> @ch
  ::
  ::  Returns the complex quotient via Smith's algorithm (scale by the larger
  ::  denominator component for numerical stability).
  ::    Examples
  ::      > (~(div ch:complex %n) 0x4000.4000 0x3c00.3c00)  ::  (2+2i)/(1+1i)=2
  ::      0x4000
  ::  Source
  ++  div
    |=  [p=@ q=@]
    =/  ar  (re p)  =/  ai  (im p)
    =/  br  (re q)  =/  bi  (im q)
    ?:  (~(gte rh rnd) (fabs br) (fabs bi))
      =/  r   (~(div rh rnd) bi br)
      =/  dn  (~(add rh rnd) br (~(mul rh rnd) bi r))
      %+  pak
        (~(div rh rnd) (~(add rh rnd) ar (~(mul rh rnd) ai r)) dn)
      (~(div rh rnd) (~(sub rh rnd) ai (~(mul rh rnd) ar r)) dn)
    =/  r   (~(div rh rnd) br bi)
    =/  dn  (~(add rh rnd) (~(mul rh rnd) br r) bi)
    %+  pak
      (~(div rh rnd) (~(add rh rnd) (~(mul rh rnd) ar r) ai) dn)
    (~(div rh rnd) (~(sub rh rnd) (~(mul rh rnd) ai r) ar) dn)
  ::    +abs:  @ch -> @ch
  ::
  ::  Returns the modulus |z| as a real-valued complex [|z|, 0], via hypot.
  ::    Examples
  ::      > (~(abs ch:complex %n) 0x4400.4200)  ::  |3+4i|=5
  ::      0x4500
  ::  Source
  ++  abs
    |=  p=@
    =/  xr  (fabs (re p))  =/  xi  (fabs (im p))
    ?:  &(=(`@rh`.~~0 xr) =(`@rh`.~~0 xi))  (pak .~~0 .~~0)
    ?:  (~(gte rh rnd) xr xi)
      =/  t  (~(div rh rnd) xi xr)
      (pak (~(mul rh rnd) xr (~(sqt rh rnd) (~(add rh rnd) .~~1 (~(mul rh rnd) t t)))) .~~0)
    =/  t  (~(div rh rnd) xr xi)
    (pak (~(mul rh rnd) xi (~(sqt rh rnd) (~(add rh rnd) .~~1 (~(mul rh rnd) t t)))) .~~0)
  ::
  ::  Comparison (no total order on complex -- only equality)
  ::
  ::    +equ:  [@ch @ch] -> ?
  ::
  ::  Loobean: bit-equality of both components.
  ::    Examples
  ::      > (~(equ ch:complex %n) 0x4000.3c00 0x4000.3c00)
  ::      %.y
  ::      > (~(equ ch:complex %n) 0x4000.3c00 0x4400.4200)
  ::      %.n
  ::  Source
  ++  equ   |=([p=@ q=@] &(=((re p) (re q)) =((im p) (im q))))
  ::    +neq:  [@ch @ch] -> ?
  ::
  ::  Loobean negation of +equ.
  ::    Examples
  ::      > (~(neq ch:complex %n) 0x4000.3c00 0x4400.4200)
  ::      %.y
  ::  Source
  ++  neq   |=([p=@ q=@] =(| (equ p q)))
  ::
  ::  Transcendental / elementary functions.  Self-contained real @rh series
  ::  (naive Taylor/AGM, fixed term counts -- accurate near the expansion point,
  ::  matching /lib/math and /lib/unum); component arithmetic stays on stdlib +rs.
  ::
  ++  rpi   `@rh`.~~3.14159                  :: pi at @rh
  ++  rexp
    |=  x=@rh  ^-  @rh
    =/  s  .~~1  =/  t  .~~1  =/  n=@  1
    |-  ?:  (gth n 20)  s
    =.  t  (~(mul rh rnd) t (~(div rh rnd) x (~(sun rh rnd) n)))
    $(n +(n), s (~(add rh rnd) s t))
  ++  rsin
    |=  x=@rh  ^-  @rh
    =/  t  x  =/  s  x  =/  n=@  1
    |-  ?:  (gth n 20)  s
    =/  k  (^mul 2 n)
    =.  t  (~(sub rh rnd) .~~0 (~(mul rh rnd) t (~(div rh rnd) (~(mul rh rnd) x x) (~(mul rh rnd) (~(sun rh rnd) k) (~(sun rh rnd) +(k))))))
    $(n +(n), s (~(add rh rnd) s t))
  ++  rcos
    |=  x=@rh  ^-  @rh
    =/  t  .~~1  =/  s  .~~1  =/  n=@  1
    |-  ?:  (gth n 20)  s
    =/  k  (^mul 2 n)
    =.  t  (~(sub rh rnd) .~~0 (~(mul rh rnd) t (~(div rh rnd) (~(mul rh rnd) x x) (~(mul rh rnd) (~(sun rh rnd) (dec k)) (~(sun rh rnd) k)))))
    $(n +(n), s (~(add rh rnd) s t))
  ++  rlog
    |=  x=@rh  ^-  @rh
    ?:  =(`@rh`.~~0 x)  `@rh`0xfc00
    =/  y   (~(div rh rnd) (~(sub rh rnd) x .~~1) (~(add rh rnd) x .~~1))
    =/  y2  (~(mul rh rnd) y y)  =/  s  y  =/  t  y  =/  n=@  1
    |-  ?:  (gth n 30)  (~(mul rh rnd) .~~2 s)
    =.  t  (~(mul rh rnd) t y2)
    =/  c  (~(div rh rnd) .~~1 (~(sun rh rnd) +((^mul 2 n))))
    $(n +(n), s (~(add rh rnd) s (~(mul rh rnd) c t)))
  ++  ratan                                  :: single-arg arctan (Gauss AGM)
    |=  x=@rh  ^-  @rh
    =/  rt  (~(sqt rh rnd) (~(add rh rnd) .~~1 (~(mul rh rnd) x x)))
    =/  a  (~(div rh rnd) .~~1 rt)  =/  b  .~~1  =/  n=@  0
    |-  ?:  (gth n 40)  (~(div rh rnd) x (~(mul rh rnd) rt b))
    =/  ai  (~(mul rh rnd) .~~0.5 (~(add rh rnd) a b))
    $(n +(n), a ai, b (~(sqt rh rnd) (~(mul rh rnd) ai b)))
  ++  ratn                                   :: atan2(y, x)
    |=  [y=@rh x=@rh]  ^-  @rh
    ?:  (~(gth rh rnd) x .~~0)                          (ratan (~(div rh rnd) y x))
    ?:  &((~(lth rh rnd) x .~~0) (~(gte rh rnd) y .~~0))  (~(add rh rnd) (ratan (~(div rh rnd) y x)) rpi)
    ?:  &((~(lth rh rnd) x .~~0) (~(lth rh rnd) y .~~0))  (~(sub rh rnd) (ratan (~(div rh rnd) y x)) rpi)
    ?:  (~(gth rh rnd) y .~~0)                          (~(div rh rnd) rpi .~~2)
    ?:  (~(lth rh rnd) y .~~0)                          (~(sub rh rnd) .~~0 (~(div rh rnd) rpi .~~2))
    .~~0
  ::    +cexp:  @ch -> @ch
  ::
  ::  Complex exponential e^z = e^a (cos b + i sin b) for z = a + bi.
  ::  Source
  ++  cexp
    |=  p=@
    ^-  @
    =/  ea  (rexp (re p))
    (pak (~(mul rh rnd) ea (rcos (im p))) (~(mul rh rnd) ea (rsin (im p))))
  ::    +clog:  @ch -> @ch
  ::
  ::  Principal complex logarithm ln(z) = ln|z| + i*atan2(b, a).
  ::  Source
  ++  clog
    |=  p=@
    ^-  @
    =/  a  (re p)
    =/  b  (im p)
    =/  mag  (~(sqt rh rnd) (~(add rh rnd) (~(mul rh rnd) a a) (~(mul rh rnd) b b)))
    (pak (rlog mag) (ratn b a))
  ::    +csqrt:  @ch -> @ch
  ::
  ::  Principal complex square root: sqrt((|z|+a)/2) + i*sgn(b)*sqrt((|z|-a)/2),
  ::  with the imaginary sign taken from b (the principal branch).
  ::  Source
  ++  csqrt
    |=  p=@
    ^-  @
    =/  a  (re p)
    =/  b  (im p)
    =/  mag  (~(sqt rh rnd) (~(add rh rnd) (~(mul rh rnd) a a) (~(mul rh rnd) b b)))
    =/  re-  (~(sqt rh rnd) (~(div rh rnd) (~(add rh rnd) mag a) .~~2))
    =/  im-  (~(sqt rh rnd) (~(div rh rnd) (~(sub rh rnd) mag a) .~~2))
    (pak re- ?:((~(lth rh rnd) b .~~0) (fneg im-) im-))
  ::    +csin:  @ch -> @ch
  ::
  ::  Complex sine sin(z) = sin(a)cosh(b) + i cos(a)sinh(b), with cosh/sinh built
  ::  from the real exponential.
  ::  Source
  ++  csin
    |=  p=@
    ^-  @
    =/  a  (re p)
    =/  b  (im p)
    =/  eb   (rexp b)
    =/  enb  (rexp (fneg b))
    =/  csh  (~(div rh rnd) (~(add rh rnd) eb enb) .~~2)
    =/  snh  (~(div rh rnd) (~(sub rh rnd) eb enb) .~~2)
    (pak (~(mul rh rnd) (rsin a) csh) (~(mul rh rnd) (rcos a) snh))
  ::    +ccos:  @ch -> @ch
  ::
  ::  Complex cosine cos(z) = cos(a)cosh(b) - i sin(a)sinh(b).
  ::  Source
  ++  ccos
    |=  p=@
    ^-  @
    =/  a  (re p)
    =/  b  (im p)
    =/  eb   (rexp b)
    =/  enb  (rexp (fneg b))
    =/  csh  (~(div rh rnd) (~(add rh rnd) eb enb) .~~2)
    =/  snh  (~(div rh rnd) (~(sub rh rnd) eb enb) .~~2)
    (pak (~(mul rh rnd) (rcos a) csh) (fneg (~(mul rh rnd) (rsin a) snh)))
  ::    +ctan:  @ch -> @ch
  ::
  ::  Complex tangent tan(z) = sin(z) / cos(z).
  ::  Source
  ++  ctan  |=(p=@ (div (csin p) (ccos p)))
  ::    +cpow:  [@ch @ch] -> @ch
  ::
  ::  Complex power z^w = exp(w * ln z) (principal branch).
  ::  Source
  ++  cpow  |=([p=@ q=@] (cexp (mul q (clog p))))
  --
::    +cq:  complex-quad (@cq), two @rq (128-bit) components.
::
::  The @rq/@cq analogue of +cs over quad-precision components (256-bit element).
::  Here 1+2i packs to
::  0x4000.0000.0000.0000.0000.0000.0000.0000.3fff.0000.0000.0000.0000.0000.0000.0000
::  and 3+4i to
::  0x4001.0000.0000.0000.0000.0000.0000.0000.4000.8000.0000.0000.0000.0000.0000.0000.
::
++  cq
  |_  rnd=rounding-mode
  ::
  ::  Components
  ::
  ::    +re:  @cq -> @rq
  ::
  ::  Returns the real component (low 128 bits) of a packed @cq value.
  ::    Examples
  ::      > (~(re cq:complex %n) 0x4000.0000.0000.0000.0000.0000.0000.0000.3fff.0000.0000.0000.0000.0000.0000.0000)
  ::      .~~~1
  ::  Source
  ++  re    |=(p=@ `@rq`(end [0 128] p))
  ::    +im:  @cq -> @rq
  ::
  ::  Returns the imaginary component (high 128 bits) of a packed @cq value.
  ::    Examples
  ::      > (~(im cq:complex %n) 0x4000.0000.0000.0000.0000.0000.0000.0000.3fff.0000.0000.0000.0000.0000.0000.0000)
  ::      .~~~2
  ::  Source
  ++  im    |=(p=@ `@rq`(rsh [0 128] p))
  ::    +pak:  [@rq @rq] -> @cq
  ::
  ::  Packs real and imaginary @rq components into one @cq atom (real low).
  ::    Examples
  ::      > (~(pak cq:complex %n) .~~~1 .~~~2)
  ::      0x4000.0000.0000.0000.0000.0000.0000.0000.3fff.0000.0000.0000.0000.0000.0000.0000
  ::  Source
  ++  pak   |=([r=@rq i=@rq] ^-(@ (con `@`r (lsh [0 128] `@`i))))
  ::  component helpers (internal)
  ++  fneg  |=(x=@rq (~(sub rq rnd) .~~~0 x))
  ++  fabs  |=(x=@rq ?:((~(lth rq rnd) x .~~~0) (~(sub rq rnd) .~~~0 x) x))
  ::
  ::  Constants
  ::
  ::    +zero:  @cq
  ::
  ::  The additive identity 0+0i.
  ::    Examples
  ::      > ~(zero cq:complex %n)
  ::      0x0
  ::  Source
  ++  zero  ^-(@ 0)
  ::    +one:  @cq
  ::
  ::  The multiplicative identity 1+0i.
  ::    Examples
  ::      > ~(one cq:complex %n)
  ::      0x3fff.0000.0000.0000.0000.0000.0000.0000
  ::  Source
  ++  one   (pak .~~~1 .~~~0)
  ::
  ::  Arithmetic
  ::
  ::    +add:  [@cq @cq] -> @cq
  ::
  ::  Returns the sum of two complex values (component-wise).  (1+2i)+(3+4i)=4+6i.
  ::    Examples
  ::      > (~(add cq:complex %n) `@`0x4000.0000.0000.0000.0000.0000.0000.0000.3fff.0000.0000.0000.0000.0000.0000.0000 0x4001.0000.0000.0000.0000.0000.0000.0000.4000.8000.0000.0000.0000.0000.0000.0000)
  ::      0x4001.8000.0000.0000.0000.0000.0000.0000.4001.0000.0000.0000.0000.0000.0000.0000
  ::  Source
  ++  add   |=([p=@ q=@] (pak (~(add rq rnd) (re p) (re q)) (~(add rq rnd) (im p) (im q))))
  ::    +sub:  [@cq @cq] -> @cq
  ::
  ::  Returns the difference of two complex values (component-wise).  =-2-2i.
  ::    Examples
  ::      > (~(sub cq:complex %n) `@`0x4000.0000.0000.0000.0000.0000.0000.0000.3fff.0000.0000.0000.0000.0000.0000.0000 0x4001.0000.0000.0000.0000.0000.0000.0000.4000.8000.0000.0000.0000.0000.0000.0000)
  ::      0xc000.0000.0000.0000.0000.0000.0000.0000.c000.0000.0000.0000.0000.0000.0000.0000
  ::  Source
  ++  sub   |=([p=@ q=@] (pak (~(sub rq rnd) (re p) (re q)) (~(sub rq rnd) (im p) (im q))))
  ::    +neg:  @cq -> @cq
  ::
  ::  Returns the additive inverse -z.  -(1+2i)=-1-2i.
  ::    Examples
  ::      > (~(neg cq:complex %n) 0x4000.0000.0000.0000.0000.0000.0000.0000.3fff.0000.0000.0000.0000.0000.0000.0000)
  ::      0xc000.0000.0000.0000.0000.0000.0000.0000.bfff.0000.0000.0000.0000.0000.0000.0000
  ::  Source
  ++  neg   |=(p=@ (pak (fneg (re p)) (fneg (im p))))
  ::    +conj:  @cq -> @cq
  ::
  ::  Returns the complex conjugate (negate the imaginary component).  conj(1+2i)=1-2i.
  ::    Examples
  ::      > (~(conj cq:complex %n) 0x4000.0000.0000.0000.0000.0000.0000.0000.3fff.0000.0000.0000.0000.0000.0000.0000)
  ::      0xc000.0000.0000.0000.0000.0000.0000.0000.3fff.0000.0000.0000.0000.0000.0000.0000
  ::  Source
  ++  conj  |=(p=@ (pak (re p) (fneg (im p))))
  ::    +mul:  [@cq @cq] -> @cq
  ::
  ::  Returns the complex product, rounded per component.  (1+2i)(3+4i)=-5+10i.
  ::    Examples
  ::      > (~(mul cq:complex %n) `@`0x4000.0000.0000.0000.0000.0000.0000.0000.3fff.0000.0000.0000.0000.0000.0000.0000 0x4001.0000.0000.0000.0000.0000.0000.0000.4000.8000.0000.0000.0000.0000.0000.0000)
  ::      0x4002.4000.0000.0000.0000.0000.0000.0000.c001.4000.0000.0000.0000.0000.0000.0000
  ::  Source
  ++  mul
    |=  [p=@ q=@]
    =/  ar  (re p)  =/  ai  (im p)
    =/  br  (re q)  =/  bi  (im q)
    %+  pak
      (~(sub rq rnd) (~(mul rq rnd) ar br) (~(mul rq rnd) ai bi))
    (~(add rq rnd) (~(mul rq rnd) ar bi) (~(mul rq rnd) ai br))
  ::    +div:  [@cq @cq] -> @cq
  ::
  ::  Returns the complex quotient via Smith's algorithm.  (2+2i)/(1+1i)=2.
  ::    Examples
  ::      > (~(div cq:complex %n) `@`0x4000.0000.0000.0000.0000.0000.0000.0000.4000.0000.0000.0000.0000.0000.0000.0000 0x3fff.0000.0000.0000.0000.0000.0000.0000.3fff.0000.0000.0000.0000.0000.0000.0000)
  ::      0x4000.0000.0000.0000.0000.0000.0000.0000
  ::  Source
  ++  div
    |=  [p=@ q=@]
    =/  ar  (re p)  =/  ai  (im p)
    =/  br  (re q)  =/  bi  (im q)
    ?:  (~(gte rq rnd) (fabs br) (fabs bi))
      =/  r   (~(div rq rnd) bi br)
      =/  dn  (~(add rq rnd) br (~(mul rq rnd) bi r))
      %+  pak
        (~(div rq rnd) (~(add rq rnd) ar (~(mul rq rnd) ai r)) dn)
      (~(div rq rnd) (~(sub rq rnd) ai (~(mul rq rnd) ar r)) dn)
    =/  r   (~(div rq rnd) br bi)
    =/  dn  (~(add rq rnd) (~(mul rq rnd) br r) bi)
    %+  pak
      (~(div rq rnd) (~(add rq rnd) (~(mul rq rnd) ar r) ai) dn)
    (~(div rq rnd) (~(sub rq rnd) (~(mul rq rnd) ai r) ar) dn)
  ::    +abs:  @cq -> @cq
  ::
  ::  Returns the modulus |z| as a real-valued complex [|z|, 0], via hypot.  |3+4i|=5.
  ::    Examples
  ::      > (~(abs cq:complex %n) 0x4001.0000.0000.0000.0000.0000.0000.0000.4000.8000.0000.0000.0000.0000.0000.0000)
  ::      0x4001.4000.0000.0000.0000.0000.0000.0000
  ::  Source
  ++  abs
    |=  p=@
    =/  xr  (fabs (re p))  =/  xi  (fabs (im p))
    ?:  &(=(`@rq`.~~~0 xr) =(`@rq`.~~~0 xi))  (pak .~~~0 .~~~0)
    ?:  (~(gte rq rnd) xr xi)
      =/  t  (~(div rq rnd) xi xr)
      (pak (~(mul rq rnd) xr (~(sqt rq rnd) (~(add rq rnd) .~~~1 (~(mul rq rnd) t t)))) .~~~0)
    =/  t  (~(div rq rnd) xr xi)
    (pak (~(mul rq rnd) xi (~(sqt rq rnd) (~(add rq rnd) .~~~1 (~(mul rq rnd) t t)))) .~~~0)
  ::
  ::  Comparison (no total order on complex -- only equality)
  ::
  ::    +equ:  [@cq @cq] -> ?
  ::
  ::  Loobean: bit-equality of both components.
  ::    Examples
  ::      > =z  0x4000.0000.0000.0000.0000.0000.0000.0000.3fff.0000.0000.0000.0000.0000.0000.0000  ::  1+2i
  ::      > (~(equ cq:complex %n) z z)
  ::      %.y
  ::  Source
  ++  equ   |=([p=@ q=@] &(=((re p) (re q)) =((im p) (im q))))
  ::    +neq:  [@cq @cq] -> ?
  ::
  ::  Loobean negation of +equ.
  ::    Examples
  ::      > (~(neq cq:complex %n) `@`0x4000.0000.0000.0000.0000.0000.0000.0000.3fff.0000.0000.0000.0000.0000.0000.0000 0x4001.0000.0000.0000.0000.0000.0000.0000.4000.8000.0000.0000.0000.0000.0000.0000)
  ::      %.y
  ::  Source
  ++  neq   |=([p=@ q=@] =(| (equ p q)))
  ::
  ::  Transcendental / elementary functions.  Self-contained real @rq series
  ::  (naive Taylor/AGM, fixed term counts -- accurate near the expansion point,
  ::  matching /lib/math and /lib/unum); component arithmetic stays on stdlib +rs.
  ::
  ++  rpi   `@rq`.~~~3.14159265358979323846                  :: pi at @rq
  ++  rexp
    |=  x=@rq  ^-  @rq
    =/  s  .~~~1  =/  t  .~~~1  =/  n=@  1
    |-  ?:  (gth n 20)  s
    =.  t  (~(mul rq rnd) t (~(div rq rnd) x (~(sun rq rnd) n)))
    $(n +(n), s (~(add rq rnd) s t))
  ++  rsin
    |=  x=@rq  ^-  @rq
    =/  t  x  =/  s  x  =/  n=@  1
    |-  ?:  (gth n 20)  s
    =/  k  (^mul 2 n)
    =.  t  (~(sub rq rnd) .~~~0 (~(mul rq rnd) t (~(div rq rnd) (~(mul rq rnd) x x) (~(mul rq rnd) (~(sun rq rnd) k) (~(sun rq rnd) +(k))))))
    $(n +(n), s (~(add rq rnd) s t))
  ++  rcos
    |=  x=@rq  ^-  @rq
    =/  t  .~~~1  =/  s  .~~~1  =/  n=@  1
    |-  ?:  (gth n 20)  s
    =/  k  (^mul 2 n)
    =.  t  (~(sub rq rnd) .~~~0 (~(mul rq rnd) t (~(div rq rnd) (~(mul rq rnd) x x) (~(mul rq rnd) (~(sun rq rnd) (dec k)) (~(sun rq rnd) k)))))
    $(n +(n), s (~(add rq rnd) s t))
  ++  rlog
    |=  x=@rq  ^-  @rq
    ?:  =(`@rq`.~~~0 x)  `@rq`0xffff.0000.0000.0000.0000.0000.0000.0000
    =/  y   (~(div rq rnd) (~(sub rq rnd) x .~~~1) (~(add rq rnd) x .~~~1))
    =/  y2  (~(mul rq rnd) y y)  =/  s  y  =/  t  y  =/  n=@  1
    |-  ?:  (gth n 30)  (~(mul rq rnd) .~~~2 s)
    =.  t  (~(mul rq rnd) t y2)
    =/  c  (~(div rq rnd) .~~~1 (~(sun rq rnd) +((^mul 2 n))))
    $(n +(n), s (~(add rq rnd) s (~(mul rq rnd) c t)))
  ++  ratan                                  :: single-arg arctan (Gauss AGM)
    |=  x=@rq  ^-  @rq
    =/  rt  (~(sqt rq rnd) (~(add rq rnd) .~~~1 (~(mul rq rnd) x x)))
    =/  a  (~(div rq rnd) .~~~1 rt)  =/  b  .~~~1  =/  n=@  0
    |-  ?:  (gth n 40)  (~(div rq rnd) x (~(mul rq rnd) rt b))
    =/  ai  (~(mul rq rnd) .~~~0.5 (~(add rq rnd) a b))
    $(n +(n), a ai, b (~(sqt rq rnd) (~(mul rq rnd) ai b)))
  ++  ratn                                   :: atan2(y, x)
    |=  [y=@rq x=@rq]  ^-  @rq
    ?:  (~(gth rq rnd) x .~~~0)                          (ratan (~(div rq rnd) y x))
    ?:  &((~(lth rq rnd) x .~~~0) (~(gte rq rnd) y .~~~0))  (~(add rq rnd) (ratan (~(div rq rnd) y x)) rpi)
    ?:  &((~(lth rq rnd) x .~~~0) (~(lth rq rnd) y .~~~0))  (~(sub rq rnd) (ratan (~(div rq rnd) y x)) rpi)
    ?:  (~(gth rq rnd) y .~~~0)                          (~(div rq rnd) rpi .~~~2)
    ?:  (~(lth rq rnd) y .~~~0)                          (~(sub rq rnd) .~~~0 (~(div rq rnd) rpi .~~~2))
    .~~~0
  ::    +cexp:  @cq -> @cq
  ::
  ::  Complex exponential e^z = e^a (cos b + i sin b) for z = a + bi.
  ::  Source
  ++  cexp
    |=  p=@
    ^-  @
    =/  ea  (rexp (re p))
    (pak (~(mul rq rnd) ea (rcos (im p))) (~(mul rq rnd) ea (rsin (im p))))
  ::    +clog:  @cq -> @cq
  ::
  ::  Principal complex logarithm ln(z) = ln|z| + i*atan2(b, a).
  ::  Source
  ++  clog
    |=  p=@
    ^-  @
    =/  a  (re p)
    =/  b  (im p)
    =/  mag  (~(sqt rq rnd) (~(add rq rnd) (~(mul rq rnd) a a) (~(mul rq rnd) b b)))
    (pak (rlog mag) (ratn b a))
  ::    +csqrt:  @cq -> @cq
  ::
  ::  Principal complex square root: sqrt((|z|+a)/2) + i*sgn(b)*sqrt((|z|-a)/2),
  ::  with the imaginary sign taken from b (the principal branch).
  ::  Source
  ++  csqrt
    |=  p=@
    ^-  @
    =/  a  (re p)
    =/  b  (im p)
    =/  mag  (~(sqt rq rnd) (~(add rq rnd) (~(mul rq rnd) a a) (~(mul rq rnd) b b)))
    =/  re-  (~(sqt rq rnd) (~(div rq rnd) (~(add rq rnd) mag a) .~~~2))
    =/  im-  (~(sqt rq rnd) (~(div rq rnd) (~(sub rq rnd) mag a) .~~~2))
    (pak re- ?:((~(lth rq rnd) b .~~~0) (fneg im-) im-))
  ::    +csin:  @cq -> @cq
  ::
  ::  Complex sine sin(z) = sin(a)cosh(b) + i cos(a)sinh(b), with cosh/sinh built
  ::  from the real exponential.
  ::  Source
  ++  csin
    |=  p=@
    ^-  @
    =/  a  (re p)
    =/  b  (im p)
    =/  eb   (rexp b)
    =/  enb  (rexp (fneg b))
    =/  csh  (~(div rq rnd) (~(add rq rnd) eb enb) .~~~2)
    =/  snh  (~(div rq rnd) (~(sub rq rnd) eb enb) .~~~2)
    (pak (~(mul rq rnd) (rsin a) csh) (~(mul rq rnd) (rcos a) snh))
  ::    +ccos:  @cq -> @cq
  ::
  ::  Complex cosine cos(z) = cos(a)cosh(b) - i sin(a)sinh(b).
  ::  Source
  ++  ccos
    |=  p=@
    ^-  @
    =/  a  (re p)
    =/  b  (im p)
    =/  eb   (rexp b)
    =/  enb  (rexp (fneg b))
    =/  csh  (~(div rq rnd) (~(add rq rnd) eb enb) .~~~2)
    =/  snh  (~(div rq rnd) (~(sub rq rnd) eb enb) .~~~2)
    (pak (~(mul rq rnd) (rcos a) csh) (fneg (~(mul rq rnd) (rsin a) snh)))
  ::    +ctan:  @cq -> @cq
  ::
  ::  Complex tangent tan(z) = sin(z) / cos(z).
  ::  Source
  ++  ctan  |=(p=@ (div (csin p) (ccos p)))
  ::    +cpow:  [@cq @cq] -> @cq
  ::
  ::  Complex power z^w = exp(w * ln z) (principal branch).
  ::  Source
  ++  cpow  |=([p=@ q=@] (cexp (mul q (clog p))))
  --
--
