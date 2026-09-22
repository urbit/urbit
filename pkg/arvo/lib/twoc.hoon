~%  %non  ..part  ~  :: jet registration; nest non in hex (cf /lib/unum, /lib/math)
|%
::  Two's-complement signed integers, MODULAR (operations wrap mod 2^width
::  rather than crashing on overflow -- a deliberate divergence from a
::  checked-overflow type, matching hardware two's-complement and Lagoon's
::  %uint wrapping).  Two doors share one implementation:
::
::  - +twid: keyed on a raw bit-WIDTH (any @), for callers like /lib/fixed
::    whose widths are not powers of two (N = a + b + 1).
::  - +twoc: keyed on a `bloq` (width = 2^bloq), for callers like /lib/unum
::    and Lagoon that work in bloq-sized lanes.  It delegates to +twid at
::    width (bex bloq), so the logic is defined once in +twid.
::
++  twid
  ~/  %twid
  |_  wid=@
  ::  +len: bit width.  +len-mod: the modulus 2^width.
  ++  len  wid
  ++  len-mod  (bex wid)
  ::  +msb: the sign bit (1 if the width-1 bit is set).
  ++  msb
    |=  a=@
    ?:  (^lth (xeb a) len)
      0
    1
  ++  ones  (dec len)
  ::  +s-to-twoc: hoon signed integer (@s) -> width-bit two's-complement.
  ::  Wraps modularly for out-of-range inputs.
  ::    > (~(s-to-twoc twid 8) -14)
  ::      242
  ++  s-to-twoc
    |=  a=@s
    ^-  @
    =+  [sn mg]=(old:si a)
    ?:  sn  (mod mg len-mod)
    (neg mg)
  ::  +twoc-to-s: width-bit two's-complement -> hoon signed integer (@s).
  ++  twoc-to-s
    |=  a=@
    ^-  @s
    ?:  =(1 (msb a))  (new:si | (abs a))
    (new:si & (mod a len-mod))
  ::
  ::  Arithmetic.  Because the low `len` bits of a sum/product are independent
  ::  of sign, the same bit-level add and multiply serve signed and unsigned.
  ::
  ++  add  ~/  %add  |=([a=@ b=@] ^-(@ (mod (^add a b) len-mod)))
  ::  +neg: two's-complement negation, ~a + 1 (flip the len bits, add one).
  ::  neg(min) = min (wraps).
  ++  neg  ~/  %neg  |=(a=@ ^-(@ (mod +((not 0 len (mod a len-mod))) len-mod)))
  ++  sub  ~/  %sub  |=([a=@ b=@] ^-(@ (add a (neg b))))
  ::  low-order bits of a product are sign-independent, so modular unsigned mul is also correct signed mul
  ++  mul  ~/  %mul  |=([a=@ b=@] ^-(@ (mod (^mul (mod a len-mod) (mod b len-mod)) len-mod)))
  ::  +abs: |a| as a two's-complement value (abs(min) wraps back to min).
  ++  abs  ~/  %abs  |=(a=@ ^-(@ ?:(=(1 (msb a)) (neg a) (mod a len-mod))))
  ::  +overflow: would a + b overflow a CHECKED signed add?  Retained for
  ::  callers that want to detect rather than wrap; +add no longer uses it.
  ++  overflow
    |=  [a=@ b=@ c=@]
    ?|  &(=(0 (msb c)) =(1 (msb a)) =(1 (msb b)))
        &(=(1 (msb c)) =(0 (msb a)) =(0 (msb b)))
    ==
  ::  +div: signed division, truncating toward zero (C / Hoon `div` style).
  ::  Division by zero crashes (as `div` does).  div(min, -1) wraps to min.
  ++  div
    ~/  %div
    |=  [a=@ b=@]
    ^-  @
    =/  sa  (msb a)
    =/  sb  (msb b)
    =/  q   (^div (abs a) (abs b))
    ?:  =(sa sb)  (mod q len-mod)
    (neg q)
  ::  +rem: signed remainder, sign follows the DIVIDEND (C `%` / truncated).
  ::  a == (add (mul (div a b) b) (rem a b)).
  ++  rem
    ~/  %rem
    |=  [a=@ b=@]
    ^-  @
    =/  r  (mod (abs a) (abs b))
    ?:  =(0 (msb a))  (mod r len-mod)
    (neg r)
  ::  +pow: a to a NON-NEGATIVE integer power n (raw @), by modular squaring.
  ++  pow
    ~/  %pow
    |=  [a=@ n=@]
    ^-  @
    =/  base  (mod a len-mod)
    =/  acc   (mod 1 len-mod)
    |-  ^-  @
    ?:  =(0 n)  acc
    =?  acc  =(1 (dis n 1))  (mul acc base)
    $(n (rsh 0 n), base (mul base base))
  ::  +extend: sign-extension fill (all-ones if negative, else zero).
  ++  extend
    |=  a=@
    ^-  @
    ?:  =((msb a) 0)
      0
    (dec (bex len))
  ::  Comparisons, by two's-complement order (negatives below non-negatives).
  ++  gth
    ~/  %gth
    |=  [a=@ b=@]
    ?:  =(1 (mix (msb a) (msb b)))
      =(0 (msb a))
    (^gth a b)
  ++  lth  ~/  %lth  |=([a=@ b=@] (gth b a))
  ++  lte  ~/  %lte  |=([a=@ b=@] !(gth a b))
  ++  gte  ~/  %gte  |=([a=@ b=@] !(gth b a))
  --
::
::  +twoc: bloq-keyed facade over +twid (width = 2^bloq).  Logic lives in
::  +twid; these arms just re-export it at the delegated width, so existing
::  callers `~(add twoc:twoc bloq)` keep working unchanged.
::
++  twoc
  |_  =bloq
  +*  w  ~(. twid (bex bloq))
  ++  len        len:w
  ++  len-mod    len-mod:w
  ++  msb        msb:w
  ++  ones       ones:w
  ++  s-to-twoc  s-to-twoc:w
  ++  twoc-to-s  twoc-to-s:w
  ++  add        add:w
  ++  neg        neg:w
  ++  sub        sub:w
  ++  mul        mul:w
  ++  abs        abs:w
  ++  overflow   overflow:w
  ++  div        div:w
  ++  rem        rem:w
  ++  pow        pow:w
  ++  extend     extend:w
  ++  gth        gth:w
  ++  lth        lth:w
  ++  lte        lte:w
  ++  gte        gte:w
  --
--
