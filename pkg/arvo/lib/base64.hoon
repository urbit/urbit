::  |base64: flexible base64 encoding for little-endian atoms
::
::  pad: include padding when encoding, require when decoding
::  url: use url-safe characters '-' for '+' and '_' for '/'
::
=+  [pad=& url=|]
|%
::
+$  byte    @D
+$  word24  @
::
++  div-ceil
  ::  divide, rounding up.
  |=  [x=@ y=@]  ^-  @
  ?:  =(0 (mod x y))
    (div x y)
  +((div x y))
::
++  explode-bytes
  ::  Explode a bytestring into list of bytes. Result is in LSB order.
  |=  =octs  ^-  (list byte)
  =/  atom-byte-width  (met 3 q.octs)
  =/  leading-zeros    (sub p.octs atom-byte-width)
  (weld (reap leading-zeros 0) (rip 3 q.octs))
::
++  explode-words
  ::  Explode a bytestring to words of bit-width `wid`. Result is in LSW order.
  |=  [wid=@ =octs]
  ^-  (list @)
  =/  atom-bit-width   (met 0 q.octs)
  =/  octs-bit-width   (mul 8 p.octs)
  =/  atom-word-width  (div-ceil atom-bit-width wid)
  =/  rslt-word-width  (div-ceil octs-bit-width wid)
  =/  pad              (sub rslt-word-width atom-word-width)
  =/  x  (ripn wid q.octs)
  %+  weld  x
  (reap pad 0)
::
::  +en:base64: encode +octs to base64 cord
::
::  Encode an `octs` into a base64 string.
::
::  First, we break up the input into a list of 24-bit words. The input
::  might not be a multiple of 24-bits, so we add 0-2 padding bytes at
::  the end (to the least-significant side, with a left-shift).
::
::  Then, we encode each block into four base64 characters.
::
::  Finally we remove the padding that we added at the beginning: for
::  each byte that was added, we replace one character with an = (unless
::  `pad` is false, in which case we just remove the extra characters).
::
++  en
  ^-  $-(octs cord)
  ::
  =/  cha
    ?:  url
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_'
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
  ::
  |^  |=  bs=octs  ^-  cord
      =/  [padding=@ blocks=(list word24)]
        (octs-to-blocks bs)
      (crip (flop (unpad padding (encode-blocks blocks))))
  ::
  ++  octs-to-blocks
    |=  bs=octs  ^-  [padding=@ud (list word24)]
    =/  padding=@ud  (~(dif fo 3) 0 p.bs)
    =/  padded=octs  [(add padding p.bs) (lsh 3 padding (rev 3 bs))]
    [padding (explode-words 24 padded)]
  ::
  ++  unpad
    |=  [extra=@ t=tape]  ^-  tape
    =/  without  (slag extra t)
    ?.  pad  without
    (weld (reap extra '=') without)
  ::
  ++  encode-blocks
    |=  ws=(list word24)  ^-  tape
    (zing (turn ws encode-block))
  ::
  ++  encode-block
    |=  w=word24  ^-  tape
    =/  a  (cut 3 [(cut 0 [0 6] w) 1] cha)
    =/  b  (cut 3 [(cut 0 [6 6] w) 1] cha)
    =/  c  (cut 3 [(cut 0 [12 6] w) 1] cha)
    =/  d  (cut 3 [(cut 0 [18 6] w) 1] cha)
    ~[a b c d]
  --
::
::  +de:base64: decode base64 cord to (unit @)
::
++  de
  |=  a=cord
  ^-  (unit octs)
  (rush a parse)
::  +parse:base64: parse base64 cord to +octs
::
++  parse
  =<  ^-  $-(nail (like octs))
      %+  sear  reduce
      ;~  plug
        %-  plus  ;~  pose
          (cook |=(a=@ (sub a 'A')) (shim 'A' 'Z'))
          (cook |=(a=@ (sub a 'G')) (shim 'a' 'z'))
          (cook |=(a=@ (add a 4)) (shim '0' '9'))
          (cold 62 (just ?:(url '-' '+')))
          (cold 63 (just ?:(url '_' '/')))
        ==
        (stun 0^2 (cold %0 tis))
      ==
  |%
  ::  +reduce:parse:base64: reduce, measure, and swap base64 digits
  ::
  ++  reduce
    |=  [dat=(list @) dap=(list @)]
    ^-  (unit octs)
    =/  lat  (lent dat)
    =/  lap  (lent dap)
    =/  dif  (~(dif fo 4) 0 lat)
    ?:  &(pad !=(dif lap))
      ::  padding required and incorrect
      ~&(%base-64-padding-err-one ~)
    ?:  &(!pad !=(0 lap))
      ::  padding not required but present
      ~&(%base-64-padding-err-two ~)
    =/  len  (sub (mul 3 (div (add lat dif) 4)) dif)
    :+  ~  len
    %+  swp  3
    (repn 6 (flop (weld dat (reap dif 0))))
  --
--
