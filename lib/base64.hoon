::  |base64: flexible base64 encoding for little-endian atoms
::
::  pad: include padding when encoding, require when decoding
::  url: use url-safe characters '-' for '+' and '_' for '/'
::
=+  [pad=& url=|]
|%
::  +en:base64: encode +octs to base64 cord
::
++  en
  |=  inp=octs
  ^-  cord
  ::  dif: offset from 3-byte block
  ::
  =/  dif=@ud  (~(dif fo 3) 0 p.inp)
  ::  dap: reversed, 3-byte block-aligned input
  ::
  =/  dap=@ux  (lsh 3 dif (rev 3 inp))
  =/  cha
    ?:  url
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_'
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
  %-  crip
  %-  flop
  %+  weld
    ?.(pad ~ (reap dif '='))
  %+  slag  dif
  |-  ^-  tape
  ?:  =(0x0 dap)  ~
  =/  d  (end 3 3 dap)
  :*  (cut 3 [(cut 0 [0 6] d) 1] cha)
      (cut 3 [(cut 0 [6 6] d) 1] cha)
      (cut 3 [(cut 0 [12 6] d) 1] cha)
      (cut 3 [(cut 0 [18 6] d) 1] cha)
      $(dap (rsh 3 3 dap))
  ==
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
    ::  %+  base  64
    %+  roll
      (weld dat (reap dif 0))
    |=([p=@ q=@] (add p (mul 64 q)))
  --
--

