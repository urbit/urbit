::  bip39 implementation in hoon
::
/+  bip39-english
::
|%
++  from-entropy
  |=  byts
  ^-  tape
  =.  wid  (mul wid 8)
  ~|  [%unsupported-entropy-bit-length wid]
  ?>  &((gte wid 128) (lte wid 256))
  ::
  =+  cs=(div wid 32)
  =/  check=@
    %^  rsh  0  (sub 256 cs)
    (sha-256l:sha (div wid 8) dat)
  =/  bits=byts
    :-  (add wid cs)
    %+  can  0
    :~  cs^check
        wid^dat
    ==
  ::
  =/  pieces
    |-  ^-  (list @)
    :-  (end 0 11 dat.bits)
    ?:  (lte wid.bits 11)  ~
    $(bits [(sub wid.bits 11) (rsh 0 11 dat.bits)])
  ::
  =/  words=(list tape)
    %+  turn  pieces
    |=  ind=@ud
    (snag ind `(list tape)`bip39-english)
  ::
  %+  roll  (flop words)
  |=  [nex=tape all=tape]
  ?~  all  nex
  :(weld all " " nex)
::
::NOTE  always produces a 512-bit result
++  to-seed
  |=  [mnem=tape pass=tape]
  ^-  @
  %-  hmac-sha512t:pbkdf:crypto
  [(crip mnem) (crip (weld "mnemonic" pass)) 2.048 64]
--
