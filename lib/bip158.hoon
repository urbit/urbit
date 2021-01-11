|%
+$  bits  [wid=@ dat=@ub]
++  params
  |%
  ++  p  19
  --
::  +str: bit streams
::   read/write are from/to the front (big-endian)
::
++  str
  |%
  ++  read-bit
    |=  s=bits
    ^-  [bit=@ub rest=bits]
    ?>  (gth wid.s 0)
    :*  ?:((gth wid.s (met 0 dat.s)) 0b0 0b1)
        [(dec wid.s) (end [0 (dec wid.s)] dat.s)]
    ==
  ::
  ++  read-bits
    |=  [n=@ s=bits]
    ^-  [bits rest=bits]
    =|  bs=bits
    |-
    ?:  =(n 0)  [bs s]
    =^  b  s  (read-bit s)
    $(n (dec n), bs (write-bit s b))
  ::
  ++  write-bit
    |=  [s=bits bit=@ub]
    ^-  bits
    ?>  (lte (met 0 bit) 1)
    [+(wid.s) (can 0 ~[s [1 bit]])]
  --
::  +gol: Golomb-Rice encoding/decoding
::
++  gol
  |%
  ++  en
    |=  b=byts
    ^-  @ux
    0x0
  ::
  ++  de
    |=  [s=bits p=@]
    |^
    ^-  [delta=@ rest=bits]
    ?>  (gth wid.s 0)
    =^  q  s  (get-q s)
    =^  r  s  (read-bits:str p s)
    [(add dat.r (lsh [0 p] q)) s]
    ::
    ++  get-q
      |=  s=bits
      =|  q=@
      =^  first-bit  s  (read-bit:str s)
      |-
      ?:  =(0 first-bit)  [q s]
      =^  b  s  (read-bit:str s)
      $(first-bit b, q +(q))
    --
  --
--
