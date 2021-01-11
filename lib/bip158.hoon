|%
+$  bits  [wid=@ dat=@ub]
++  params
  |%
  ++  p  19
  --
::  +stre: bit streams
::   read/write are from/to the front
::
++  stre
  |%
  ++  read-bit
    |=  s=bits
    ^-  [bit=@ub rest=bits]
    ?>  (gth wid.s 0)
    :*  ?:((gth wid.s (met 0 dat.s)) 0b0 0b1)
        [(dec wid.s) (end [0 (dec wid.s)] dat.s)]
    ==
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
    |=  s=bits
    ^-  [hash=byts rest=bits]
    [*byts *bits]
  --
--
