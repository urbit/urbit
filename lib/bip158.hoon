|%
+$  bits  [wid=@ dat=@ub]
++  params
  |%
  ++  p  19
  --
::  +str: bit streams
::   read is from the front
::   write appends to the back
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
    $(n (dec n), bs (write-bits bs [1 b]))
  ::
  ++  write-bits
    |=  [s1=bits s2=bits]
    ^-  bits
    [(add wid.s1 wid.s2) (can 0 ~[s2 s1])]
  --
::  +gol: Golomb-Rice encoding/decoding
::
++  gol
  |%
  ::  +en: encode x and append to end of s
  ::
  ++  en
    |=  [s=bits x=@ p=@]
    ^-  bits
    =+  q=(rsh [0 p] x)
    =+  unary=[+(q) (lsh [0 1] (dec (bex q)))]
    =+  r=[p (end [0 p] x)]
    %+  write-bits:str  s
    (write-bits:str unary r)
  ::
  ++  de
    |=  [s=bits p=@]
    ^-  [delta=@ rest=bits]
    |^  ?>  (gth wid.s 0)
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
