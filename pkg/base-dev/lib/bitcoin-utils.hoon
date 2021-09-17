::  lib/bitcoin-utils.hoon
::  Utilities for working with BTC data types and transactions
::
/-  *bitcoin
~%  %bitcoin-utils-lib  ..part  ~
|%
::
::  TODO: move this bit/byt stuff to zuse
::  bit/byte utilities
::
::
::  +blop: munge bit and byt sequences (cat, flip, take, drop)
::
++  blop
  ~/  %blop
  |_  =bloq
  +$  biyts  [wid=@ud dat=@]
  ++  cat
    |=  bs=(list biyts)
    ^-  biyts
    :-  (roll (turn bs |=(b=biyts -.b)) add)
    (can bloq (flop bs))
  ::  +flip: flip endianness while preserving lead/trail zeroes
  ::
  ++  flip
    |=  b=biyts
    ^-  biyts
    [wid.b (rev bloq b)]
  ::  +take: take n bloqs from front
  ::  pads front with extra zeroes if n is longer than input
  ::
  ++  take
    |=  [n=@ b=biyts]
    ^-  biyts
    ?:  (gth n wid.b)
      [n dat.b]
    [n (rsh [bloq (sub wid.b n)] dat.b)]
  ::  +drop: drop n bloqs from front
  ::  returns 0^0 if n >= width
  ::
  ++  drop
    |=  [n=@ b=biyts]
    ^-  biyts
    ?:  (gte n wid.b)
      0^0x0
    =+  n-take=(sub wid.b n)
    [n-take (end [bloq n-take] dat.b)]
  --
++  byt  ~(. blop 3)
::
++  bit
  ~/  %bit
  =/  bl    ~(. blop 0)
  |%
  ++  cat   cat:bl:bit
  ++  flip  flip:bl:bit
  ++  take  take:bl:bit
  ++  drop  drop:bl:bit
  ++  from-atoms
    |=  [bitwidth=@ digits=(list @)]
    ^-  bits
    %-  cat:bit
    %+  turn  digits
    |=  a=@
    ?>  (lte (met 0 a) bitwidth)
    [bitwidth `@ub`a]
  ::  +to-atoms: convert bits to atoms of bitwidth
  ::
   ++  to-atoms
    |=  [bitwidth=@ bs=bits]
    ^-  (list @)
    =|  res=(list @)
    ?>  =(0 (mod wid.bs bitwidth))
    |-
    ?:  =(0 wid.bs)  res
    %=  $
        res  (snoc res dat:(take:bit bitwidth bs))
        bs   (drop:bit bitwidth bs)
    ==
  --
::  big endian sha256: input and output are both MSB first (big endian)
::
++  sha256
  ~/  %sha256
  |=  =byts
  ^-  hexb
  %-  flip:byt
  [32 (shay (flip:byt byts))]
::
++  dsha256
  ~/  %dsha256
  |=  =byts
  (sha256 (sha256 byts))
::
++  hash-160
  ~/  %hash-160
  |=  val=byts
  ^-  hexb
  =,  ripemd:crypto
  :-  20
  %-  ripemd-160
  (sha256 val)

::
::  hxb: hex parsing utilities
::
++  hxb
  ~%  %hxb  ..blop  ~
  |%
  ++  from-cord
    ~/  %from-cord
    |=  h=@t
    ^-  hexb
    ?:  =('' h)  1^0x0
    ::  Add leading 00
    ::
    =+  (lsh [3 2] h)
    ::  Group by 4-size block
    ::
    =+  (rsh [3 2] -)
    ::  Parse hex to atom
    ::
    =/  a  (need (de:base16:mimes:html -))
    [-.a `@ux`+.a]
  ::
  ++  to-cord
    ~/  %to-cord
    |=  =hexb
    ^-  cord
    (en:base16:mimes:html hexb)
  --
::
::  +csiz: CompactSize integers (a Bitcoin-specific datatype)
::  https://btcinformation.org/en/developer-reference#compactsize-unsigned-integers
::   - encode: big endian to little endian
::   - decode: little endian to big endian
::
++  csiz
  ~%  %csiz  ..blop  ~
  |%
  ++  en
    ~/  %en
    |=  a=@
    ^-  hexb
    =/  l=@  (met 3 a)
    ?:  =(l 1)  1^a
    ?:  =(l 2)  (cat:byt ~[1^0xfd (flip:byt 2^a)])
    ?:  (lte l 4)  (cat:byt ~[1^0xfe (flip:byt 4^a)])
    ?:  (lte l 8)  (cat:byt ~[1^0xff (flip:byt 8^a)])
    ~|("Cannot encode CompactSize longer than 8 bytes" !!)
  ::
  ++  de
    ~/  %de
    |=  h=hexb
    ^-  [n=hexb rest=hexb]
    =/  s=@ux  dat:(take:byt 1 h)
    ?:  (lth s 0xfd)  [1^s (drop:byt 1 h)]
    ~|  "Invalid compact-size at start of {<h>}"
    =/  len=bloq
    ?+  s  !!
      %0xfd  1
      %0xfe  2
      %0xff  3
    ==
    :_  (drop:byt (add 1 len) h)
    %-  flip:byt
    (take:byt (bex len) (drop:byt 1 h))
  ::  +dea: atom instead of hexb for parsed CompactSize
  ::
  ++  dea
    |=  h=hexb
    ^-  [a=@ rest=hexb]
    =>  (de h)
    [dat.n rest]
  --
--
