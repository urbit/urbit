=/  protocol-version=?(%0 %1 %2 %3 %4 %5 %6 %7)  %0
::
=>
|%
+$  dyad    [sndr=@p rcvr=@p]
+$  packet  [=dyad encrypted=? origin=(unit @uxlane) content=*]
::  +rank: which kind of ship address, by length
::
::    0: galaxy or star
::    1: planet
::    2: moon
::    3: comet
::
+$  rank  ?(%0 %1 %2 %3)
--
|%
::  +encode-packet: TODO
::
++  encode-packet
  |=  packet
  ^-  @uxblob
  ::
  =/  sndr-meta  (encode-ship-metadata sndr.dyad)
  =/  rcvr-meta  (encode-ship-metadata rcvr.dyad)
  ::  body: <<sndr rcvr (jam [origin content])>>
  ::
  =/  body=@
    ;:  mix
      sndr.dyad
      (lsh 3 size.sndr-meta rcvr.dyad)
      (lsh 3 (add size.sndr-meta size.rcvr-meta) (jam [origin content]))
    ==
  ::  header: 32-bit header assembled from bitstreams of fields
  ::
  ::    <<version checksum sndr-rank rcvr-rank encryption-type unused>>
  ::    4 bits at the end of the header are unused.
  ::
  =/  header
    %+  can  0
    :~  [3 protocol-version]
        [20 (mug body)]
        [2 rank.sndr-meta]
        [2 rank.rcvr-meta]
        [5 ?:(encrypted %0 %1)]
    ==
  ::  result is <<header body>>
  ::
  (mix header (lsh 5 1 body))
::  +decode-packet: deserialize packet from bytestream, reading header
::
++  decode-packet
  |=  blob=@uxblob
  ^-  packet
  ::  first 32 (2^5) bits are header; the rest is body
  ::
  =/  header  (end 5 1 blob)
  =/  body    (rsh 5 1 blob)
  ::
  =/  version    (end 0 3 header)
  =/  checksum   (cut 0 [3 20] header)
  =/  sndr-size  (decode-ship-size (cut 0 [23 2] header))
  =/  rcvr-size  (decode-ship-size (cut 0 [25 2] header))
  =/  encrypted  ?+((cut 0 [27 5] header) !! %0 %.y, %1 %.n)
  ::
  ?>  =(protocol-version version)
  ?>  =(checksum (end 0 20 (mug body)))
  ::
  =/  =dyad
    :-  sndr=(end 3 sndr-size body)
    rcvr=(cut 3 [sndr-size rcvr-size] body)
  ::
  =+  ;;  [origin=(unit @uxlane) content=*]
      %-  cue
      (rsh 3 (add rcvr-size sndr-size) body)
  ::
  [dyad encrypted origin content]
::  +decode-ship-size: decode a 2-bit ship type specifier into a byte width
::
::    Type 0: galaxy or star -- 2 bytes
::    Type 1: planet         -- 4 bytes
::    Type 2: moon           -- 8 bytes
::    Type 3: comet          -- 16 bytes
::
++  decode-ship-size
  |=  rank=@
  ^-  @
  ::
  ?+  rank  !!
    %0  2
    %1  4
    %2  8
    %3  16
  ==
::  +encode-ship-metadata: produce size (in bytes) and address rank for :ship
::
::    0: galaxy or star
::    1: planet
::    2: moon
::    3: comet
::
++  encode-ship-metadata
  |=  =ship
  ^-  [size=@ =rank]
  ::
  =/  size=@  (met 3 ship)
  ::
  ?:  (lte size 2)  [2 %0]
  ?:  (lte size 4)  [4 %1]
  ?:  (lte size 8)  [8 %2]
  [16 %3]
--
