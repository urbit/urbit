/-  bc=bitcoin
/+  bcu=bitcoin-utils
|%
++  params
  |%
  ++  p  19
  ++  m  784.931
  --
::
++  siphash
  |=  [k=byts m=byts]
  ^-  byts
  |^
  ?>  =(wid.k 16)
  ?>  (lte (met 3 dat.k) wid.k)
  ?>  (lte (met 3 dat.m) wid.m)
  =.  k  (flim:sha k)
  =.  m  (flim:sha m)
  (flim:sha (fin (comp m (init dat.k))))
  :: Initialise internal state
  ::
  ++  init
    |=  k=@
    ^-  [@ @ @ @]
    =/  k0=@  (end [6 1] k)
    =/  k1=@  (cut 6 [1 1] k)
    :^    (mix k0 0x736f.6d65.7073.6575)
        (mix k1 0x646f.7261.6e64.6f6d)
      (mix k0 0x6c79.6765.6e65.7261)
    (mix k1 0x7465.6462.7974.6573)
  ::
  :: Compression rounds
  ++  comp
    |=  [m=byts v=[v0=@ v1=@ v2=@ v3=@]]
    ^-  [@ @ @ @]
    =/  len=@ud  (div wid.m 8)
    =/  last=@  (lsh [3 7] (mod wid.m 256))
    =|  i=@ud
    =|  w=@
    |-
    =.  w  (cut 6 [i 1] dat.m)
    ?:  =(i len)
      =.  v3.v  (mix v3.v (mix last w))
      =.  v  (rnd (rnd v))
      =.  v0.v  (mix v0.v (mix last w))
      v
    %=  $
      v  =.  v3.v  (mix v3.v w)
         =.  v  (rnd (rnd v))
         =.  v0.v  (mix v0.v w)
         v
      i  (add i 1)
    ==
  ::
  :: Finalisation rounds
  ++  fin
    |=  v=[v0=@ v1=@ v2=@ v3=@]
    ^-  byts
    =.  v2.v  (mix v2.v 0xff)
    =.  v  (rnd (rnd (rnd (rnd v))))
    :-  8
    :(mix v0.v v1.v v2.v v3.v)
  ::
  :: Sipround
  ++  rnd
    |=  [v0=@ v1=@ v2=@ v3=@]
    ^-  [@ @ @ @]
    =.  v0  (~(sum fe 6) v0 v1)
    =.  v2  (~(sum fe 6) v2 v3)
    =.  v1  (~(rol fe 6) 0 13 v1)
    =.  v3  (~(rol fe 6) 0 16 v3)
    =.  v1  (mix v1 v0)
    =.  v3  (mix v3 v2)
    =.  v0  (~(rol fe 6) 0 32 v0)
    =.  v2  (~(sum fe 6) v2 v1)
    =.  v0  (~(sum fe 6) v0 v3)
    =.  v1  (~(rol fe 6) 0 17 v1)
    =.  v3  (~(rol fe 6) 0 21 v3)
    =.  v1  (mix v1 v2)
    =.  v3  (mix v3 v0)
    =.  v2  (~(rol fe 6) 0 32 v2)
    [v0 v1 v2 v3]
  --
::  +str: bit streams
::   read is from the front
::   write appends to the back
::
++  str
  |%
  ++  read-bit
    |=  s=bits:bc
    ^-  [bit=@ub rest=bits:bc]
    ?>  (gth wid.s 0)
    :*  ?:((gth wid.s (met 0 dat.s)) 0b0 0b1)
        [(dec wid.s) (end [0 (dec wid.s)] dat.s)]
    ==
  ::
  ++  read-bits
    |=  [n=@ s=bits:bc]
    ^-  [bits:bc rest=bits:bc]
    =|  bs=bits:bc
    |-
    ?:  =(n 0)  [bs s]
    =^  b  s  (read-bit s)
    $(n (dec n), bs (write-bits bs [1 b]))
  ::
  ++  write-bits
    |=  [s1=bits:bc s2=bits:bc]
    ^-  bits:bc
    [(add wid.s1 wid.s2) (can 0 ~[s2 s1])]
  --
::  +gol: Golomb-Rice encoding/decoding
::
++  gol
  |%
  ::  +en: encode x and append to end of s
  ::   - s: bits stream
  ::   - x: number to add to the stream
  ::   - p: golomb-rice p param
  ::
  ++  en
    |=  [s=bits:bc x=@ p=@]
    ^-  bits:bc
    =+  q=(rsh [0 p] x)
    =+  unary=[+(q) (lsh [0 1] (dec (bex q)))]
    =+  r=[p (end [0 p] x)]
    %+  write-bits:str  s
    (write-bits:str unary r)
  ::
  ++  de
    |=  [s=bits:bc p=@]
    ^-  [delta=@ rest=bits:bc]
    |^  ?>  (gth wid.s 0)
    =^  q  s  (get-q s)
    =^  r  s  (read-bits:str p s)
    [(add dat.r (lsh [0 p] q)) s]
    ::
    ++  get-q
      |=  s=bits:bc
      =|  q=@
      =^  first-bit  s  (read-bit:str s)
      |-
      ?:  =(0 first-bit)  [q s]
      =^  b  s  (read-bit:str s)
      $(first-bit b, q +(q))
    --
  --
::  +hsh
::
++  hsh
  |%
  ::  +to-range
  ::   - item: scriptpubkey to hash
  ::   - f: N*M
  ::   - k: key for siphash (end of blockhash, reversed)
  ::
  ++  to-range
    |=  [item=byts f=@ k=byts]
    ^-  @
    (rsh [0 64] (mul f (swp 3 dat:(siphash k item))))
  ::  +set-construct: return sorted hashes of scriptpubkeys
  ::
  ++  set-construct
  |=  [items=(list byts) k=byts f=@]
    ^-  (list @)
    %+  sort
      %+  turn  items
      |=  item=byts
      (to-range item f k)
    lth
  --
::
++  parse-filter
  |=  filter=hexb:bc
  ^-  [n=@ux gcs-set=bits:bc]
  =/  n  n:(de:csiz:bcu filter)
  =/  lead=@  ?:(=(1 wid.n) 1 +(wid.n))
  :-  dat.n
  [(mul 8 (sub wid.filter lead)) `@ub`dat:(drop:byt:bcu lead filter)]
::  +to-key: blockhash (little endian) to key for siphash
::
++  to-key
  |=  blockhash=tape
  ^-  byts
  %+  take:byt:bcu  16
  %-  flip:byt:bcu
  (from-cord:hxb:bcu (crip blockhash))
::  +match: whether block filter matches *any* target scriptpubkeys
::   - filter: full block filter, with leading N
::   - k: key for siphash (end of blockhash, reversed)
::   - targets: scriptpubkeys to match
::
++  match
  |=  [filter=hexb:bc k=byts targets=(list byts)]
  ^-  ?
  =/  [p=@ m=@]  [p:params m:params]
  =/  [n=@ux gcs-set=bits:bc]  (parse-filter filter)
  =+  target-hs=(set-construct:hsh targets k (mul n m))
  =+  last-val=0
  |-
  ?~  target-hs  %.n
  ?:  =(last-val i.target-hs)
    %.y
  ?:  (gth last-val i.target-hs)
    $(target-hs t.target-hs)
  ::  last-val is less than target: check next val in GCS, if any
  ::
  ?:  (lth wid.gcs-set p)  %.n
  =^  delta  gcs-set
    (de:gol gcs-set p)
  $(last-val (add delta last-val))
::  +all-match: returns all target byts that match
::   - filter: full block filter, with leading N
::   - targets: scriptpubkeys to match
::
++  all-match
  |=  [filter=hexb:bc blockhash=hexb:bc targets=(list [address:bc byts])]
  ^-  (set [address:bc hexb:bc])
  =/  k  (to-key (trip (to-cord:hxb:bcu blockhash)))
  %-  ~(gas in *(set [address:bc hexb:bc]))
  =/  [p=@ m=@]  [p:params m:params]
  =/  [n=@ux gcs-set=bits:bc]  (parse-filter filter)
  =/  target-map=(map @ [address:bc hexb:bc])
    %-  ~(gas by *(map @ [address:bc hexb:bc]))
    %+  turn  targets
    |=  [a=address:bc t=hexb:bc]
    [(to-range:hsh t (mul n m) k) a t]
  =+  target-hs=(sort ~(tap in ~(key by target-map)) lth)
  =+  last-val=0
  =|  matches=(list @)
  |-
  ?~  target-hs
    (murn matches ~(get by target-map))
  ?:  =(last-val i.target-hs)
    %=  $
        target-hs  t.target-hs
        matches    [last-val matches]
    ==
  ?:  (gth last-val i.target-hs)
    $(target-hs t.target-hs)
  ::  last-val is less than target: get next val in GCS, if any
  ::
  ?:  (lth wid.gcs-set p)
    (murn matches ~(get by target-map))
  =^  delta  gcs-set
    (de:gol gcs-set p)
  $(last-val (add delta last-val))
::
--
