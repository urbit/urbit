::  bip32 implementation in hoon
::  temporarily includes supporting crypto, this should all go into stdlib
::
::  tmp useful links:
::  https://bitcoin.stackexchange.com/questions/61957/edge-cases-for-bip32
::  https://bitcoin.stackexchange.com/questions/21974/need-sample-compressed-and-uncompressed-public-private-key-pairs-for-bigintege
::  https://crypto.stackexchange.com/questions/41316/complete-set-of-test-vectors-for-ecdsa-secp256k1
::  https://github.com/scogliani/ecc-test-vectors/tree/master/ecc_pointmul_test_vectors
::  https://crypto.stackexchange.com/a/21206
::
|%
::
+=  byts  [wid=@ud dat=@]  ::NOTE  different from octs, those expect @t/lsb
::
::NOTE  tested to be correct against
::      https://en.bitcoin.it/wiki/BIP_0032_TestVectors
++  bip
  =,  hmac
  =,  secp
  =+  ecc=secp256k1
  ::  prv:  private key
  ::  pub:  public key
  ::  cad:  chain code
  ::  dep:  depth in chain
  ::  ind:  index at depth
  ::  pif:  parent fingerprint (4 bytes)
  |_  [prv=@ pub=pont cad=@ dep=@ud ind=@ud pif=@]
  ::
  +=  keyc  [key=@ cai=@]  ::  prv/pub key + chain code
  ::
  ::  elliptic curve operations and values
  ::
  ++  point  priv-to-pub.ecc
  ::
  ++  ser-p  compress-point.ecc
  ::
  ++  n      ^n:ecc
  ::
  ::  core initialization
  ::
  ++  from-seed
    |=  byts
    ^+  +>
    =+  der=(hmac-sha512l [12 'dees nioctiB'] [wid dat])
    =+  pri=(cut 3 [32 32] der)
    +>.$(prv pri, pub (point pri), cad (cut 3 [0 32] der))
  ::
  ++  from-private
    |=  keyc
    +>(prv key, pub (point key), cad cai)
  ::
  ++  from-public
    |=  keyc
    +>(pub (decompress-point.ecc key), cad cai)
  ::
  ++  from-public-point
    |=  [pon=pont cai=@]
    +>(pub pon, cad cai)
  ::
  ++  from-extended
    |=  t=tape
    =+  x=(de-base58check 4 t)
    =>  |%
        ++  take
          |=  b=@ud
          ^-  [v=@ x=@]
          :-  (end 3 b x)
          (rsh 3 b x)
        --
    =^  k  x  (take 33)
    =^  c  x  (take 32)
    =^  i  x  (take 4)
    =^  p  x  (take 4)
    =^  d  x  (take 1)
    ?>  =(0 x)  ::  sanity check
    %.  [d i p]
    =<  set-metadata
    =+  v=(scag 4 t)
    ?:  =("xprv" v)  (from-private k c)
    ?:  =("xpub" v)  (from-public k c)
    !!
  ::
  ++  set-metadata
    |=  [d=@ud i=@ud p=@]
    +>(dep d, ind i, pif p)
  ::
  ::  derivation
  ::
  ++  derivation-path
    ;~  pfix
      ;~(pose (jest 'm/') (easy ~))
    %+  most  net
    ;~  pose
      %+  cook
        |=(i=@ (add i (bex 31)))
      ;~(sfix dem say)
    ::
      dem
    ==  ==
  ::
  ++  derive-path
    |=  t=tape
    %-  derive-sequence
    (scan t derivation-path)
  ::
  ++  derive-sequence
    |=  j=(list @u)
    ?~  j  +>
    =.  +>  (derive i.j)
    $(j t.j)
  ::
  ::
  ++  derive
    ?:  =(0 prv)
      derive-public
    derive-private
  ::
  ++  derive-private
    |=  i=@u
    ^+  +>
    ::  we must have a private key to derive the next one
    ?:  =(0 prv)
      ~|  %know-no-private-key
      !!
    ::  derive child at i
    =+  ^-  [left=@ right=@]  ::TODO  =/ w/o face
      =-  [(cut 3 [32 32] -) (cut 3 [0 32] -)]
      %+  hmac-sha512l  [32 cad]
      :-  37
      ?:  (gte i (bex 31))
        ::  hardened child
        (can 3 ~[4^i 32^prv 1^0])
      ::  normal child
      (can 3 ~[4^i 33^(ser-p (point prv))])
    =+  key=(mod (add left prv) n)
    ::  rare exception, invalid key, go to the next one
    ?:  |(=(0 key) (gte left n))  $(i +(i))
    %_  +>.$
      prv   key
      pub   (point key)
      cad   right
      dep   +(dep)
      ind   i
      pif   fingerprint
    ==
  ::
  ++  derive-public
    |=  i=@u
    ^+  +>
    ::  public keys can't be hardened
    ?:  (gte i (bex 31))
      ~|  %cant-derive-hardened-public-key
      !!
    ::  derive child at i
    =+  ^-  [left=@ right=@]  ::TODO  =/  w/o face
      =-  [(cut 3 [32 32] -) (cut 3 [0 32] -)]
      %+  hmac-sha512l  [32 cad]
      37^(can 3 ~[4^i 33^(ser-p pub)])
    ::  rare exception, invalid key, go to the next one
    ?:  (gte left n)  $(i +(i))  ::TODO  or child key is "point at infinity"
    %_  +>.$
      pub   (jc-add.ecc (point left) pub)
      cad   right
      dep   +(dep)
      ind   i
      pif   fingerprint
    ==
  ::
  ::  rendering
  ::
  ++  private-key     ?.(=(0 prv) prv ~|(%know-no-private-key !!))
  ++  public-key      (ser-p pub)
  ++  chain-code      cad
  ++  private-chain   [private-key cad]
  ++  public-chain    [public-key cad]
  ::
  ++  identity        (hash160 public-key)
  ++  fingerprint     (cut 3 [16 4] identity)
  ::
  ++  prv-extended
    %+  en-b58c-bip32  0x488.ade4
    (build-extended private-key)
  ::
  ++  pub-extended
    %+  en-b58c-bip32  0x488.b21e
    (build-extended public-key)
  ::
  ++  build-extended
    |=  key=@
    %+  can  3
    :~  33^key
        32^cad
        4^ind
        4^pif
        1^dep
    ==
  ::
  ++  en-b58c-bip32
    |=  [v=@ k=@]
    (en-base58check [4 v] [74 k])
  ::
  ::  stdlib
  ::
  ++  en-base58check
    ::  v: version bytes
    ::  vw: amount of version bytes
    |=  [[vw=@u v=@] [dw=@u d=@]]
    %-  en-base58
    =+  p=[(add vw dw) (can 3 ~[dw^d vw^v])]
    =-  (can 3 ~[4^- p])
    %^  rsh  3  28
    (sha-256l:sha 32 (sha-256l:sha p))
  ::
  ++  de-base58check
    ::  vw: amount of version bytes
    |=  [vw=@u t=tape]
    =+  x=(de-base58 t)
    =+  hash=(sha-256l:sha 32 (sha-256:sha (rsh 3 4 x)))
    ?>  =((end 3 4 x) (rsh 3 28 hash))
    (cut 3 [vw (sub (met 3 x) (add 4 vw))] x)
  ::
  ++  en-base58
    |=  dat=@
    =/  cha
      '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'
    %-  flop
    |-  ^-  tape
    ?:  =(0 dat)  ~
    :-  (cut 3 [(mod dat 58) 1] cha)
    $(dat (div dat 58))
  ::
  ++  de-base58
    |=  t=tape
    =-  (scan t (bass 58 (plus -)))
    ;~  pose
      (cook |=(a=@ (sub a 56)) (shim 'A' 'H'))
      (cook |=(a=@ (sub a 57)) (shim 'J' 'N'))
      (cook |=(a=@ (sub a 58)) (shim 'P' 'Z'))
      (cook |=(a=@ (sub a 64)) (shim 'a' 'k'))
      (cook |=(a=@ (sub a 65)) (shim 'm' 'z'))
      (cook |=(a=@ (sub a 49)) (shim '1' '9'))
    ==
  --
::
::  argon2
::
::NOTE  ported from and tested against
::      https://pypi.org/project/argon2pure/
++  argon2
  |%
  ::
  ::  structures
  ::
  +=  argon-type  ?(%d %i %id %u)
  ::
  ::  shorthands
  ::
  ++  argon2-minimal
    (argon2 32 %id 0x13 1 8 1 *byts *byts)
  ::
  ::TODO  discuss and standardize?
  ++  argon2-urbit
    (argon2 64 %u 0x13 4 1.024 10 *byts *byts)
  ::
  ::  argon2 proper
  ::
  ::  main argon2 operation
  ++  argon2
    ::  out:       desired output size in bytes
    ::  typ:       argon2 type
    ::  version:   argon2 version (0x10/v1.0 or 0x13/v1.3)
    ::  threads:   amount of threads/parallelism
    ::  mem-cost:  kb of memory to use
    ::  time-cost: iterations to run
    ::  key:       optional secret
    ::  extra:     optional arbitrary data
    |=  $:  out=@ud
            typ=argon-type
            version=@ux
          ::
            threads=@ud
            mem-cost=@ud
            time-cost=@ud
          ::
            key=byts
            extra=byts
        ==
    ^-  $-([msg=byts sat=byts] @)
    ::
    ::  check configuration sanity
    ::
    ?:  =(0 threads)
      ~|  %parallelism-must-be-above-zero
      !!
    ?:  =(0 time-cost)
      ~|  %time-cost-must-be-above-zero
      !!
    ?:  (lth mem-cost (mul 8 threads))
      ~|  :-  %memory-cost-must-be-at-least-threads
          [threads %times 8 (mul 8 threads)]
      !!
    ?.  |(=(0x10 version) =(0x13 version))
      ~|  [%unsupported-version version %want [0x10 0x13]]
      !!
    ::
    ::  calculate constants and initialize buffer
    ::
    ::  for each thread, there is a row in the buffer.
    ::  the amount of columns depends on the memory-cost.
    ::  columns are split into groups of four.
    ::  a single such quarter section of a row is a segment.
    ::
    ::  blocks:     (m_prime)
    ::  columns:    row length (q)
    ::  seg-length: segment length
    =/  blocks=@ud
      ::  round mem-cost down to the nearest multiple of 4*threads
      =+  (mul 4 threads)
      (mul (div mem-cost -) -)
    =+  columns=(div blocks threads)
    =+  seg-length=(div columns 4)
    ::
    =/  buffer=(list (list @))
      (reap threads (reap columns 0))
    ::
    ::  main function
    ::
    ::  msg: the main input
    ::  sat: optional salt
    |=  [msg=byts sat=byts]
    ^-  @
    ::
    ::  h0: initial 64-byte block
    =/  h0=@
      =-  (blake2b - 0^0 64)
      :-  :(add 40 wid.msg wid.sat wid.key wid.extra)
      %+  can  3
      =+  (cury (cury rev 3) 4)
      :~  (prep-wid extra)
          (prep-wid key)
          (prep-wid sat)
          (prep-wid msg)
          4^(- (type-to-num typ))
          4^(- version)
          4^(- time-cost)
          4^(- mem-cost)
          4^(- out)
          4^(- threads)
      ==
    ::
    ::  do time-cost passes over the buffer
    ::
    =+  t=0
    |-
    ?:  (lth t time-cost)
      ::
      ::  process all four segments in the columns...
      ::
      =+  s=0
      |-
      ?.  (lth s 4)  ^$(t +(t))
      ::
      ::  ...of every row/thread
      ::
      =+  r=0
      |-
      ?.  (lth r threads)  ^$(s +(s))
      =;  new=_buffer
        $(buffer new, r +(r))
      %-  fill-segment
      :*  buffer   h0
          t        s          r
          blocks   columns    seg-length
          threads  time-cost  typ         version
      ==
    ::
    ::  mix all rows together and hash the result
    ::
    =+  r=0
    =|  final=@
    |-
    ?:  =(r threads)
      (hash 1.024^final out)
    =-  $(final -, r +(r))
    %+  mix  final
    (snag (dec columns) (snag r buffer))
  ::
  ::  per-segment computation
  ++  fill-segment
    |=  $:  buffer=(list (list @))
            h0=@
          ::
            itn=@ud
            seg=@ud
            row=@ud
          ::
            blocks=@ud
            columns=@ud
            seg-length=@ud
          ::
            threads=@ud
            time-cost=@ud
            typ=argon-type
            version=@ux
        ==
    ::
    ::  fill-segment utilities
    ::
    =>  |%
        ++  put-word
          |=  [rob=(list @) i=@ud d=@]
          %+  weld  (scag i rob)
          [d (slag +(i) rob)]
        --
    ^+  buffer
    ::
    ::  rob:   row buffer to operate on
    ::  do-i:  whether to use prns from input rather than state
    ::  rands: prns generated from input, if we do-i
    =+  rob=(snag row buffer)
    =/  do-i=?
      ?|  ?=(%i typ)
          &(?=(%id typ) =(0 itn) (lte seg 1))
          &(?=(%u typ) =(0 itn) (lte seg 2))
      ==
    =/  rands=(list (pair @ @))
      ?.  do-i  ~
      ::
      ::  keep going until we have a list of :seg-length prn pairs
      ::
      =+  l=0
      =+  counter=1
      |-  ^-  (list (pair @ @))
      ?:  (gte l seg-length)  ~
      =-  (weld - $(counter +(counter), l (add l 128)))
      ::
      ::  generate pseudorandom block by compressing metadata
      ::
      =/  random-block=@
        %+  compress  0
        %+  compress  0
        %^  lsh  3  968
        %+  rep  6
        =+  (cury (cury rev 3) 8)
        :~  (- counter)
            (- (type-to-num typ))
            (- time-cost)
            (- blocks)
            (- seg)
            (- row)
            (- itn)
        ==
      ::
      ::  split the random-block into 64-bit sections,
      ::  then extract the first two 4-byte sections from each.
      ::
      %+  turn  (flop (rip 6 random-block))
      |=  a=@
      ^-  (pair @ @)
      :-  (rev 3 4 (rsh 5 1 a))
      (rev 3 4 (end 5 1 a))
    ::
    ::  iterate over the entire segment length
    ::
    =+  sin=0
    |-
    ::
    ::  when done, produce the updated buffer
    ::
    ?:  =(sin seg-length)
      %+  weld  (scag row buffer)
      [rob (slag +(row) buffer)]
    ::
    ::  col: current column to process
    =/  col=@ud
      (add (mul seg seg-length) sin)
    ::
    ::  first two columns are generated from h0
    ::
    ?:  &(=(0 itn) (lth col 2))
      =+  (app-num (app-num 64^h0 col) row)
      =+  (hash - 1.024)
      $(rob (put-word rob col -), sin +(sin))
    ::
    ::  c1, c2: prns for picking reference block
    =+  ^-  [c1=@ c2=@]  ::TODO  =/ w/o face
      ?:  do-i  (snag sin rands)
      =+  =-  (snag - rob)
          ?:  =(0 col)  (dec columns)
          (mod (dec col) columns)
      :-  (rev 3 4 (cut 3 [1.020 4] -))
      (rev 3 4 (cut 3 [1.016 4] -))
    ::
    ::  ref-row: reference block row
    =/  ref-row=@ud
      ?:  &(=(0 itn) =(0 seg))  row
      (mod c2 threads)
    ::
    ::  ref-col: reference block column
    =/  ref-col=@ud
      =-  (mod - columns)
      %+  add
        ::  starting index
        ?:  |(=(0 itn) =(3 seg))  0
        (mul +(seg) seg-length)
      ::  pseudorandom offset
      =-  %+  sub  (dec -)
          %^  rsh  0  32
          %+  mul  -
          (rsh 0 32 (mul c1 c1))
      ::  reference area size
      ?:  =(0 itn)
        ?:  |(=(0 seg) =(row ref-row))  (dec col)
        ?:  =(0 sin)  (dec (mul seg seg-length))
        (mul seg seg-length)
      =+  sul=(sub columns seg-length)
      ?:  =(ref-row row)   (dec (add sul sin))
      ?:  =(0 sin)  (dec sul)
      sul
    ::
    ::  compress the previous and reference block
    ::  to create the new block
    ::
    =/  new=@
      %+  compress
        =-  (snag - rob)
        ::  previous index, wrap-around
        ?:  =(0 col)  (dec columns)
        (mod (dec col) columns)
      ::  get reference block
      %+  snag  ref-col
      ?:  =(ref-row row)  rob
      (snag ref-row buffer)
    ::
    ::  starting from v1.3, we xor the new block in,
    ::  rather than directly overwriting the old block
    ::
    =?  new  &(!=(0 itn) =(0x13 version))
      (mix new (snag col rob))
    $(rob (put-word rob col new), sin +(sin))
  ::
  ::  compression function (g)
  ++  compress
    ::  x, y: assumed to be 1024 bytes
    |=  [x=@ y=@]
    ^-  @
    ::
    =+  r=(mix x y)
    =|  q=(list @)
    ::
    ::  iterate over rows of r to get q
    ::
    =+  i=0
    |-
    ?:  (lth i 8)
      =;  p=(list @)
        $(q (weld q p), i +(i))
      %-  permute
      =-  (weld (reap (sub 8 (lent -)) 0) -)
      %-  flop
      %+  rip  7
      (cut 10 [(sub 7 i) 1] r)
    ::
    ::  iterate over columns of q to get z
    ::
    =/  z=(list @)  (reap 64 0)
    =.  i  0
    |-
    ::
    ::  when done, assemble z and xor it with r
    ::
    ?.  (lth i 8)
      (mix (rep 7 (flop z)) r)
    ::
    ::  permute the column
    ::
    =/  out=(list @)
      %-  permute
      :~  (snag i q)
          (snag (add i 8) q)
          (snag (add i 16) q)
          (snag (add i 24) q)
          (snag (add i 32) q)
          (snag (add i 40) q)
          (snag (add i 48) q)
          (snag (add i 56) q)
      ==
    ::
    ::  put the result into z per column
    ::
    =+  j=0
    |-
    ?:  =(8 j)  ^$(i +(i))
    =-  $(z -, j +(j))
    =+  (add i (mul j 8))
    %+  weld  (scag - z)
    [(snag j out) (slag +(-) z)]
  ::
  ::  permutation function (p)
  ++  permute
    ::NOTE  this function really just takes and produces
    ::      8 values, but taking and producing them as
    ::      lists helps clean up the code significantly.
    |=  s=(list @)
    ?>  =(8 (lent s))
    ^-  (list @)
    ::
    ::  list inputs as 16 8-byte values
    ::
    =/  v=(list @)
      %-  zing
      ^-  (list (list @))
      %+  turn  s
      |=  a=@
      ::  rev for endianness
      =+  (rip 6 (rev 3 16 a))
      (weld - (reap (sub 2 (lent -)) 0))
    ::
    ::  do permutation rounds
    ::
    =.  v  (do-round v 0 4 8 12)
    =.  v  (do-round v 1 5 9 13)
    =.  v  (do-round v 2 6 10 14)
    =.  v  (do-round v 3 7 11 15)
    =.  v  (do-round v 0 5 10 15)
    =.  v  (do-round v 1 6 11 12)
    =.  v  (do-round v 2 7 8 13)
    =.  v  (do-round v 3 4 9 14)
    ::  rev for endianness
    =.  v  (turn v (cury (cury rev 3) 8))
    ::
    ::  cat v back together into 8 16-byte values
    ::
    %+  turn  (gulf 0 7)
    |=  i=@
    =+  (mul 2 i)
    (cat 6 (snag +(-) v) (snag - v))
  ::
  ::  perform a round and produce updated value list
  ++  do-round
    |=  [v=(list @) na=@ nb=@ nc=@ nd=@]
    ^+  v
    =>  |%
        ++  get-word
          |=  i=@ud
          (snag i v)
        ::
        ++  put-word
          |=  [i=@ud d=@]
          ^+  v
          %+  weld  (scag i v)
          [d (slag +(i) v)]
        --
    =-  =.  v  (put-word na a)
        =.  v  (put-word nb b)
        =.  v  (put-word nc c)
               (put-word nd d)
    %-  round
    :*  (get-word na)
        (get-word nb)
        (get-word nc)
        (get-word nd)
    ==
  ::
  ::  perform a round (bg) and produce updated values
  ++  round
    |=  [a=@ b=@ c=@ d=@]
    ^-  [a=@ b=@ c=@ d=@]
    ::  operate on 64 bit words
    =+  fed=~(. fe 6)
    =*  sum  sum:fed
    =*  ror  ror:fed
    =+  end=(cury (cury end 5) 1)
    =.  a  :(sum a b :(mul 2 (end a) (end b)))
    =.  d  (ror 0 32 (mix d a))
    =.  c  :(sum c d :(mul 2 (end c) (end d)))
    =.  b  (ror 0 24 (mix b c))
    =.  a  :(sum a b :(mul 2 (end a) (end b)))
    =.  d  (ror 0 16 (mix d a))
    =.  c  :(sum c d :(mul 2 (end c) (end d)))
    =.  b  (ror 0 63 (mix b c))
    [a b c d]
  ::
  ::  argon2 wrapper around blake2b (h')
  ++  hash
    |=  [byts out=@ud]
    ^-  @
    ::
    ::  msg: input with byte-length prepended
    =+  msg=(prep-num [wid dat] out)
    ::
    ::  if requested size is low enough, hash directly
    ::
    ?:  (lte out 64)
      (blake2b msg 0^0 out)
    ::
    ::  build up the result by hashing and re-hashing
    ::  the input message, adding the first 32 bytes
    ::  of the hash to the result, until we have the
    ::  desired output size.
    ::
    =+  tmp=(blake2b msg 0^0 64)
    =+  res=(rsh 3 32 tmp)
    =.  out  (sub out 32)
    |-
    ?:  (gth out 64)
      =.  tmp  (blake2b 64^tmp 0^0 64)
      =.  res  (add (lsh 3 32 res) (rsh 3 32 tmp))
      $(out (sub out 32))
    %+  add  (lsh 3 out res)
    (blake2b 64^tmp 0^0 out)
  ::
  ::  utilities
  ::
  ++  type-to-num
    |=  t=argon-type
    ?-  t
      %d    0
      %i    1
      %id   2
      %u   10
    ==
  ::
  ++  app-num
    |=  [byts num=@ud]
    ^-  byts
    :-  (add wid 4)
    %+  can  3
    ~[4^(rev 3 4 num) wid^dat]
  ::
  ++  prep-num
    |=  [byts num=@ud]
    ^-  byts
    :-  (add wid 4)
    %+  can  3
    ~[wid^dat 4^(rev 3 4 num)]
  ::
  ++  prep-wid
    |=  a=byts
    (prep-num a wid.a)
  --
::
::  blake2
::
::TODO  generalize for both blake2 variants
++  blake2b
  |=  [msg=byts key=byts out=@ud]
  ^-  @
  ::  initialization vector
  =/  iv=@
    0x6a09.e667.f3bc.c908.
      bb67.ae85.84ca.a73b.
      3c6e.f372.fe94.f82b.
      a54f.f53a.5f1d.36f1.
      510e.527f.ade6.82d1.
      9b05.688c.2b3e.6c1f.
      1f83.d9ab.fb41.bd6b.
      5be0.cd19.137e.2179
  ::  per-round constants
  =/  sigma=(list (list @ud))
    :~  :~   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  ==
        :~  14  10   4   8   9  15  13   6   1  12   0   2  11   7   5   3  ==
        :~  11   8  12   0   5   2  15  13  10  14   3   6   7   1   9   4  ==
        :~   7   9   3   1  13  12  11  14   2   6   5  10   4   0  15   8  ==
        :~   9   0   5   7   2   4  10  15  14   1  11  12   6   8   3  13  ==
        :~   2  12   6  10   0  11   8   3   4  13   7   5  15  14   1   9  ==
        :~  12   5   1  15  14  13   4  10   0   7   6   3   9   2   8  11  ==
        :~  13  11   7  14  12   1   3   9   5   0  15   4   8   6   2  10  ==
        :~   6  15  14   9  11   3   0   8  12   2  13   7   1   4  10   5  ==
        :~  10   2   8   4   7   6   1   5  15  11   9  14   3  12  13   0  ==
        :~   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  ==
        :~  14  10   4   8   9  15  13   6   1  12   0   2  11   7   5   3  ==
    ==
  =>  |%
      ++  get-word-list
        |=  [h=@ w=@ud]
        ^-  (list @)
        %-  flop
        =+  l=(rip 6 h)
        =-  (weld - l)
        (reap (sub w (lent l)) 0)
      ::
      ++  get-word
        |=  [h=@ i=@ud w=@ud]
        ^-  @
        %+  snag  i
        (get-word-list h w)
      ::
      ++  put-word
        |=  [h=@ i=@ud w=@ud d=@]
        ^-  @
        %+  rep  6
        =+  l=(get-word-list h w)
        %-  flop
        %+  weld  (scag i l)
        [d (slag +(i) l)]
      ::
      ++  mod-word
        |*  [h=@ i=@ud w=@ud g=$-(@ @)]
        (put-word h i w (g (get-word h i w)))
      ::
      ++  pad
        |=  [byts len=@ud]
        (lsh 3 (sub len wid) dat)
      ::
      ++  compress
        |=  [h=@ c=@ t=@ud l=?]
        ^-  @
        ::  set up local work vector
        =+  v=(add (lsh 6 8 h) iv)
        ::  xor the counter t into v
        =.  v
          %-  mod-word
          :^  v  12  16
          (cury mix (end 0 64 t))
        =.  v
          %-  mod-word
          :^  v  13  16
          (cury mix (rsh 0 64 t))
        ::  for the last block, invert v14
        =?  v  l
          %-  mod-word
          :^  v  14  16
          (cury mix 0xffff.ffff.ffff.ffff)
        ::  twelve rounds of message mixing
        =+  i=0
        =|  s=(list @)
        |^
          ?:  =(i 12)
            ::  xor upper and lower halves of v into state h
            =.  h  (mix h (rsh 6 8 v))
            (mix h (end 6 8 v))
          ::  select message mixing schedule and mix v
          =.  s  (snag (mod i 10) sigma)
          =.  v  (do-mix 0 4 8 12 0 1)
          =.  v  (do-mix 1 5 9 13 2 3)
          =.  v  (do-mix 2 6 10 14 4 5)
          =.  v  (do-mix 3 7 11 15 6 7)
          =.  v  (do-mix 0 5 10 15 8 9)
          =.  v  (do-mix 1 6 11 12 10 11)
          =.  v  (do-mix 2 7 8 13 12 13)
          =.  v  (do-mix 3 4 9 14 14 15)
          $(i +(i))
        ::
        ++  do-mix
          |=  [na=@ nb=@ nc=@ nd=@ nx=@ ny=@]
          ^-  @
          =-  =.  v  (put-word v na 16 a)
              =.  v  (put-word v nb 16 b)
              =.  v  (put-word v nc 16 c)
                     (put-word v nd 16 d)
          %-  b2mix
          :*  (get-word v na 16)
              (get-word v nb 16)
              (get-word v nc 16)
              (get-word v nd 16)
              (get-word c (snag nx s) 16)
              (get-word c (snag ny s) 16)
          ==
        --
      ::
      ++  b2mix
        |=  [a=@ b=@ c=@ d=@ x=@ y=@]
        ^-  [a=@ b=@ c=@ d=@]
        =.  x  (rev 3 8 x)
        =.  y  (rev 3 8 y)
        =+  fed=~(. fe 6)
        =.  a  :(sum:fed a b x)
        =.  d  (ror:fed 0 32 (mix d a))
        =.  c  (sum:fed c d)
        =.  b  (ror:fed 0 24 (mix b c))
        =.  a  :(sum:fed a b y)
        =.  d  (ror:fed 0 16 (mix d a))
        =.  c  (sum:fed c d)
        =.  b  (ror:fed 0 63 (mix b c))
        [a b c d]
      --
  ::  ensure inputs adhere to contraints
  =.  out  (max 1 (min out 64))
  =.  wid.msg  (min wid.msg (bex 128))
  =.  wid.key  (min wid.key 64)
  =.  dat.msg  (end 3 wid.msg dat.msg)
  =.  dat.key  (end 3 wid.key dat.key)
  ::  initialize state vector
  =+  h=iv
  ::  mix key length and output length into h0
  =.  h
    %-  mod-word
    :^  h  0  8
    %+  cury  mix
    %+  add  0x101.0000
    (add (lsh 3 1 wid.key) out)
  ::  keep track of how much we've compressed
  =*  mes  dat.msg
  =+  com=0
  =+  rem=wid.msg
  ::  if we have a key, pad it and prepend to msg
  =?  mes  (gth wid.key 0)
    (can 3 ~[rem^mes 128^(pad key 128)])
  =?  rem  (gth wid.key 0)
    (add rem 128)
  |-
  ::  compress 128-byte chunks of the message
  ?:  (gth rem 128)
    =+  c=(cut 3 [(sub rem 128) 128] mes)
    =.  com   (add com 128)
    %_  $
      rem   (sub rem 128)
      h     (compress h c com |)
    ==
  ::  compress the final bytes of the msg
  =+  c=(cut 3 [0 rem] mes)
  =.  com  (add com rem)
  =.  c  (pad [rem c] 128)
  =.  h  (compress h c com &)
  ::  produce output of desired length
  %^  rsh  3  (sub 64 out)
  ::  do some word
  %+  rep  6
  %+  turn  (flop (gulf 0 7))
  |=  a=@
  (rev 3 8 (get-word h a 8))
::
++  hash160
  |=  d=@
  (ripemd-160 256 (sha-256:sha d))
::
::  ripemd
::
++  md5-pad
  |=  byts
  ^-  byts
  =+  (sub 511 (mod (add wid 64) 512))
  :-  :(add 64 +(-) wid)
  %+  can  0
  ~[64^(rev 3 8 wid) +(-)^(lsh 0 - 1) wid^dat]
::
::NOTE  verified correct against:
::      http://homes.esat.kuleuven.be/~bosselae/ripemd160.html
++  ripemd-160
  ::  w: data size in bits
  ::  d: data to hash
  |=  byts
  ^-  @
  ::  add padding
  =+  (md5-pad wid dat)
  ::  endianness
  =.  dat
    %+  rep  5
    %+  turn  (rip 5 dat)
    |=(a=@ (rev 3 4 a))
  =*  x  dat
  =+  blocks=(div wid 512)
  =+  fev=~(. fe 5)
  ::  initial register values
  =+  h0=0x6745.2301
  =+  h1=0xefcd.ab89
  =+  h2=0x98ba.dcfe
  =+  h3=0x1032.5476
  =+  h4=0xc3d2.e1f0
  ::  i: current block
  =+  [i=0 j=0]
  =+  *[a=@ b=@ c=@ d=@ e=@]       ::  a..e
  =+  *[aa=@ bb=@ cc=@ dd=@ ee=@]  ::  a'..e'
  |^
    ?:  =(i blocks)
      %+  rep  5
      %+  turn  `(list @)`~[h4 h3 h2 h1 h0]
      ::  endianness
      |=(h=@ (rev 3 4 h))
    =:  a  h0     aa  h0
        b  h1     bb  h1
        c  h2     cc  h2
        d  h3     dd  h3
        e  h4     ee  h4
    ==
    ::  j: current word
    =+  j=0
    |-
    ?:  =(j 80)
      %=  ^$
        i   +(i)
        h1  :(sum:fev h2 d ee)
        h2  :(sum:fev h3 e aa)
        h3  :(sum:fev h4 a bb)
        h4  :(sum:fev h0 b cc)
        h0  :(sum:fev h1 c dd)
      ==
    %=  $
      j  +(j)
    ::
      a   e
      b   (fn j a b c d e (get (r j)) (k j) (s j))
      c   b
      d   (rol 10 c)
      e   d
    ::
      aa  ee
      bb  (fn (sub 79 j) aa bb cc dd ee (get (rr j)) (kk j) (ss j))
      cc  bb
      dd  (rol 10 cc)
      ee  dd
    ==
  ::
  ++  get  ::  word from x in block i
    |=  j=@ud
    =+  (add (mul i 16) +(j))
    (cut 5 [(sub (mul blocks 16) -) 1] x)
  ::
  ++  fn
    |=  [j=@ud a=@ b=@ c=@ d=@ e=@ m=@ k=@ s=@]
    =-  (sum:fev (rol s :(sum:fev a m k -)) e)
    =.  j  (div j 16)
    ?:  =(0 j)  (mix (mix b c) d)
    ?:  =(1 j)  (con (dis b c) (dis (not 0 32 b) d))
    ?:  =(2 j)  (mix (con b (not 0 32 c)) d)
    ?:  =(3 j)  (con (dis b d) (dis c (not 0 32 d)))
    ?:  =(4 j)  (mix b (con c (not 0 32 d)))
    !!
  ::
  ++  rol  (cury rol:fev 0)
  ::
  ++  k
    |=  j=@ud
    =.  j  (div j 16)
    ?:  =(0 j)  0x0
    ?:  =(1 j)  0x5a82.7999
    ?:  =(2 j)  0x6ed9.eba1
    ?:  =(3 j)  0x8f1b.bcdc
    ?:  =(4 j)  0xa953.fd4e
    !!
  ::
  ++  kk  ::  k'
    |=  j=@ud
    =.  j  (div j 16)
    ?:  =(0 j)  0x50a2.8be6
    ?:  =(1 j)  0x5c4d.d124
    ?:  =(2 j)  0x6d70.3ef3
    ?:  =(3 j)  0x7a6d.76e9
    ?:  =(4 j)  0x0
    !!
  ::
  ++  r
    |=  j=@ud
    %+  snag  j
    ^-  (list @)
    :~  0  1  2  3  4  5  6  7  8  9  10  11  12  13  14  15
        7  4  13  1  10  6  15  3  12  0  9  5  2  14  11  8
        3  10  14  4  9  15  8  1  2  7  0  6  13  11  5  12
        1  9  11  10  0  8  12  4  13  3  7  15  14  5  6  2
        4  0  5  9  7  12  2  10  14  1  3  8  11  6  15  13
    ==
  ::
  ++  rr  ::  r'
    |=  j=@ud
    %+  snag  j
    ^-  (list @)
    :~  5  14  7  0  9  2  11  4  13  6  15  8  1  10  3  12
        6  11  3  7  0  13  5  10  14  15  8  12  4  9  1  2
        15  5  1  3  7  14  6  9  11  8  12  2  10  0  4  13
        8  6  4  1  3  11  15  0  5  12  2  13  9  7  10  14
        12  15  10  4  1  5  8  7  6  2  13  14  0  3  9  11
    ==
  ::
  ++  s
    |=  j=@ud
    %+  snag  j
    ^-  (list @)
    :~  11  14  15  12  5  8  7  9  11  13  14  15  6  7  9  8
        7  6  8  13  11  9  7  15  7  12  15  9  11  7  13  12
        11  13  6  7  14  9  13  15  14  8  13  6  5  12  7  5
        11  12  14  15  14  15  9  8  9  14  5  6  8  6  5  12
        9  15  5  11  6  8  13  12  5  12  13  14  11  8  5  6
    ==
  ::
  ++  ss  ::  s'
    |=  j=@ud
    %+  snag  j
    ^-  (list @)
    :~  8  9  9  11  13  15  15  5  7  7  8  11  14  14  12  6
        9  13  15  7  12  8  9  11  7  7  12  7  6  15  13  11
        9  7  15  11  8  6  6  14  12  13  5  14  13  13  7  5
        15  5  8  11  14  14  6  14  6  9  12  9  12  5  15  8
        8  5  12  9  12  5  14  6  8  13  6  5  15  13  11  11
    ==
  --
::
::  hmac
::
::TODO  ++hmc/hml returns reverse byte order results,
::      so does ++pbk/pbl which depends on it,
::      but not secp, which also depends on them
::NOTE  tested to be correct against https://tools.ietf.org/html/rfc4231
++  hmac  ::  correct byte-order hmac-family
  =,  sha
  |%
  ++  meet  |=([k=@ m=@] [[(met 3 k) k] [(met 3 m) m]])
  ::
  ++  hmac-sha1    (cork meet hmac-sha1l)
  ++  hmac-sha256  (cork meet hmac-sha256l)
  ++  hmac-sha512  (cork meet hmac-sha512l)
  ::
  ++  hmac-sha1l    (cury hmac sha-1l 64 20)
  ++  hmac-sha256l  (cury hmac sha-256l 64 32)
  ++  hmac-sha512l  (cury hmac sha-512l 128 64)
  ::
  ++  hmac
    ::  boq: block size in bytes used by haj
    ::  out: bytes output by haj
    |*  [[haj=$-([@u @] @) boq=@u out=@u] key=byts msg=byts]
    ::  ensure key and message fit signaled lengths
    ::TODO  other crypto implementations should do this too, probably
    =.  dat.key  (end 3 wid.key dat.key)
    =.  dat.msg  (end 3 wid.msg dat.msg)
    ::  keys longer than block size are shortened by hashing
    =?  dat.key  (gth wid.key boq)  (haj wid.key dat.key)
    =?  wid.key  (gth wid.key boq)  out
    ::  keys shorter than block size are right-padded
    =?  dat.key  (lth wid.key boq)  (lsh 3 (sub boq wid.key) dat.key)
    ::  pad key, inner and outer
    =+  kip=(mix dat.key (fil 3 boq 0x36))
    =+  kop=(mix dat.key (fil 3 boq 0x5c))
    ::  append inner padding to message, then hash
    =+  (haj (add wid.msg boq) (add (lsh 3 wid.msg kip) dat.msg))
    ::  prepend outer padding to result, hash again
    (haj (add out boq) (add (lsh 3 out kop) -))
  --
::
++  sha  ::  correct byte-order sha-family
  |%
  ++  sha-1f    (cork flin shan)
  ++  sha-1     (cork meet sha-1l)
  ::
  ++  sha-256   :(cork flin shax (flip 32))
  ++  sha-256l  :(cork flim shay (flip 32))
  ::
  ++  sha-512   :(cork flin shaz (flip 64))
  ++  sha-512l  :(cork flim shal (flip 64))
  ::
  ++  flin      |=(a=@ (swp 3 a))                       ::  flip input
  ++  flim      |=(byts [wid (rev 3 wid dat)])          ::  flip input w/ length
  ++  flip      |=(w=@u (cury (cury rev 3) w))          ::  flip output of size
  ++  meet      |=(a=@ [(met 3 a) a])
  ::
  ++  sha-1l
    |=  byts
    =+  [few==>(fe .(a 5)) wac=|=({a/@ b/@} (cut 5 [a 1] b))]
    =+  [sum=sum.few ror=ror.few rol=rol.few net=net.few inv=inv.few]
    =+  ral=(lsh 0 3 wid)
    =+  ^=  ful
        %+  can  0
        :~  [ral (rev 3 wid dat)]
            [8 128]
            [(mod (sub 960 (mod (add 8 ral) 512)) 512) 0]
            [64 (~(net fe 6) ral)]
        ==
    =+  lex=(met 9 ful)
    =+  kbx=0xca62.c1d6.8f1b.bcdc.6ed9.eba1.5a82.7999
    =+  hax=0xc3d2.e1f0.1032.5476.98ba.dcfe.efcd.ab89.6745.2301
    =+  i=0
    |-
    ?:  =(i lex)
      (rep 5 (flop (rip 5 hax)))
    =+  ^=  wox
        =+  dux=(cut 9 [i 1] ful)
        =+  wox=(rep 5 (turn (rip 5 dux) net))
        =+  j=16
        |-  ^-  @
        ?:  =(80 j)
          wox
        =+  :*  l=(wac (sub j 3) wox)
                m=(wac (sub j 8) wox)
                n=(wac (sub j 14) wox)
                o=(wac (sub j 16) wox)
            ==
        =+  z=(rol 0 1 :(mix l m n o))
        $(wox (con (lsh 5 j z) wox), j +(j))
    =+  j=0
    =+  :*  a=(wac 0 hax)
            b=(wac 1 hax)
            c=(wac 2 hax)
            d=(wac 3 hax)
            e=(wac 4 hax)
        ==
    |-  ^-  @
    ?:  =(80 j)
      %=  ^$
        i  +(i)
        hax  %+  rep  5
             :~
                 (sum a (wac 0 hax))
                 (sum b (wac 1 hax))
                 (sum c (wac 2 hax))
                 (sum d (wac 3 hax))
                 (sum e (wac 4 hax))
             ==
      ==
    =+  fx=(con (dis b c) (dis (not 5 1 b) d))
    =+  fy=:(mix b c d)
    =+  fz=:(con (dis b c) (dis b d) (dis c d))
    =+  ^=  tem
        ?:  &((gte j 0) (lte j 19))
          :(sum (rol 0 5 a) fx e (wac 0 kbx) (wac j wox))
        ?:  &((gte j 20) (lte j 39))
          :(sum (rol 0 5 a) fy e (wac 1 kbx) (wac j wox))
        ?:  &((gte j 40) (lte j 59))
          :(sum (rol 0 5 a) fz e (wac 2 kbx) (wac j wox))
        :(sum (rol 0 5 a) fy e (wac 3 kbx) (wac j wox))
    $(j +(j), a tem, b a, c (rol 0 30 b), d c, e d)
  --
::
::
++  secp
  |%
  +=  jaco  [x=@ y=@ z=@]                               ::  jacobian point
  +=  pont  [x=@ y=@]                                   ::  curve point
  ::
  ++  secp192k1  ::TODO  unverified
    %+  secp  24
    :*  p=0xffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.
            ffff.ffff.ffff.ffff.ffff.fffe.ffff.ee37
        a=0
        b=3
        ^=  g
        :*  x=0xdb4f.f10e.c057.e9ae.26b0.7d02.
                80b7.f434.1da5.d1b1.eae0.6c7d
            y=0x9b2f.2f6d.9c56.28a7.8441.63d0.
                15be.8634.4082.aa88.d95e.2f9d
        ==
        n=0xffff.ffff.ffff.ffff.ffff.fffe.
            26f2.fc17.0f69.466a.74de.fd8d
    ==
  ::
  ++  secp192r1  ::TODO  incorrect
    %+  secp  24
    :*  p=0xffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.
            ffff.ffff.ffff.ffff.ffff.fffe.ffff.fc2f
        a=0xffff.ffff.ffff.ffff.ffff.ffff.
            ffff.fffe.ffff.ffff.ffff.fffc
        b=0x6421.0519.e59c.80e7.0fa7.e9ab.
            7224.3049.feb8.deec.c146.b9b1
        ^=  g
        :*  x=0x188d.a80e.b030.90f6.7cbf.20eb.
                43a1.8800.f4ff.0afd.82ff.1012
             y=0x719.2b95.ffc8.da78.6310.11ed.
                6b24.cdd5.73f9.77a1.1e79.4811
        ==
        n=0xffff.ffff.ffff.ffff.ffff.ffff.
            99de.f836.146b.c9b1.b4d2.2831
    ==
  ::
  ::TODO  more
  ::
  ++  secp256k1  ::NOTE  verified correct
    %+  secp  32
    :*  p=0xffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.    ::  modulo
            ffff.ffff.ffff.ffff.ffff.fffe.ffff.fc2f
        a=0                                             ::  y^2=x^3+ax+b
        b=7
        ^=  g                                           ::  "prime" point
        :*  x=0x79be.667e.f9dc.bbac.55a0.6295.ce87.0b07.
                029b.fcdb.2dce.28d9.59f2.815b.16f8.1798
            y=0x483a.da77.26a3.c465.5da4.fbfc.0e11.08a8.
                fd17.b448.a685.5419.9c47.d08f.fb10.d4b8
        ==
        n=0xffff.ffff.ffff.ffff.ffff.ffff.ffff.fffe.    ::  prime order of g
            baae.dce6.af48.a03b.bfd2.5e8c.d036.4141
    ==
  ::
  ++  secp256r1  ::TODO  incorrect
    %+  secp  32
    :*  p=0xffff.ffff.0000.0001.0000.0000.0000.0000.
            0000.0000.ffff.ffff.ffff.ffff.ffff.ffff
        a=0xffff.ffff.0000.0001.0000.0000.0000.0000.
            0000.0000.ffff.ffff.ffff.ffff.ffff.fffc
        b=0x5ac6.35d8.aa3a.93e7.b3eb.bd55.7698.86bc.
            651d.06b0.cc53.b0f6.3bce.3c3e.27d2.604b
        ^=  g
        :*  x=0x6b17.d1f2.e12c.4247.f8bc.e6e5.63a4.40f2.
                7703.7d81.2deb.33a0.f4a1.3945.d898.c296
            y=0x4fe3.42e2.fe1a.7f9b.8ee7.eb4a.7c0f.9e16.
                2bce.3357.6b31.5ece.cbb6.4068.37bf.51f5
        ==
        n=0xffff.ffff.0000.0000.ffff.ffff.ffff.ffff.
            bce6.faad.a717.9e84.f3b9.cac2.fc63.2551
     ==
  ::
  ++  secp
    |=  [w=@ p=@ a=@ b=@ g=pont n=@]
    =/  p  ~(. fo p)
    =/  n  ~(. fo n)
    |%
    ++  compress-point
      |=  pont
      ^-  @
      (can 3 ~[w^x 1^(add 0x2 (cut 0 [0 1] y))])
    ::
    ++  serialize-point
      |=  pont
      ^-  @
      (can 3 ~[w^y w^x 1^0x4])
    ::
    ++  decompress-point
      |=  dat=@
      ^-  pont
      =+  x=(end 3 w a)
      =+  y=:(add (pow x 3) (mul a x) b)
      =+  s=(rsh 3 32 dat)
      :-  x
      ?:  =(0x2 s)  y
      ?:  =(0x3 s)  y
      ~|  [`@ux`s `@ux`dat]
      !!
    ::
    ++  priv-to-pub                                     ::  get pub from priv
      |=  prv=@
      ^-  pont
      (jc-mul g prv)
    ::
    ++  hmc                                             ::  hmac swap endianness
      |=  [k=@ kl=@ t=@ tl=@]
      ^-  @
      (swp 3 (hml:scr:crypto (swp 3 k) kl (swp 3 t) tl))
    ::
    ++  make-k                                          ::  deterministic nonce
      =,  mimes:html
      |=  [has=@uvI prv=@]
      ^-  @
      =/  v  (fil 3 w 1)
      =/  k  0
      =.  k  (hmc k w [+ -]:(as-octs (can 3 [w has] [w prv] [1 0x0] [w v] ~)))
      =.  v  (hmc k w v w)
      =.  k  (hmc k w [+ -]:(as-octs (can 3 [w has] [w prv] [1 0x1] [w v] ~)))
      =.  v  (hmc k w v w)
      (hmc k w v w)
    ::
    ++  ecdsa-raw-sign                                  ::  generate signature
      |=  [has=@uvI prv=@]
      ^-  [v=@ r=@ s=@]
      =/  z  has
      =/  k  (make-k has prv)
      =+  [r y]=(jc-mul g k)
      =/  s  (pro.n `@`(inv.n k) `@`(sum.n z (mul r prv))) ::TODO  mul.n?
      =/  big-s  (gte (mul 2 s) ^n)
      :*  v=(add 27 (mix (end 0 1 y) ?:(big-s 1 0)))
          r=r
          s=?.(big-s s (sub ^n s))
      ==
    ::
    ++  ecdsa-raw-recover                               ::  get pubkey from sig
      |=  [has=@uvI sig=[v=@ r=@ s=@]]
      ^-  pont
      ?>  ?&((lte 27 v.sig) (lte v.sig 34))
      =/  x  r.sig
      =/  ysq  (sum.p b (exp.p 3 x))               ::  omits A=0
      =/  bet  (exp.p (div +(^p) 4) ysq)
      =/  y  ?:(=(1 (end 0 1 (mix v.sig bet))) bet (dif.p 0 bet))
      ?>  =(0 (dif.p ysq (pro.p y y)))
      ?<  =(0 (sit.n r.sig))
      ?<  =(0 (sit.n s.sig))
      =/  gz  (mul:jc [x y 1]:g (dif.n 0 has))
      =/  xy  (mul:jc [x y 1] s.sig)
      =/  qr  (add:jc gz xy)
      (from:jc (mul:jc qr (inv.n r.sig)))
    ::
    ++  jc-mul                                              ::  point x scalar
      |=  [a=pont n=@]
      ^-  pont
      (from:jc (mul:jc (into:jc a) n))
    ::
    ++  jc-add                                              ::  add points
      |=  [a=pont b=pont]
      ^-  pont
      (from:jc (add:jc (into:jc a) (into:jc b)))
    ::
    ++  jc                                                  ::  jacobian core
      |%
      ++  add                                               ::  addition
        |=  [a=jaco b=jaco]
        ^-  jaco
        ?:  =(0 y.a)  b
        ?:  =(0 y.b)  a
        =/  u1  :(pro.p x.a z.b z.b)
        =/  u2  :(pro.p x.b z.a z.a)
        =/  s1  :(pro.p y.a z.b z.b z.b)
        =/  s2  :(pro.p y.b z.a z.a z.a)
        ?:  =(u1 u2)
          ?.  =(s1 s2)
            [0 0 1]
          (dub a)
        =/  h  (dif.p u2 u1)
        =/  r  (dif.p s2 s1)
        =/  h2  (pro.p h h)
        =/  h3  (pro.p h2 h)
        =/  u1h2  (pro.p u1 h2)
        =/  nx  (dif.p (pro.p r r) :(sum.p h3 u1h2 u1h2))
        =/  ny  (dif.p (pro.p r (dif.p u1h2 nx)) (pro.p s1 h3))
        =/  nz  :(pro.p h z.a z.b)
        [nx ny nz]
      ::
      ++  dub                                               ::  double
        |=  a=jaco
        ^-  jaco
        ?:  =(0 y.a)
          [0 0 0]
        =/  ysq  (pro.p y.a y.a)
        =/  s  :(pro.p 4 x.a ysq)
        =/  m  :(pro.p 3 x.a x.a)                           ::  omits A=0
        =/  nx  (dif.p (pro.p m m) (sum.p s s))
        =/  ny  (dif.p (pro.p m (dif.p s nx)) :(pro.p 8 ysq ysq))
        =/  nz  :(pro.p 2 y.a z.a)
        [nx ny nz]
      ::
      ++  mul                                               :: jaco x scalar
        |=  [a=jaco n=@]
        ^-  jaco
        ?:  =(0 y.a)
          [0 0 1]
        ?:  =(0 n)
          [0 0 1]
        ?:  =(1 n)
          a
        ?:  (gte n ^^n)
          $(n (mod n ^^n))
        ?:  =(0 (mod n 2))
          (dub $(n (div n 2)))
        (add a (dub $(n (div n 2))))
      ::
      ++  from                                              :: jaco -> point
        |=  a=jaco
        ^-  pont
        =/  z  (inv.p z.a)
        [:(pro.p x.a z z) :(pro.p y.a z z z)]
      ::
      ++  into                                              :: point -> jaco
        |=  pont
        ^-  jaco
        [x y z=1]
      --
    --
  --
--
