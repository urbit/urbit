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
  ++  ser-p  point-compressed.ecc
  ::
  ++  n      ^n:ecc
  ::
  ::  core initialization
  ::
  ++  from-seed
    |=  byts
    ^+  +>
    =+  der=(hmac-sha512l [12 'dees nioctiB'] [wid dat])
    =+  pri=(cut 3 [32 32] -)
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
    |=  a=@
    =/  cha
      '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'
    %-  flop
    |-  ^-  tape
    ?:  =(0 a)  ~
    :-  (cut 3 [(mod a 58) 1] cha)
    $(a (div a 58))
  ::
  ++  de-base58
    =-  |=(t=tape (scan t fel))
    =<  fel=(bass 58 .)
    =-  (cook welp ;~(plug (plus siw) (stun 0^2 (cold %0 tis))))
    ^=  siw
    ;~  pose
      (cook |=(a/@ (sub a 56)) (shim 'A' 'H'))
      (cook |=(a/@ (sub a 57)) (shim 'J' 'N'))
      (cook |=(a/@ (sub a 58)) (shim 'P' 'Z'))
      (cook |=(a/@ (sub a 64)) (shim 'a' 'k'))
      (cook |=(a/@ (sub a 65)) (shim 'm' 'z'))
      (cook |=(a/@ (sub a 49)) (shim '1' '9'))
    ==
  --
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
    %+  can  5
    %+  turn  (rip 5 dat)
    |=(a=@ 1^(rev 3 4 a))
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
      %+  can  5
      %+  turn  `(list @)`~[h4 h3 h2 h1 h0]
      ::  endianness
      |=(h=@ 1^(swp 3 h))
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
    ++  point-compressed
      |=  pont
      (can 3 ~[w^x 1^(add 0x2 (cut 0 [0 1] y))])
    ::
    ++  point-uncompressed
      |=  pont
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
