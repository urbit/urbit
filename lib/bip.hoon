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
  ++  hmac-sha256  (cork meet hmac-sha256l)
  ++  hmac-sha512  (cork meet hmac-sha512l)
  ::
  ++  hmac-sha256l  (cury hmac sha-256l 64 32)
  ++  hmac-sha512l  (cury hmac sha-512l 128 64)
  ::
  ++  hmac
    ::  boq: block size used by haj
    ::  out: bytes output by haj
    |*  [[haj=$-([@u @] @) boq=@u out=@u] [kl=@u key=@] [ml=@u msg=@]]
    ::  ensure key and message fit signalled lengths
    =.  key  (end 3 kl key)
    =.  msg  (end 3 ml msg)
    ::  keys longer than block size are shortened by hashing
    =?  key  (gth kl boq)  (haj kl key)
    =?  kl   (gth kl boq)  out
    ::  keys shorter than block size are right-padded
    =?  key  (lth kl boq)  (lsh 3 (sub boq kl) key)
    ::  pad key, inner and outer
    =+  kip=(mix key (fil 3 boq 0x36))
    =+  kop=(mix key (fil 3 boq 0x5c))
    ::  append inner padding to message, then hash
    =+  (haj (add ml boq) (add (lsh 3 boq msg) kip))
    ::  prepend outer padding to result, hash again
    (haj (add out boq) (add (lsh 3 out kop) -))
  --
::
++  sha  ::  correct byte-order sha-family
  |%
  ++  sha-1     (cork flin shan)
  ::
  ++  sha-256   :(cork flin shax (flip 32))
  ++  sha-256l  :(cork flim shay (flip 32))
  ::
  ++  sha-512   :(cork flin shaz (flip 64))
  ++  sha-512l  :(cork flim shal (flip 64))
  ::
  ++  flin      |=(a=@ (swp 3 a))                       ::  flip input
  ++  flim      |=([w=@u a=@] [w (rev 3 w a)])          ::  flip input w/ length
  ++  flip      |=(w=@u (cury (cury rev 3) w))          ::  flip output of size
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
