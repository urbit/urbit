::  bip32 implementation in hoon
::
::  to use, call one of the core initialization arms.
::  using the produced core, derive as needed and take out the data you want.
::
::NOTE  tested to be correct against
::      https://en.bitcoin.it/wiki/BIP_0032_TestVectors
::
=,  hmac:crypto
=,  secp:crypto
=+  ecc=secp256k1
::
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
  =+  v=(swag [1 3] t)
  ?:  =("prv" v)  (from-private k c)
  ?:  =("pub" v)  (from-public k c)
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
  =/  [left=@ right=@]
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
  =/  [left=@ right=@]
    =-  [(cut 3 [32 32] -) (cut 3 [0 32] -)]
    %+  hmac-sha512l  [32 cad]
    37^(can 3 ~[4^i 33^(ser-p pub)])
  ::  rare exception, invalid key, go to the next one
  ?:  (gte left n)  $(i +(i))  ::TODO  or child key is "point at infinity"
  =.  pub  (jc-add.ecc (point left) pub)
  %_  +>.$
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
++  address
  |=  network=?(%main %regtest %testnet)
  ^-  @uc
  ::  removes checksum
  ::
  %^  rsh  3  4
  ::  +en-base58check returns a naked atom instead of a tape
  ::  (i.e. en-base58:mimes:html is moved outside to +en-b58c-bip32)
  ::
  %+  en-base58check
    [4 (version-bytes network %pub %.n)]
  [20 identity]
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
  %-  en-base58:mimes:html
  (en-base58check [4 v] [74 k])
::
::  base58check
::
++  en-base58check
  ::  v: version bytes
  ::  d: data
  |=  [v=byts d=byts]
  =+  p=[(add wid.v wid.d) (can 3 ~[d v])]
  =-  (can 3 ~[4^- p])
  %^  rsh  3  28
  (sha-256l:sha 32 (sha-256l:sha p))
::
++  de-base58check
  ::  vw: amount of version bytes
  |=  [vw=@u t=tape]
  =+  x=(de-base58:mimes:html t)
  =+  hash=(sha-256l:sha 32 (sha-256:sha (rsh 3 4 x)))
  ?>  =((end 3 4 x) (rsh 3 28 hash))
  (cut 3 [vw (sub (met 3 x) (add 4 vw))] x)
::
++  hash160
  |=  d=@
  (ripemd-160:ripemd:crypto 32 (sha-256:sha d))
::
::      tpub (testnet/regtest)         0x435.87cf
::      tpriv (testnet/regtest)        0x435.8394
::      xpub (main):                   0x488.b21e
::      xpriv (main:                   0x488.ade4
::      pubkey hash (testnet/regtest): 0x6f
::      priv hash   (testnet/regtest): 0xef
::
++  version-bytes
  |=  [network=?(%main %regtest %testnet) type=?(%pub %priv) bip32=?]
  ^-  @ux
  |^  ?+  [network type]  ~|(%not-supported-version-bytes !!)
        [?(%regtest %testnet) %pub]   ?:(bip32 xpub-key pay-to-pubkey)
        [?(%regtest %testnet) %priv]  ?:(bip32 xpriv-key private-key)
        [%main %pub]                  ?:(bip32 xpub-key pay-to-pubkey)
        [%main %priv]                 ?:(bip32 xpriv-key private-key)
      ==
  ::
  ++  pay-to-pubkey  ?:(=(type %main) 0x0 0x6f)
  ++  private-key    ?:(=(type %main) 0x80 0xef)
  ++  xpub-key       ?:(=(type %main) 0x435.87cf 0x488.b21e)
  ++  xpriv-key      ?:(=(type %main) 0x435.8394 0x488.ade4)
  --
--
