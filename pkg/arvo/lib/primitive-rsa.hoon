::  |rsa: primitive, textbook RSA
::
::    Unpadded, unsafe, unsuitable for encryption!
::
|%
::  +key:rsa: rsa public or private key
::
+$  key
  $:  ::  pub:  public parameters (n=modulus, e=pub-exponent)
      ::
      pub=[n=@ux e=@ux]
      ::  sek:  secret parameters (d=private-exponent, p/q=primes)
      ::
      sek=(unit [d=@ux p=@ux q=@ux])
  ==
::  +ramp: make rabin-miller probabilistic prime
::
::    XX replace +ramp:number?
::    a: bitwidth
::    b: snags (XX small primes to check divisibility?)
::    c: entropy
::
++  ramp
  |=  [a=@ b=(list @) c=@]
  =.  c  (shas %ramp c)
  :: XX what is this value?
  ::
  =|  d=@
  |-  ^-  @ux
  :: XX what is this condition?
  ::
  ?:  =((mul 100 a) d)
    ~|(%ar-ramp !!)
  :: e: prime candidate
  ::
  ::   Sets low bit, as prime must be odd.
  ::   Sets high bit, as +raw:og only gives up to :a bits.
  ::
  =/  e  :(con 1 (lsh [0 (dec a)] 1) (~(raw og c) a))
  :: XX what algorithm is this modular remainder check?
  ::
  ?:  ?&  (levy b |=(f=@ !=(1 (mod e f))))
          (pram:number e)
      ==
    e
  $(c +(c), d (shax d))
::  +elcm:rsa: carmichael totient
::
++  elcm
  |=  [a=@ b=@]
  (div (mul a b) d:(egcd a b))
::  +new-key:rsa: write somethingXXX
::
++  new-key
  =/  e  `@ux`65.537
  |=  [wid=@ eny=@]
  ^-  key
  =/  diw  (rsh 0 wid)
  =/  p=@ux  (ramp diw [3 5 ~] eny)
  =/  q=@ux  (ramp diw [3 5 ~] +(eny))
  =/  n=@ux  (mul p q)
  =/  d=@ux  (~(inv fo (elcm (dec p) (dec q))) e)
  [[n e] `[d p q]]
::  +en:rsa: primitive RSA encryption
::
::    ciphertext = message^e (mod n)
::
++  en
  |=  [m=@ k=key]
  ~|  %rsa-len
  ?>  (lte (met 0 m) (met 0 n.pub.k))
  (~(exp fo n.pub.k) e.pub.k m)
::  +de:rsa: primitive RSA decryption
::
::    message = ciphertext^d (mod e)
::
++  de
  |=  [m=@ k=key]
  :: XX assert rsa-len here too?
  ~|  %rsa-need-ring
  ?>  ?=(^ sek.k)
  =/  fu  (fu:number p.u.sek.k q.u.sek.k)
  (out.fu (exp.fu d.u.sek.k (sit.fu m)))
--
