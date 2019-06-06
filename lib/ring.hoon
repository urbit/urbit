::  ring signatures over the edwards curve
::
|%
::  an ugly copy/paste of the private parts of +ed:crypto here
::
++  ed
  =+  ~+
      ::  q: prime modulus of field
      ::
      =+  [b=256 q=(sub (bex 255) 19)]
      =+  fq=~(. fo q)
      ::  l: prime order
      ::
      =+  ^=  l
           %+  add
             (bex 252)
           27.742.317.777.372.353.535.851.937.790.883.648.493
      =+  d=(dif.fq 0 (fra.fq 121.665 121.666))
      =+  ii=(exp.fq (div (dec q) 4) 2)
      [b=b q=q fq=fq l=l d=d ii=ii]
  ::
  |%
  ::                                                ::  ++norm:ed:crypto
  ++  norm                                          ::
    |=(x/@ ?:(=(0 (mod x 2)) x (sub q x)))
  ::                                                ::  ++xrec:ed:crypto
  ++  xrec                                          ::  recover x-coord
    |=  y/@  ^-  @
    =+  ^=  xx
        %+  mul  (dif.fq (mul y y) 1)
                 (inv.fq +(:(mul d y y)))
    =+  x=(exp.fq (div (add 3 q) 8) xx)
    ?:  !=(0 (dif.fq (mul x x) (sit.fq xx)))
      (norm (pro.fq x ii))
    (norm x)
  ::
  ++  ward                                          ::  edwards multiply
    |=  {pp/{@ @} qq/{@ @}}  ^-  {@ @}
    =+  dp=:(pro.fq d -.pp -.qq +.pp +.qq)
    =+  ^=  xt
        %+  pro.fq
          %+  sum.fq
            (pro.fq -.pp +.qq)
          (pro.fq -.qq +.pp)
        (inv.fq (sum.fq 1 dp))
    =+  ^=  yt
        %+  pro.fq
          %+  sum.fq
            (pro.fq +.pp +.qq)
          (pro.fq -.pp -.qq)
        (inv.fq (dif.fq 1 dp))
    [xt yt]
  ::                                                ::  ++scam:ed:crypto
  ++  scam                                          ::  scalar multiply
    |=  {pp/{@ @} e/@}  ^-  {@ @}
    ?:  =(0 e)
      [0 1]
    =+  qq=$(e (div e 2))
    =>  .(qq (ward qq qq))
    ?:  =(1 (dis 1 e))
      (ward qq pp)
    qq
  ::                                                ::  ++curv:ed:crypto
  ++  curv                                          ::  point on curve?
    |=  {x/@ y/@}  ^-  ?
    .=  0
        %+  dif.fq
          %+  sum.fq
            (pro.fq (sub q (sit.fq x)) x)
          (pro.fq y y)
        (sum.fq 1 :(pro.fq d x x y y))
  ::                                                ::  ++deco:ed:crypto
  ++  deco                                          ::  decode point
    |=  s/@  ^-  (unit {@ @})
    =+  y=(cut 0 [0 (dec b)] s)
    =+  si=(cut 0 [(dec b) 1] s)
    =+  x=(xrec y)
    =>  .(x ?:(!=(si (dis 1 x)) (sub q x) x))
    =+  pp=[x y]
    ?.  (curv pp)
      ~
    [~ pp]
  ::  +prime-order: the prime order of the edwards curve
  ::
  ++  l
    ^l
  ::                                                ::  ++bb:ed:crypto
  ++  bb                                            ::
    =+  bby=(pro.fq 4 (inv.fq 5))
    [(xrec bby) bby]
  --
::  +point: point on the ed25519 curve
::
+$  point
  [@ @]
::  +ecc-n: order of the elliptic group curve ed25519
::
++  ecc-n
  ~+
  l:ed
::  +ecc-g: the curve base point of ed25519
::
++  ecc-g
  ~+
  bb:ed
::  +point-mul: scalar multiplication (module operation on elliptic curve)
::
++  point-mul
  |=  [scalar=@ =point]
  (scam:ed point scalar)
::  +point-add: addition (group operation on elliptic curve)
::
++  point-add
  ward:ed
::  +point-base-mul: scalar multiplication over the base point
::
++  point-base-mul
  |=  scalar=@
  (point-mul scalar ecc-g)
::  +oracle: deterministic random response on input
::
++  oracle
  |=  input=*
  (mod (shaz (jam input)) ecc-n)
::
::::
::
::  +generate-public-linkage: generate public linkage information
::
++  generate-public-linkage
  |=  link-scope=*
  ^-  [data=@ h=point]
  ::
  =/  data=@   (oracle link-scope)
  =/  h=point  (point-base-mul data)
  [data h]

::  +generate-linkage: generates linkage information from scope and private key
::
::    data: deterministically picked data point based off scope
::    h:    h = [data] * g
::    y:    y = [x] * h
++  generate-linkage
  |=  [link-scope=(unit *) my-private-key=@]
  ^-  (unit [data=@ h=point y=point])
  ::
  ?~  link-scope
    ~
  ::
  =+  [data=@ h=point]=(generate-public-linkage u.link-scope)
  =/  y=point  (point-mul my-private-key h)
  [~ data h y]
::  +generate-challenge: generate challenge from a given message
::
::    When :link-scope is ~ (ie, we're not building a linked ring signature),
::    calculates just the hash of `[message g]`. Otherwise, weaves the linkage
::    state into the challenge.
::
++  generate-challenge
  |=  $:  ::  common to both linked and unlinked
          message=*
          g=point
          ::  high level universal state
          ::
          link-state=(unit [data=@ h=point y=point])
          ::  point to include in challenge when link-state isn't ~
          ::
          h=(unit point)
      ==
  ^-  @
  ::
  %-  oracle
  ?~  link-state
    [message g]
  [data.u.link-state y.u.link-state message g (need h)]
::  +generate-challenges: generates the full list of challenges
::
++  generate-challenges
  |=  $:  link-state=(unit [data=@ h=point y=point])
          message=*
          public-keys=(list point)
          ss=(list @)
      ::
          prev-k=@u
          prev-s=@
          prev-ch=@
          challenges=(list @)
      ==
  ^-  (list @)
  ::
  =/  gs=point
    %+  point-add
      (point-mul prev-s ecc-g)
    (point-mul prev-ch (snag prev-k public-keys))
  ::
  =/  hs=(unit point)
    ?~  link-state
      ~
    ::
    :-  ~
    %+  point-add
      (point-mul prev-s h.u.link-state)
    (point-mul prev-ch y.u.link-state)
  ::
  =/  ch=@
    (generate-challenge message gs link-state hs)
  ::
  ?~  ss
    [ch challenges]
  ::
  %_  $
    prev-k      (mod (add prev-k 1) (lent public-keys))
    prev-s      i.ss
    prev-ch     ch
    ss          t.ss
    challenges  [ch challenges]
  ==
::  +point-mul-h: maybe multiply u by h in linkage
::
::    Since linkage tags are optional, we need to be able to just do the math
::    in case :linkage is set and fall through otherwise. +point-mul-h is used
::    to generate the (unit point) consumed by +generate-challenge.
::
++  point-mul-h
  |=  [u=@ linkage=(unit [data=@ h=point y=point])]
  ^-  (unit point)
  ?~  linkage
    ~
  [~ (point-mul u h.u.linkage)]
::  +reorder: reorders a list so the ith element is first
::
++  reorder
  |*  [i=@ l=(list)]
  %+  weld
    (slag i l)
  (scag i l)
::  +ring-signature: types of a ring signature
::
++  ring-signature
  $:  ch0=@
      ::
      s=(list @)
      ::  linked ring signature tag
      ::
      ::    Two linked ring signatures with the same link scope can be shown to
      ::    have been made by the same private key, leading to Sybil
      ::    resistance...but if your private keys are compromised, your
      ::    adversary can determine which signatures you made.
      ::
      y=(unit point)
  ==
--
::  Signature interface
::
|%
::  +sign: creates a ring signature on an ed25519 curve
::
::    Creates an optionally linkable ring signature on 
::
++  sign
  |=  $:  message=*
          link-scope=(unit *)
      ::
          anonymity-set=(set point)
          my-public-key=point
          my-private-key=@
      ::
          eny=@uvJ
      ==
  ^-  ring-signature
  |^  ~&  [%anonymity-list anonymity-list]
      ::  k: our public-key's position in :anonymity-list
      ::
      =/  k=@u
        ~|  [%couldnt-find my-public-key in=anonymity-list]
        (need (find [my-public-key ~] anonymity-list))
      ::  Generate linkage information if given
      ::
      =/  linkage=(unit [data=@ h=point y=point])
        (generate-linkage link-scope my-private-key)
      ::  initialize our random number generator from entropy
      ::
      =+  rand=~(. og eny)
      ::  generate the random s values used in the ring
      ::
      =^  random-s-values=(list @)  rand
        =|  count=@
        =|  random-s-values=(list @)
        |-
        ?:  =(count (sub participants 1))
          [random-s-values rand]
        ::
        =^  v=@  rand  (rads:rand ecc-n)
        $(count (add 1 count), random-s-values [v random-s-values])
      ::
      ?>  ?=(^ random-s-values)
      =/  sk1=@  i.random-s-values
      =/  sk2-to-prev-sk=(list @)  t.random-s-values
      ::  Pick a random :u
      ::
      =^  u=@  rand
        (rads:rand ecc-n)
      ::  Compute challenge at k + 1
      ::
      =/  chk1=@
        %-  generate-challenge  :*
          message
          (point-mul u ecc-g)
          linkage
          (point-mul-h u linkage)
        ==
      ::  Generate challenges for [ck, ..., c1, c0, ... ck + 2, ck + 1]
      ::
      =/  reversed-chk-to-chk1=(list @)
        %-  generate-challenges  :*
          linkage
          message
          anonymity-list
          sk2-to-prev-sk
        ::
          (mod (add k 1) participants)
          sk1
          chk1
          [chk1 ~]
        ==
      =/  chk=@  (head reversed-chk-to-chk1)
      ::  Compute s = u - x * c mod n
      ::
      ::    TODO: I believe this part is wrong and that this is what is
      ::    breaking the signature verification. For some reason, this doesn't
      ::    result in . I must be screwing up the math here, but I don't
      ::    understand how.
      ::
      ::    The aos implementation is "let sK = (u - ECDSA.private_d privKey *
      ::    chK) `mod` n", and I believe the following is equivalent? At least
      ::    with smaller prime numbers, testing it in both the dojo and ghci,
      ::    they got the same results on simple things like `5 - 14 % 7`.
      ::
      ::    But I must be doing something wrong here because this sk doesn't
      ::    line up with the rest of the ring.
      ::
      =/  sk=@  (~(dif fo ecc-n) u (mul my-private-key chk))
      ::
      =/  ordered-challenges=(list @)
        (order-challenges k (flop reversed-chk-to-chk1))
      ::
      =/  ordered-ss=(list @)  (order-ss k [sk sk1 sk2-to-prev-sk])
      =/  ch0  (head ordered-challenges)
      ::
      [ch0 ordered-ss ?~(linkage ~ `y.u.linkage)]
  ::
  ++  anonymity-list
    ~(tap in anonymity-set)
  ::
  ++  participants
    (lent anonymity-list)
  ::
  ++  order-challenges
    |=  [k=@ ch=(list @)]
    (reorder (sub participants (add k 1)) ch)
  ::
  ++  order-ss
    |=  [k=@ sk-to-prev-sk=(list @)]
    (reorder (sub participants k) sk-to-prev-sk)
  --
::  +verify: verify signature
::
++  verify
  |=  $:  message=*
          link-scope=(unit *)
      ::
          anonymity-set=(set point)
          signature=ring-signature
      ==
  ^-  ?
  ::  TODO: if our signature has a linking y, we must have a link-scope and
  ::  vice versa.
  ::
  ::  decompose the signature into [s0 s1 s2....]
  ::
  ~!  s.signature
  ?>  ?=([@ @ *] s.signature)
  =/  s0=@  i.s.signature
  =/  s1=@  i.t.s.signature
  =/  s2-to-end=(list @)  t.t.s.signature
  ::  anonymity-list: set of public keys listified in ring order
  ::
  =/  anonymity-list=(list point)
    ~(tap in anonymity-set)
  ::  participants: length of :anonymity-list
  ::
  =/  participants=@u
    (lent anonymity-list)
  ::
  =/  z0p=point
    %+  point-add
      (point-mul s0 ecc-g)
    ::
    (point-mul ch0.signature (head anonymity-list))
  ::  generate the linkage using public data, and the y point from the signature
  ::
  =/  linkage=(unit [data=@ h=point y=point])
    ?~  link-scope
      ~
    =+  [data=@ h=point]=(generate-public-linkage u.link-scope)
    :-  ~
    [data h (need y.signature)]
  ::
  =/  z0pp=(unit point)
    ?~  linkage
      ~
    :-  ~
    %+  point-add
      (point-mul s0 h.u.linkage)
    (point-mul ch0.signature y.u.linkage)
  ::  initial challenge
  ::
  =/  ch1=@
    (generate-challenge message z0p linkage z0pp)
  ::
  =/  challenges
    %-  generate-challenges  :*
      linkage
      message
      anonymity-list
      s2-to-end
    ::
      (mod 1 participants)
      s1
      ch1
      [ch1 ~]
    ==
  ::
  =(ch0.signature (head challenges))
--
