/-  *ring
::  ring signatures over the edwards curve
::
|%
::  +oracle: deterministic random response on input
::
++  oracle
  |=  input=*
  :: l:ed is ecc-n
  (mod (shaz (jam input)) l:ed:crypto)
::
::::
::
::  +generate-public-linkage: generate public linkage information
::
++  generate-public-linkage
  |=  link-scope=*
  ^-  [data=@ h=@udpoint]
  ::
  =/  data=@   (oracle link-scope)
  =/  h=@udpoint  (scalarmult-base:ed:crypto data)
  [data h]

::  +generate-linkage: generates linkage information from scope and private key
::
::    data: deterministically picked data point based off scope
::    h:    h = [data] * g
::    y:    y = [x] * h
++  generate-linkage
  |=  [link-scope=(unit *) my-private-key=@]
  ^-  (unit [data=@ h=@udpoint y=@udpoint])
  ::
  ?~  link-scope
    ~
  ::
  =+  [data=@ h=@udpoint]=(generate-public-linkage u.link-scope)
  =/  y=@udpoint  (scalarmult:ed:crypto my-private-key h)
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
          g=@udpoint
          ::  high level universal state
          ::
          link-state=(unit [data=@ h=@udpoint y=@udpoint])
          ::  point to include in challenge when link-state isn't ~
          ::
          h=(unit @udpoint)
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
  |=  $:  link-state=(unit [data=@ h=@udpoint y=@udpoint])
          message=*
          public-keys=(list @udpoint)
          ss=(list @)
      ::
          prev-k=@u
          prev-s=@
          prev-ch=@
          challenges=(list @)
      ==
  ^-  (list @)
  ::
  =/  gs=@udpoint
    %-  add-scalarmult-scalarmult-base:ed:crypto  :*
      prev-ch
      (snag prev-k public-keys)
      prev-s
    ==
  ::
  =/  hs=(unit @udpoint)
    ?~  link-state
      ~
    ::
    :-  ~
    %-  add-double-scalarmult:ed:crypto  :*
      prev-s
      h.u.link-state
      prev-ch
      y.u.link-state
    ==
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
::  +scalarmult-h: maybe multiply u by h in linkage
::
::    Since linkage tags are optional, we need to be able to just do the math
::    in case :linkage is set and fall through otherwise. +scalarmult-h is used
::    to generate the (unit point) consumed by +generate-challenge.
::
++  scalarmult-h
  |=  [u=@ linkage=(unit [data=@ h=@udpoint y=@udpoint])]
  ^-  (unit @udpoint)
  ?~  linkage
    ~
  [~ (scalarmult:ed:crypto u h.u.linkage)]
::  +reorder: reorders a list so the ith element is first
::
++  reorder
  |*  [i=@ l=(list)]
  %+  weld
    (slag i l)
  (scag i l)
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
          anonymity-set=(set @udpoint)
          my-public-key=@udpoint
          my-private-key=@udscalar
      ::
          eny=@uvJ
      ==
  ^-  ring-signature
  |^  ::  k: our public-key's position in :anonymity-list
      ::
      =/  k=@u
        ~|  [%couldnt-find my-public-key in=anonymity-list]
        (need (find [my-public-key ~] anonymity-list))
      ::  Generate linkage information if given
      ::
      =/  linkage=(unit [data=@ h=@udpoint y=@udpoint])
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
        =^  v=@  rand  (rads:rand l:ed:crypto)
        $(count (add 1 count), random-s-values [v random-s-values])
      ::
      ?>  ?=(^ random-s-values)
      =/  sk1=@  i.random-s-values
      =/  sk2-to-prev-sk=(list @)  t.random-s-values
      ::  Pick a random :u
      ::
      =^  u=@  rand
        (rads:rand l:ed:crypto)
      ::  Compute challenge at k + 1
      ::
      =/  chk1=@
        %-  generate-challenge  :*
          message
          (scalarmult-base:ed:crypto u)
          linkage
          (scalarmult-h u linkage)
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
      =/  sk=@  (~(dif fo l:ed:crypto) u (mul my-private-key chk))
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
          anonymity-set=(set @udpoint)
          signature=ring-signature
      ==
  ^-  ?
  ::  TODO: if our signature has a linking y, we must have a link-scope and
  ::  vice versa.
  ::
  ::  decompose the signature into [s0 s1 s2....]
  ::
  ?>  ?=([@ @ *] s.signature)
  =/  s0=@  i.s.signature
  =/  s1=@  i.t.s.signature
  =/  s2-to-end=(list @)  t.t.s.signature
  ::  anonymity-list: set of public keys listified in ring order
  ::
  =/  anonymity-list=(list @udpoint)
    ~(tap in anonymity-set)
  ::  participants: length of :anonymity-list
  ::
  =/  participants=@u
    (lent anonymity-list)
  ::
  =/  z0p=@udpoint
    %-  add-scalarmult-scalarmult-base:ed:crypto  :*
       ch0.signature
       (head anonymity-list)
       s0
    ==
  ::  generate the linkage using public data, and the y point from the signature
  ::
  =/  linkage=(unit [data=@ h=@udpoint y=@udpoint])
    ?~  link-scope
      ~
    =+  [data=@ h=@udpoint]=(generate-public-linkage u.link-scope)
    :-  ~
    [data h (need y.signature)]
  ::
  =/  z0pp=(unit @udpoint)
    ?~  linkage
      ~
    :-  ~
    %-  add-double-scalarmult:ed:crypto  :*
      s0
      h.u.linkage
      ch0.signature
      y.u.linkage
    ==
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

::  +public-key-for-ship: 
::
::    TODO: We should go talk to Azimuth to get the ship's real public key. But
::    for now, we need to 
::
++  public-key-for-ship
  |=  p=@p
  ^-  @udpoint
  ::
  (scalarmult-base:ed:crypto `@udscalar`p)
--
