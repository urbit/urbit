/-  *ring
::  ring signatures over the edwards curve
::
|%
::  +raw is the raw internal ring signature implementation. +raw does not deal
::  with urbit ship identities or urbit nouns and is low level. It only deals
::  with ed25519 keys and message digests.
::
::  This raw interface is vaguely modeled on the haskell aos-signature package,
::  but is written in terms of ed25519 primitives instead of general ECC and
::  changes how linkage tags are computed so that how linkage occurs is a
::  client decision instead of hard coding the set of public keys as the
::  linkage scope.
::
++  raw
  |%
  ::  +generate-public-linkage: generate public linkage information
  ::
  ++  generate-public-linkage
    |=  link-scope=@
    ^-  [data=@ h=@udpoint]
    ::
    =/  data=@   (mod link-scope l:ed:crypto)
    =/  h=@udpoint  (scalarmult-base:ed:crypto data)
    [data h]
  ::  +generate-linkage: linkage information from scope and private key
  ::
  ::    data: deterministically picked data point based off scope
  ::    h:    h = [data] * g
  ::    y:    y = [x] * h
  ++  generate-linkage
    |=  [link-scope=(unit @) my-private-key=@]
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
  ::    calculates just the hash of `[message g]`. Otherwise, weaves the
  ::    linkage state into the challenge.
  ::
  ++  generate-challenge
    |=  $:  ::  common to both linked and unlinked
            message=@
            g=@udpoint
            ::  high level universal state
            ::
            link-state=(unit [data=@ h=@udpoint y=@udpoint])
            ::  point to include in challenge when link-state isn't ~
            ::
            h=(unit @udpoint)
        ==
    ^-  @
    ::  concatenate and reduce our message down to a 512-bit hash
    =/  concatenated
      ?~  link-state
        (shal 96 (can 3 ~[[64 message] [32 g]]))
      ::
      %+  shal  192
      %+  can  3
      :~  [64 message]
          [32 g]
          [32 data.u.link-state]
          [32 y.u.link-state]
          [32 (need h)]
      ==
    ::
    (mod concatenated l:ed:crypto)
  ::  +generate-challenges: generates the full list of challenges
  ::
  ++  generate-challenges
    |=  $:  link-state=(unit [data=@ h=@udpoint y=@udpoint])
            message=@
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
  ::    in case :linkage is set and fall through otherwise. +scalarmult-h is
  ::    used to generate the (unit point) consumed by +generate-challenge.
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
  ::  +sign: creates a ring signature on an ed25519 curve
  ::
  ++  sign
    |=  $:  message=@
            link-scope=(unit @)
        ::
            anonymity-set=(set @udpoint)
            my-public-key=@udpoint
            my-private-key=@udscalar
        ::
            eny=@uvJ
        ==
    ^-  raw-ring-signature
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
    |=  $:  message=@
            link-scope=(unit @)
        ::
            anonymity-set=(set @udpoint)
            signature=raw-ring-signature
        ==
    ^-  ?
    ::  if there's a linkage scope but no tag, fail
    ::
    ?:  &(?=(^ link-scope) ?=(~ y.signature))
      %.n
    ::  if there's no linkage scope but a tag, fail
    ::
    ?:  &(?=(~ link-scope) ?=(^ y.signature))
      %.n
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
    ::  generate the linkage using public data, and the y point from the
    ::  signature
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
  --
::  +detail: details about getting keys from Azimuth
::
++  detail
  |%
  ::  +seed-to-private-key-scalar: keyfile form to scalar we can multiply with
  ::
  ++  seed-to-private-key-scalar
    |=  sk=@I  ^-  @udscalar
    ?:  (gth (met 3 sk) 32)  !!
    =+  h=(shal (rsh [0 3] b:ed:crypto) sk)
    %+  add
      (bex (sub b:ed:crypto 2))
    (lsh [0 3] (cut 0 [3 (sub b:ed:crypto 5)] h))
  ::  +get-public-key-from-pass: decode the raw @ public key structure
  ::
  ++  get-public-key-from-pass
    |=  a=pass
    ^-  [@ @]
    =+  [mag=(end 3 a) bod=(rsh 3 a)]
    ~|  %not-crub-pubkey  ?>  =('b' mag)
    [cry=(rsh 8 bod) sgn=(end 8 bod)]
  ::
  ::
  ++  get-private-key-from-ring
    |=  a=ring
    ^-  [@ @]
    =+  [mag=(end 3 a) bod=(rsh 3 a)]
    ~|  %not-crub-seckey  ?>  =('B' mag)
    =+  [c=(rsh 8 bod) s=(end 8 bod)]
    ::  todo: do we puck here?
    [c s]
  ::  +ship-life-to-pubid: fetches public key information from jael
  ::
  ++  ship-life-to-pubid
    |=  [our=@p now=@da ship=@p =life]
    ^-  @udpoint
    ::
    =/  d=[=^life =pass *]
      =/  scry-path=path
        :~  %j
            (scot %p our)
            %deed
            (scot %da now)
            (scot %p ship)
            (scot %ud life)
        ==
      .^([^life pass *] scry-path)
    ::  we have the deed which has pass, which is several numbers +cat-ed
    ::  together; pull out the keys
    ::
    =/  x=[crypt=@ auth=@]  (get-public-key-from-pass pass.d)
    ::
    `@udpoint`auth.x
  ::
  ++  build-signing-participants
    |=  [our=@p now=@da invited=(list @p)]
    ^-  [(set [@p life]) (set @udpoint)]
    ::
    =|  participants=(set [@p life])
    =|  keys=(set @udpoint)
    ::
    |-
    ?~  invited
      [participants keys]
    ::
    =/  lyfe=(unit @ud)
      .^((unit @ud) j+/(scot %p our)/lyfe/(scot %da now)/(scot %p i.invited))
    ::
    ?~  lyfe
      $(invited t.invited)
    ::
    =/  pubkey=@udpoint  (ship-life-to-pubid our now i.invited u.lyfe)
    ::
    =.  participants  (~(put in participants) [i.invited u.lyfe])
    =.  keys          (~(put in keys) pubkey)
    ::
    $(invited t.invited)
  ::
  ::
  ++  build-verifying-participants
    |=  [our=@p now=@da invited=(list [ship=@p =life])]
    ^-  (set @udpoint)
    ::
    =|  keys=(set @udpoint)
    ::
    |-
    ?~  invited
      keys
    ::
    =/  pubkey=@udpoint
      (ship-life-to-pubid our now ship.i.invited life.i.invited)
    =.  keys
      (~(put in keys) pubkey)
    ::
    $(invited t.invited)
  --
--
::  public interface
::
|%
::  +sign: ring-signs a message using the current ship
::
++  sign
  |=  $:  our=@p
          now=@da
          eny=@uvJ
      ::
          message=*
          link-scope=(unit *)
          anonymity-set=(set @p)
      ==
  ^-  ring-signature
  ::  if our is not in @p, we must be in @p.
  ::
  =.  anonymity-set  (~(put in anonymity-set) our)
  ::
  =/  msg-hash=@  (shaz (jam message))
  =/  link-hash=(unit @)  (bind link-scope |=(a=* (shaz (jam a))))
  ::  get everyone's public keys
  ::
  =/  p=[participants=(set [ship=@p =life]) keys=(set @udpoint)]
    (build-signing-participants:detail our now ~(tap in anonymity-set))
  ::  get our ships' current life
  ::
  =/  our-life=life
    .^(life %j /(scot %p our)/life/(scot %da now)/(scot %p our))
  ::  get our ships' secret keyfile ring
  ::
  =/  secret-ring=ring
    .^(ring %j /(scot %p our)/vein/(scot %da now)/(scot %ud our-life))
  ::  fetch the encoded auth seed from the ring
  ::
  =/  secret-auth-seed=@
    +:(get-private-key-from-ring:detail secret-ring)
  ::  get our ships' public key
  ::
  =/  public-key=@udpoint
    (ship-life-to-pubid:detail our now our our-life)
  ::
  :-  participants.p
  :-  link-scope
  %-  sign:raw  :*
    msg-hash
    link-hash
    keys.p
    public-key
    (seed-to-private-key-scalar:detail secret-auth-seed)
    eny
  ==
::  +verify: verifies a message against a ring signature
::
++  verify
  |=  [our=@p now=@da message=* =ring-signature]
  ^-  ?
  ::
  =/  keys=(set @udpoint)
    %^  build-verifying-participants:detail  our  now
    ~(tap in participants.ring-signature)
  ::
  =/  msg-hash=@  (shaz (jam message))
  =/  link-hash=(unit @)  (bind link-scope.ring-signature |=(a=* (shaz (jam a))))
  ::
  (verify:raw msg-hash link-hash keys raw.ring-signature)
--
