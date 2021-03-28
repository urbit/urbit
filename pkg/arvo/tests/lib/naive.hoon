/+  *test, naive, ethereum
|%
++  address  @ux
++  n  |=([=^state:naive =^input:naive] (naive verifier +<))
::  TODO: does this uniquely produce the pubkey?
::
++  verifier
  ^-  ^verifier:naive
  |=  [dat=@ v=@ r=@ s=@]
  =/  result
    %-  mule
    |.
    =,  secp256k1:secp:crypto
    %-  address-from-pub:key:ethereum
    %-  serialize-point
    (ecdsa-raw-recover dat v r s)
  ?-  -.result
    %|  ~
    %&  `p.result
  ==
::
++  key  address-from-prv:key:ethereum
++  log
  |=  [event=@t data=@t topics=(lest @)]
  ^-  ^input:naive
  [%log ~ *@ux data (hash-log-name:naive event) topics]
::
::
++  owner-changed
  |=  [=ship =address]
  (log 'OwnerChanged(uint32,address)' *@t ship address ~)
::
++  init-bud
  |=  =^state:naive
  (n state (owner-changed ~bud 0x123))
::
++  init-l2-marbud
  |=  =^state:naive
  =^  f1  state  (init-bud state)
  =^  f2  state  (n state (owner-changed ~marbud (key ~marbud)))
  =^  f3  state  (n state (owner-changed ~marbud deposit-address:naive))
  [:(welp f1 f2 f3) state]
::
++  sign-tx
  |=  [pk=@ tx=@]
  =+  (ecdsa-raw-sign:secp256k1:secp:crypto tx pk)
  (cat 3 (can 3 1^v 32^r 32^s ~) tx)
::
++  transfer-point
  |=  [=ship =address reset=?]
  %+  sign-tx  ship
  %:  can  3
    1^(can 0 7^%0 1^reset ~)
    4^ship
    20^address
    ~
  ==
--
::
|%
++  test-log
  %+  expect-eq
    !>
    `[[[~bud %*(. *point:naive dominion %l1, owner.own 0x123)] ~ ~] ~ ~]
  ::
    !>
    %^  naive  verifier  *^state:naive
    :*  %log  ~  *@ux  *@t
        (hash-log-name:naive 'OwnerChanged(uint32,address)')  (@ux ~bud)  0x123  ~
    ==
::
++  test-deposit
  %+  expect-eq
    !>  %l2
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-l2-marbud state)
    dominion:(~(got by points.state) ~marbud)
::
++  test-batch
  %+  expect-eq
    !>  0x234
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-l2-marbud state)
    =^  f  state  (n state %bat (transfer-point ~marbud 0x234 |))
    owner.own:(~(got by points.state) ~marbud)
--
