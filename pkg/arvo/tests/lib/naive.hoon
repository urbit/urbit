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
  |=  [log-name=@ux data=@ux topics=(lest @)]
  ^-  ^input:naive
  [%log *@ux data log-name topics]
::
::
++  owner-changed
  |=  [=ship =address]
  (log owner-changed:log-names:naive *@ux ship address ~)
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
  |=  [pk=@ nonce=@ud tx=@]
  =+  (ecdsa-raw-sign:secp256k1:secp:crypto (dad:naive 5 nonce tx) pk)
  (cat 3 (can 3 1^v 32^r 32^s ~) tx)
::
++  transfer-point
  |=  [nonce=@ud =ship =address reset=?]
  %^  sign-tx  ship  nonce
  %:  can  3
    1^0
    4^ship
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
    :-  [%point ~bud %owner 0x123]~
    :_  [~  ~]  :_  [~ ~]
    :-  ~bud
    %*(. *point:naive dominion %l1, owner.own 0x123^0, who.sponsor.net ~bud)
  ::
    !>
    %^  naive  verifier  *^state:naive
    :*  %log  *@ux  *@ux
        owner-changed:log-names:naive  (@ux ~bud)  0x123  ~
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
    !>  [0x234 2]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-l2-marbud state)
    =^  f  state  (n state %bat (transfer-point 0 ~marbud (key ~marbud) |))
    =^  f  state  (n state %bat (transfer-point 1 ~marbud 0x234 |))
    owner.own:(~(got by points.state) ~marbud)
--
