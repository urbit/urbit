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
  |=  [log-name=@ux data=@t topics=(lest @)]
  ^-  ^input:naive
  [%log *@ux data log-name topics]
::
::
++  owner-changed
  |=  [=ship =address]
  (log owner-changed:log-names:naive *@t ship address ~)
::
++  init-bud
  |=  =^state:naive
  (n state (owner-changed ~bud 0x123))
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
::
++  l2-init-marbud
  |=  =^state:naive
  =^  f1  state  (init-bud state)
  =^  f2  state  (n state (owner-changed ~marbud (key ~marbud)))
  =^  f3  state  (n state (owner-changed ~marbud deposit-address:naive))
  [:(welp f1 f2 f3) state]
::
++  l2-init-dopbud
  |=  =^state:naive
  =^  f1  state  (init-bud state)
  =^  f2  state  (n state (owner-changed ~dopbud (key ~dopbud)))
  =^  f3  state  (n state (l2-changed-spawn-proxy ~dopbud))
  [:(welp f1 f2 f3) state]
  ::
++  l2-changed-spawn-proxy
  |=  =ship
  (log changed-spawn-proxy:log-names:naive *@t ship deposit-address:naive ~)
::
++  l2-spawn-ship
  |=  [nonce=@ud from-ship=ship spawn-ship=ship =address]
  %^  sign-tx  from-ship  nonce
  %:  can  3
    1^0 ::proxy set to owner
  ::1^(can 0 3^%1 5^0 ~)  I think this should use the spawn proxy? but it creates an infinte loop
    4^from-ship
    1^(can 0 7^%1 1^0 ~)
    4^spawn-ship
    20^address
    ~
  ==
::
--
::
|%
++  test-log
  %+  expect-eq
    !>
    :-  [%point ~bud %owner 0x123]~
    [[[~bud %*(. *point:naive dominion %l1, owner.own 0x123^0)] ~ ~] ~ ~]
  ::
    !>
    %^  naive  verifier  *^state:naive
    :*  %log  *@ux  *@t
        owner-changed:log-names:naive  (@ux ~bud)  0x123  ~
    ==
::
++  test-deposit
  %+  expect-eq
    !>  %l2
  ::
    !>
    =|  =^state:naive
    =^  f  state  (l2-init-marbud state)
    dominion:(~(got by points.state) ~marbud)
::
++  test-batch
  %+  expect-eq
    !>  [0x234 2]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (l2-init-marbud state)
    =^  f  state  (n state %bat (transfer-point 0 ~marbud (key ~marbud) |))
    =^  f  state  (n state %bat (transfer-point 1 ~marbud 0x234 |))
    owner.own:(~(got by points.state) ~marbud)
::
++  test-l2-spawn-proxy-deposit
  %+  expect-eq
    !>  %spawn
  ::
    !>
    =|  =^state:naive
    =^  f  state  (l2-init-dopbud state)
    dominion:(~(got by points.state) ~dopbud)
::
++  test-marbud-l2-spawn-point
  %+  expect-eq
    !>  [`@ux`(key ~linnup-torsyx) 0]
    ::
    !>
    =|  =^state:naive
    =^  f  state  (l2-init-marbud state)
    =^  f  state  (n state %bat (l2-spawn-ship 0 ~marbud ~linnup-torsyx (key ~linnup-torsyx)))
    transfer-proxy.own:(~(got by points.state) ~linnup-torsyx)
::
++  test-dopbud-l2-spawn-point
  %+  expect-eq
    !>  [`@ux`(key ~palsep-picdun) 0]
    ::
    !>
    =|  =^state:naive
    =^  f  state  (l2-init-dopbud state)
    =^  f  state  (n state %bat (l2-spawn-ship 0 ~dopbud ~palsep-picdun (key ~palsep-picdun)))
    transfer-proxy.own:(~(got by points.state) ~palsep-picdun)
--
