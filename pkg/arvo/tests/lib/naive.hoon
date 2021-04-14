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
::
++  log
  |=  [log-name=@ux data=@t topics=(lest @)]
  ^-  ^input:naive
  [%log *@ux data log-name topics]
::
::  ~bud is so that we aren't testing something impossible in Azimuth, like a star spawned before its sponsor galaxy
::
++  init-bud
  |=  =^state:naive
  (n state (owner-changed:l1 ~bud 0x123))
::
::  ~dopbud is for testing L1 ownership with L2 spawn proxy
::
++  init-dopbud
  |=  =^state:naive
  =^  f1  state  (init-bud state)
  =^  f2  state  (n state (owner-changed:l1 ~dopbud (key ~dopbud)))
  =^  f3  state  (n state (changed-spawn-proxy:l1 ~dopbud deposit-address:naive))
  [:(welp f1 f2 f3) state]
::
::  ~marbud is for testing L2 ownership
::
++  init-marbud
  |=  =^state:naive
  =^  f1  state  (init-bud state)
  =^  f2  state  (n state (owner-changed:l1 ~marbud (key ~marbud)))
  =^  f3  state  (n state (owner-changed:l1 ~marbud deposit-address:naive))
  [:(welp f1 f2 f3) state]
::
++  set-proxy-bits
  |=  from-proxy=@tas
  =/  proxy
    ?+  from-proxy  !!
      %own       %0
      %spawn     %1
      %manage    %2
      %vote      %3
      %transfer  %4
    ==
  1^(can 0 1^0 3^proxy 4^0 ~)
::
++  sign-tx
  |=  [pk=@ nonce=@ud tx=@]
  =+  (ecdsa-raw-sign:secp256k1:secp:crypto (dad:naive 5 nonce tx) pk)
  (cat 3 (can 3 1^v 32^r 32^s ~) tx)
::
++  l1
  |%
  ::
  ::  Azimuth.sol events
  ::
  ++  owner-changed
    |=  [=ship =address]
    (log owner-changed:log-names:naive *@t ship address ~)
  ::
  ::  TODO:  Activated (not in lib/naive.hoon)
  ::  TODO:  Spawned  (not in lib/naive.hoon)
  ::
  ++  escape-requested
    |=  [escapee=ship parent=ship]
    (log escape-requested:log-names:naive *@t escapee parent ~)
  ::
  ++  escape-canceled
  ::  The parent is pinned but not used in lib/naive.hoon for some reason
    |=  [escapee=ship parent=ship]
    (log escape-canceled:log-names:naive *@t escapee parent ~)
  ::
  ++  escape-accepted
    |=  [escapee=ship parent=ship]
    (log escape-accepted:log-names:naive *@t escapee parent ~)
  ::
  ++  lost-sponsor
    |=  [lost=ship parent=ship]
    (log lost-sponsor:log-names:naive *@t lost parent ~)
  ::
  ::  TODO:  ChangedKeys (lib/naive.hoon still has TODOs)
  ::
  ++  broke-continuity
    |=  [=ship rift=@]
    (log broke-continuity:log-names:naive rift ship ~)
  ::
  ++  changed-spawn-proxy
    |=  [=ship =address]
    (log changed-spawn-proxy:log-names:naive *@t ship address ~)
  ::
  ++  changed-transfer-proxy
    |=  [=ship =address]
    (log changed-transfer-proxy:log-names:naive *@t ship address ~)
  ::
  ++  changed-management-proxy
    |=  [=ship =address]
    (log changed-management-proxy:log-names:naive *@t ship address ~)
  ::
  ++  changed-voting-proxy
    |=  [=ship =address]
    (log changed-voting-proxy:log-names:naive *@t ship address ~)
  ::
  ::  TODO:  ChangedDns (lib/naive still has TODOs)
  ::
  ::  Ecliptic.sol events
  ::
  ++  approval-for-all
    |=  [owner=address operator=address approved=@t]
    (log approval-for-all:log-names:naive approved owner operator ~)
  ::
  --
::
++  l2
  ::
  |%
  ::
  ++  spawn-ship
    |=  [nonce=@ud from-ship=ship proxy=@tas spawn-ship=ship =address]
    %^  sign-tx  from-ship  nonce
    %:  can  3
      (set-proxy-bits proxy)
      4^from-ship
      1^(can 0 7^%1 1^0 ~)
      4^spawn-ship
      20^address
      ~
    ==
  ::
  ++  transfer-point
    |=  [nonce=@ud =ship =address proxy=@tas reset=?]
    %^  sign-tx  ship  nonce
    %:  can  3
      (set-proxy-bits proxy)
      4^ship
      1^(can 0 7^%0 1^reset ~)
      4^ship
      20^address
      ~
    ==
  --
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
    =^  f  state  (init-marbud state)
    dominion:(~(got by points.state) ~marbud)
::
++  test-batch
  %+  expect-eq
    !>  [0x234 2]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat (transfer-point:l2 0 ~marbud (key ~marbud) %own |))
    =^  f  state  (n state %bat (transfer-point:l2 1 ~marbud 0x234 %own |))
    owner.own:(~(got by points.state) ~marbud)
::
++  test-l2-spawn-proxy-deposit
  %+  expect-eq
    !>  %spawn
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-dopbud state)
    dominion:(~(got by points.state) ~dopbud)
::
++  test-marbud-l2-spawn-point
  %+  expect-eq
    !>  [`@ux`(key ~linnup-torsyx) 0]
    ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat (spawn-ship:l2 0 ~marbud %own ~linnup-torsyx (key ~linnup-torsyx)))
    transfer-proxy.own:(~(got by points.state) ~linnup-torsyx)
::
++  test-dopbud-l2-spawn-point
  %+  expect-eq
    !>  [`@ux`(key ~palsep-picdun) 0]
    ::
    !>
    =|  =^state:naive
    =^  f  state  (init-dopbud state)
    =^  f  state  (n state %bat (spawn-ship:l2 0 ~dopbud %own ~palsep-picdun (key ~palsep-picdun)))
    transfer-proxy.own:(~(got by points.state) ~palsep-picdun)
::
--
