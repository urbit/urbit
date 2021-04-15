/+  *test, naive, ethereum
|%
++  address  @ux
++  n  |=([=^state:naive =^input:naive] (%*(. naive lac |) verifier +<))
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
  |=  [log-name=@ux data=@ux topics=(lest @)]
  ^-  ^input:naive
  [%log *@ux data log-name topics]
::
::  ~bud is so that we aren't testing something impossible in Azimuth, like a star spawned before its sponsor galaxy
::
++  init-bud
  |=  =^state:naive
  ^-  [effects:naive ^state:naive]
  (n state (owner-changed:l1 ~bud (key ~bud)))
::
::  ~dopbud is for testing L1 ownership with L2 spawn proxy
::
++  init-dopbud
  |=  =^state:naive
  ^-  [effects:naive ^state:naive]
  =^  f1  state  (init-bud state)
  =^  f2  state  (n state (owner-changed:l1 ~dopbud (key ~dopbud)))
  =^  f3  state  (n state (changed-spawn-proxy:l1 ~dopbud deposit-address:naive))
  [:(welp f1 f2 f3) state]
::
::  ~marbud is for testing L2 ownership
::
++  init-marbud
  |=  =^state:naive
  ^-  [effects:naive ^state:naive]
  =^  f1  state  (init-bud state)
  =^  f2  state  (n state (owner-changed:l1 ~marbud (key ~marbud)))
  =^  f3  state  (n state (owner-changed:l1 ~marbud deposit-address:naive))
  [:(welp f1 f2 f3) state]
::
++  sign-tx
  |=  [pk=@ nonce=@ud tx=@]  ^-  @
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
    (log owner-changed:log-names:naive *@ux ship address ~)
  ::
  ::  TODO:  Activated (not in lib/naive.hoon)
  ::  TODO:  Spawned  (not in lib/naive.hoon)
  ::
  ++  escape-requested
    |=  [escapee=ship parent=ship]
    (log escape-requested:log-names:naive *@ux escapee parent ~)
  ::
  ++  escape-canceled
  ::  The parent is pinned but not used in lib/naive.hoon for some reason
    |=  [escapee=ship parent=ship]
    (log escape-canceled:log-names:naive *@ux escapee parent ~)
  ::
  ++  escape-accepted
    |=  [escapee=ship parent=ship]
    (log escape-accepted:log-names:naive *@ux escapee parent ~)
  ::
  ++  lost-sponsor
    |=  [lost=ship parent=ship]
    (log lost-sponsor:log-names:naive *@ux lost parent ~)
  ::
  ::  TODO:  ChangedKeys (lib/naive.hoon still has TODOs)
  ::
  ++  broke-continuity
    |=  [=ship rift=@]
    (log broke-continuity:log-names:naive rift ship ~)
  ::
  ++  changed-spawn-proxy
    |=  [=ship =address]
    (log changed-spawn-proxy:log-names:naive *@ux ship address ~)
  ::
  ++  changed-transfer-proxy
    |=  [=ship =address]
    (log changed-transfer-proxy:log-names:naive *@ux ship address ~)
  ::
  ++  changed-management-proxy
    |=  [=ship =address]
    (log changed-management-proxy:log-names:naive *@ux ship address ~)
  ::
  ++  changed-voting-proxy
    |=  [=ship =address]
    (log changed-voting-proxy:log-names:naive *@ux ship address ~)
  ::
  ::  TODO:  ChangedDns (lib/naive still has TODOs)
  ::
  ::  Ecliptic.sol events
  ::
  ++  approval-for-all
    |=  [owner=address operator=address approved=@]
    (log approval-for-all:log-names:naive approved owner operator ~)
  ::
  --
::
++  l2
  ::
  |%
  ::
  ::  TODO: Allow requesting ship to differ from the ship the action is for
  ::
  ++  spawn
    |=  [nonce=@ud parent=ship proxy=@tas child=ship =address]  ^-  @
    ::  TODO: allow requesting ship and parent ship to differ
    %^  sign-tx  parent  nonce
    (take-ship-address:bits %spawn parent proxy child address)
  ::
  ++  transfer-point
    |=  [nonce=@ud =ship =address proxy=@tas reset=?]  ^-  @
    ::  TODO: allow requesting ship and target ship to differ
    %^  sign-tx  ship  nonce
    %:  can  3
      (from-proxy:bits proxy)
      4^ship
      1^(can 0 7^%0 1^reset ~)                   :: %transfer-point
      4^ship
      20^address
      ~
    ==
  ::
  ++  configure-keys
    |=  [nonce=@ud =ship proxy=@tas breach=@ encrypt=@ auth=@ crypto-suite=@]  ^-  @
    %^  sign-tx  ship  nonce
    %:  can  3
      (from-proxy:bits proxy)
      4^ship
      1^(can 0 7^%2 1^breach ~)                 :: %configure-keys
      4^ship
      32^encrypt
      32^auth
      4^crypto-suite
      ~
    ==
  ::
  ++  escape
    |=  [nonce=@ud child=ship proxy=@tas parent=ship]  ^-  @
    %^  sign-tx  child  nonce
    (take-escape:bits %escape child proxy parent)
  ::
  ++  cancel-escape
    |=  [nonce=@ud child=ship proxy=@tas parent=ship]  ^-  @
    %^  sign-tx  child  nonce
    (take-escape:bits %cancel-escape child proxy parent)
  ::
  ++  adopt
    |=  [nonce=@ud child=ship proxy=@tas parent=ship]  ^-  @
    %^  sign-tx  child  nonce
    (take-escape:bits %adopt child proxy parent)
  ::
  ++  reject
    |=  [nonce=@ud child=ship proxy=@tas parent=ship]  ^-  @
    %^  sign-tx  child  nonce
    (take-escape:bits %reject child proxy parent)
  ::
  ++  detach
    |=  [nonce=@ud child=ship proxy=@tas parent=ship]  ^-  @
    %^  sign-tx  child  nonce
    (take-escape:bits %detach child proxy parent)
  ::
  ++  set-management-proxy
    |=  [nonce=@ud =ship proxy=@tas =address]  ^-  @
    %^  sign-tx  ship  nonce
    (take-ship-address:bits %set-management-proxy ship proxy ship address)
  ::
  ++  set-spawn-proxy
    |=  [nonce=@ud =ship proxy=@tas =address]  ^-  @
    %^  sign-tx  ship  nonce
    (take-ship-address:bits %set-spawn-proxy ship proxy ship address)
  ::
  ++  set-voting-proxy
    |=  [nonce=@ud =ship proxy=@tas =address]  ^-  @
    %^  sign-tx  ship  nonce
    (take-ship-address:bits %set-voting-proxy ship proxy ship address)
  ::
  ++  set-transfer-proxy
    |=  [nonce=@ud =ship proxy=@tas =address]  ^-  @
    %^  sign-tx  ship  nonce
    (take-ship-address:bits %set-transfer-proxy ship proxy ship address)
  ::
  ++  bits
    ::
    |%
    ::
    ::  TODO: Shouldn't need to pass all these arguments along - they should already be in the subject somewhere
    ::
    ++  take-escape
      |=  [action=@tas child=ship proxy=@tas parent=ship]  ^-  @
      =/  op
        ?+  action  !!
          %escape         %3
          %cancel-escape  %4
          %adopt          %5
          %reject         %6
          %detach         %7
        ==
      %:  can  3
        (from-proxy proxy)
        4^child
        1^(can 0 7^op 1^0 ~)
        4^child
        4^parent
        ~
      ==
    ::
    ++  take-ship-address
      |=  [action=@tas from=ship proxy=@tas target=ship =address]  ^-  @
      =/  op
        ?+  action  !!
          %spawn                    %1
          %set-management-proxy     %8
          %set-spawn-proxy          %9
          %set-voting-proxy         %10
          %set-transfer-proxy       %11
        ==
      %:  can  3
        (from-proxy proxy)
        4^from
        1^(can 0 7^op 1^0 ~)
        4^target
        20^address
        ~
      ==
    ::
    ++  from-proxy
      |=  prx=@tas
      ^-  [@ @]
      =/  proxy
        ?+  prx  !!
          %own       %0
          %spawn     %1
          %manage    %2
          %vote      %3
          %transfer  %4
        ==
      1^(can 0 1^0 3^proxy 4^0 ~)
    ::
    --
  ::
  --
::
--
::
|%
++  test-log  ^-  tang
  %+  expect-eq
    !>
    :-  [%point ~bud %owner 0x123]~
    :_  [~ ~]  :_  [~ ~]
    :-  ~bud
    %*(. *point:naive dominion %l1, owner.own 0x123^0, who.sponsor.net ~bud)
  ::
    !>
    %^  naive  verifier  *^state:naive
    :*  %log  *@ux  *@ux
        owner-changed:log-names:naive  (@ux ~bud)  0x123  ~
    ==
::
++  test-deposit  ^-  tang
  %+  expect-eq
    !>  %l2
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    dominion:(~(got by points.state) ~marbud)
::
++  test-batch  ^-  tang
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
++  test-l1-changed-spawn-proxy  ^-  tang
  %+  expect-eq
    !>  [0x123 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-bud state)
    =^  f  state  (n state (changed-spawn-proxy:l1 ~bud 0x123))
    spawn-proxy.own:(~(got by points.state) ~bud)
::
++  test-l1-changed-transfer-proxy  ^-  tang
  %+  expect-eq
    !>  [0x123 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-bud state)
    =^  f  state  (n state (changed-transfer-proxy:l1 ~bud 0x123))
    transfer-proxy.own:(~(got by points.state) ~bud)
::
++  test-l1-changed-management-proxy  ^-  tang
  %+  expect-eq
    !>  [0x123 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-bud state)
    =^  f  state  (n state (changed-management-proxy:l1 ~bud 0x123))
    management-proxy.own:(~(got by points.state) ~bud)
::
++  test-l1-changed-voting-proxy  ^-  tang
  %+  expect-eq
    !>  [0x123 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-bud state)
    =^  f  state  (n state (changed-voting-proxy:l1 ~bud 0x123))
    voting-proxy.own:(~(got by points.state) ~bud)
::
++  test-l2-set-spawn-proxy  ^-  tang
  %+  expect-eq
    !>  [0x123 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat (set-spawn-proxy:l2 0 ~marbud %own 0x123))
    spawn-proxy.own:(~(got by points.state) ~marbud)
::
++  test-l2-set-transfer-proxy  ^-  tang
  %+  expect-eq
    !>  [0x123 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat (set-transfer-proxy:l2 0 ~marbud %own 0x123))
    transfer-proxy.own:(~(got by points.state) ~marbud)
::
++  test-l2-set-management-proxy  ^-  tang
  %+  expect-eq
    !>  [0x123 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat (set-management-proxy:l2 0 ~marbud %own 0x123))
    management-proxy.own:(~(got by points.state) ~marbud)
::
++  test-l2-set-voting-proxy  ^-  tang
  ::  Only galaxies have voting proxies, and they aren't allowed on L2
  %-  expect-fail
    |.
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat (set-voting-proxy:l2 0 ~marbud %own 0x123))
    voting-proxy.own:(~(got by points.state) ~marbud)
::
++  test-l2-spawn-proxy-deposit  ^-  tang
  %+  expect-eq
    !>  %spawn
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-dopbud state)
    dominion:(~(got by points.state) ~dopbud)
::
++  test-marbud-l2-spawn  ^-  tang
  %+  expect-eq
    !>  [`@ux`(key ~linnup-torsyx) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat (spawn:l2 0 ~marbud %own ~linnup-torsyx (key ~linnup-torsyx)))
    transfer-proxy.own:(~(got by points.state) ~linnup-torsyx)
::
++  test-dopbud-l2-spawn  ^-  tang
  %+  expect-eq
    !>  [`@ux`(key ~palsep-picdun) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-dopbud state)
    =^  f  state  (n state %bat (spawn:l2 0 ~dopbud %own ~palsep-picdun (key ~palsep-picdun)))
    transfer-proxy.own:(~(got by points.state) ~palsep-picdun)
::
++  test-dopbud-l2-spawn-after-transfer  ^-  tang
  ::  Currently fails, does not spawn ~laclur-rachul unless you leave ~dopbud's ownership address alone
  ::  All individual transactions work fine though, its the sequence that breaks something.
  %+  expect-eq
    !>  [`@ux`(key ~laclur-rachul) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-dopbud state)
    =^  f  state  (n state %bat (spawn:l2 0 ~dopbud %own ~palsep-picdun (key ~palsep-picdun)))
    =^  f  state  (n state (owner-changed:l1 ~dopbud 0x345))
    =^  f  state  (n state %bat (spawn:l2 1 ~dopbud %own ~laclur-rachul (key ~laclur-rachul)))
    transfer-proxy.own:(~(got by points.state) ~laclur-rachul)
::
++  test-linnup-torsyx-l2-transfer-ownership  ^-  tang
  %+  expect-eq
    !>  [`@ux`(key ~linnup-torsyx) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat (spawn:l2 0 ~marbud %own ~linnup-torsyx (key ~linnup-torsyx)))
    =^  f  state  (n state %bat (transfer-point:l2 0 ~linnup-torsyx (key ~linnup-torsyx) %transfer &))
    owner.own:(~(got by points.state) ~linnup-torsyx)
::
++  test-palsep-picdun-l2-transfer-ownership  ^-  tang
  %+  expect-eq
    !>  [`@ux`(key ~palsep-picdun) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-dopbud state)
    =^  f  state  (n state %bat (spawn:l2 0 ~dopbud %own ~palsep-picdun (key ~palsep-picdun)))
    =^  f  state  (n state %bat (transfer-point:l2 0 ~palsep-picdun (key ~palsep-picdun) %transfer &))
    owner.own:(~(got by points.state) ~palsep-picdun)
::
--
