/+  *test, naive, ethereum, azimuth, *naive-transactions
::
|%
++  n  |=([=^state:naive =^input:naive] (%*(. naive lac |) verifier 1.337 +<))
::
++  addr  address-from-prv:key:ethereum
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
  (n state (owner-changed:l1 ~bud (addr %bud-key-0)))
::
::  ~wes is for testing sponsors of stars
::
++  init-wes
  |=  =^state:naive
  ^-  [effects:naive ^state:naive]
  (n state (owner-changed:l1 ~wes (addr %wes-key-0)))
::
::  ~dopbud is for testing L1 ownership with L2 spawn proxy
::
++  init-dopbud
  |=  =^state:naive
  ^-  [effects:naive ^state:naive]
  =^  f1  state  (init-bud state)
  =^  f2  state  (n state (owner-changed:l1 ~dopbud (addr %dopbud-key-0)))
  =^  f3  state  (n state (changed-spawn-proxy:l1 ~dopbud deposit-address:naive))
  [:(welp f1 f2 f3) state]
::
::  ~marbud is for testing L2 ownership
::
++  init-marbud
  |=  =^state:naive
  ^-  [effects:naive ^state:naive]
  =^  f1  state  (init-bud state)
  =^  f2  state  (n state (owner-changed:l1 ~marbud (addr %marbud-key-0)))
  =^  f3  state  (n state (owner-changed:l1 ~marbud deposit-address:naive))
  [:(welp f1 f2 f3) state]
::
:: ~litbud is for testing L2 sponsorship
::
++  init-litbud
  |=  =^state:naive
  ^-  [effects:naive ^state:naive]
  :: ~bud should already be spawned, though trying to init ~bud again shouldn't matter i think?
  :: =^  f1  state  (init-bud state)
  =^  f2  state  (n state (owner-changed:l1 ~litbud (addr %litbud-key-0)))
  =^  f3  state  (n state (owner-changed:l1 ~litbud deposit-address:naive))
  [:(welp f2 f3) state]
::
::  ~sambud is for testing L1 stars
::
++  init-sambud
   |=  =^state:naive
   ^-  [effects:naive ^state:naive]
   =^  f1  state  (init-bud state)
   =^  f2  state  (n state (owner-changed:l1 ~sambud (addr %sambud-key-0)))
   [:(welp f1 f2) state]
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
  ++  changed-keys
    |=  [=ship suite=@ encr=@ auth=@ life=@]
    =/  keys=@ux
      %:  can  8
        1^life
        1^suite
        1^auth
        1^encr
        ~
      ==
    (log changed-keys:log-names:naive keys ship ~)
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
--
::
:: Common values used for tests
::
|%
::
++  encr    (shax 'You will forget that you ever read this sentence.')
++  auth    (shax 'You cant know that this sentence is true.')
++  suit     1
::
++  marbud-own  [~marbud %marbud-key-0 %own]
++  marbud-spn  [~marbud %marbud-skey %spawn]
++  marbud-mgt  [~marbud %marbud-mkey %manage]
::
++  dopbud-own  [~dopbud %dopbud-key-0 %own]
::
++  litbud-own  [~litbud %litbud-key-0 %own]
::
++  lt-own      [~linnup-torsyx %lt-key-0 %own]
++  lt-xfr      [~linnup-torsyx %lt-key-0 %transfer]
--
::
:: Tests
::
|%
::
++  test-log  ^-  tang
  %+  expect-eq
    !>
    :-  [%point ~bud %owner (addr %bud-key-0)]~
    :_  [~ ~]  :_  [~ ~]
    :-  ~bud
    %*(. *point:naive dominion %l1, owner.own (addr %bud-key-0)^0, who.sponsor.net ~bud)
  ::
    !>
    %^  naive  verifier  1.337  :-  *^state:naive
    :*  %log  *@ux  *@ux
        owner-changed:log-names:naive  (@ux ~bud)  (addr %bud-key-0)  ~
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
  =/  marbud-transfer    [%transfer-point (addr %marbud-key-0) |]
  =/  marbud-transfer-2  [%transfer-point (addr %marbud-key-1) |]
  ::
  %+  expect-eq
    !>  [(addr %marbud-key-1) 2]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 marbud-own marbud-transfer))
    =^  f  state  (n state %bat q:(gen-tx 1 marbud-own marbud-transfer-2))
    owner.own:(~(got by points.state) ~marbud)
::
++  test-l1-changed-spawn-proxy  ^-  tang
  %+  expect-eq
    !>  [(addr %bud-skey) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-bud state)
    =^  f  state  (n state (changed-spawn-proxy:l1 ~bud (addr %bud-skey)))
    spawn-proxy.own:(~(got by points.state) ~bud)
::
++  test-l1-changed-transfer-proxy  ^-  tang
  %+  expect-eq
    !>  [(addr %bud-key-1) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-bud state)
    =^  f  state  (n state (changed-transfer-proxy:l1 ~bud (addr %bud-key-1)))
    transfer-proxy.own:(~(got by points.state) ~bud)
::
++  test-l1-changed-management-proxy  ^-  tang
  %+  expect-eq
    !>  [(addr %bud-mkey) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-bud state)
    =^  f  state  (n state (changed-management-proxy:l1 ~bud (addr %bud-mkey)))
    management-proxy.own:(~(got by points.state) ~bud)
::
++  test-l1-changed-voting-proxy  ^-  tang
  %+  expect-eq
    !>  [(addr %bud-vkey) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-bud state)
    =^  f  state  (n state (changed-voting-proxy:l1 ~bud (addr %bud-vkey)))
    voting-proxy.own:(~(got by points.state) ~bud)
::
++  test-l1-changed-keys  ^-  tang
  =/  life          1
  =/  new-keys      [~bud suit encr auth life]
  ::
  %+  expect-eq
    !>  [suit auth encr]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-bud state)
    =^  f  state  (n state (changed-keys:l1 new-keys))
    |1:keys.net:(~(got by points.state) ~bud)
::
++  test-l1-star-escape-requested  ^-  tang
  %+  expect-eq
    !>  [~ ~wes]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-wes state)
    =^  f  state  (init-sambud state)
    =^  f  state  (n state (escape-requested:l1 ~sambud ~wes))
    escape.net:(~(got by points.state) ~sambud)
::
++  test-l1-star-escape-canceled  ^-  tang
  %+  expect-eq
    !>  ~
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-wes state)
    =^  f  state  (init-sambud state)
    =^  f  state  (n state (escape-requested:l1 ~sambud ~wes))
    =^  f  state  (n state (escape-canceled:l1 ~sambud ~wes))
    escape.net:(~(got by points.state) ~sambud)
::
++  test-l1-star-adopt-accept  ^-  tang
  %+  expect-eq
    !>  [~ %.y ~wes]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-wes state)
    =^  f  state  (init-sambud state)
    =^  f  state  (n state (escape-requested:l1 ~sambud ~wes))
    =^  f  state  (n state (escape-accepted:l1 ~sambud ~wes))
    [escape.net sponsor.net]:(~(got by points.state) ~sambud)
::
++  test-l1-star-lost-sponsor  ^-  tang
  %+  expect-eq
    !>  [~ %.n ~bud]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-sambud state)
    =^  f  state  (n state (lost-sponsor:l1 ~sambud ~bud))
    [escape.net sponsor.net]:(~(got by points.state) ~sambud)
::
::  TODO: sponsorship tests for l1 planets, and L1/L2 sponsorship tests
::
++  test-l2-set-spawn-proxy  ^-  tang
  =/  marbud-sproxy  [%set-spawn-proxy (addr %marbud-skey)]
  ::
  %+  expect-eq
    !>  [(addr %marbud-skey) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 marbud-own marbud-sproxy))
    spawn-proxy.own:(~(got by points.state) ~marbud)
::
++  test-l2-set-transfer-proxy  ^-  tang
  =/  marbud-tproxy  [%set-transfer-proxy (addr %marbud-tkey)]
  ::
  %+  expect-eq
    !>  [(addr %marbud-tkey) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 marbud-own marbud-tproxy))
    transfer-proxy.own:(~(got by points.state) ~marbud)
::
++  test-l2-set-management-proxy  ^-  tang
  =/  marbud-mproxy  [%set-management-proxy (addr %marbud-mkey)]
  ::
  %+  expect-eq
    !>  [(addr %marbud-mkey) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 marbud-own marbud-mproxy))
    management-proxy.own:(~(got by points.state) ~marbud)
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
  =/  marbud-sproxy  [%set-spawn-proxy (addr %marbud-skey)]
  =/  lt-spawn       [%spawn ~linnup-torsyx (addr %lt-key-0)]
  ::
  ;:  weld
    %+  expect-eq
    ::  Tests l2 spawning with ownership
      !>  [`@ux`(addr %lt-key-0) 0]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-own lt-spawn))
      transfer-proxy.own:(~(got by points.state) ~linnup-torsyx)
    ::
    %+  expect-eq
    ::  Tests l2 spawning with spawn proxy
      !>  [`@ux`(addr %lt-key-0) 0]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-own marbud-sproxy))
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-spn lt-spawn))
      transfer-proxy.own:(~(got by points.state) ~linnup-torsyx)
  ==
::
++  test-marbud-l2-double-spawn  ^-  tang
  ::  Attempts to spawn the same planet twice, once with ownership and once with spawn proxy
  =/  marbud-sproxy   [%set-spawn-proxy (addr %marbud-skey)]
  =/  lt-spawn-0      [%spawn ~linnup-torsyx (addr %lt-key-0)]
  =/  lt-spawn-1      [%spawn ~linnup-torsyx (addr %lt-key-1)]
  ::
  %-  expect-fail
    |.
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 marbud-own marbud-sproxy))
    =^  f  state  (n state %bat q:(gen-tx 1 marbud-own lt-spawn-0))
    =^  f  state  (n state %bat q:(gen-tx 0 marbud-spn lt-spawn-1))
    state
::
++  test-marbud-l2-change-keys  ^-  tang
  =/  new-keys       [%configure-keys suit encr auth |]
  =/  marbud-mproxy  [%set-management-proxy (addr %marbud-mkey)]
  ::
  ;:  weld
    %+  expect-eq
      !>  [suit auth encr]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-own new-keys))
      |1:keys.net:(~(got by points.state) ~marbud)
    ::
    %+  expect-eq
      !>  [suit auth encr]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-own marbud-mproxy))
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-mgt new-keys))
      |1:keys.net:(~(got by points.state) ~marbud)
    ::
    :: TODO: make sure nobody else can change these keys
  ==
::
:: TODO: transfer breach via transfer proxy
++  test-marbud-l2-proxies-transfer  ^-  tang
  =/  marbud-t1                  [~marbud %marbud-key-1 %transfer]
  =/  new-keys                   [%configure-keys suit encr auth |]
  =/  marbud-sproxy              [%set-spawn-proxy (addr %marbud-skey)]
  =/  marbud-mproxy              [%set-management-proxy (addr %marbud-mkey)]
  =/  marbud-tproxy              [%set-transfer-proxy (addr %marbud-key-1)]
  =/  marbud-transfer-breach     [%transfer-point (addr %marbud-key-1) &]
  =/  marbud-transfer-no-breach  [%transfer-point (addr %marbud-key-1) |]
  ::
  ;:  weld
    %+  expect-eq
    ::  Tests that proxies are reset on transfer with breach
    ::
      !>
      :*  [(addr %marbud-key-1) 3]       :: ownership
          [0 0]                          :: spawn-proxy
          [0 0]                          :: management-proxy
          [0 0]                          :: voting-proxy
          [0 1]                          :: transfer-proxy
      ==
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-own marbud-sproxy))
      =^  f  state  (n state %bat q:(gen-tx 1 marbud-own marbud-mproxy))
      =^  f  state  (n state %bat q:(gen-tx 2 marbud-own marbud-tproxy))
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-t1 marbud-transfer-breach))
      ^-  [[@ @] [@ @] [@ @] [@ @] [@ @]]
      own:(~(got by points.state) ~marbud)
    ::
    %+  expect-eq
    ::  Tests that networking keys are reset on transfer with breach
      !>
      [0 0 0]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-own new-keys))
      =^  f  state  (n state %bat q:(gen-tx 1 marbud-own [%transfer-point (addr %marbud-key-0) &]))
      |1:keys.net:(~(got by points.state) ~marbud)
    ::
    %+  expect-eq
    ::  Tests that proxies are not reset when transfering without breach
      !>
      :*  [(addr %marbud-key-1) 3]       :: ownership
          [(addr %marbud-skey) 0]        :: spawn-proxy
          [(addr %marbud-mkey) 0]        :: management-proxy
          [0 0]                          :: voting-proxy
          [0 1]                          :: transfer-proxy
      ==
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-own marbud-sproxy))
      =^  f  state  (n state %bat q:(gen-tx 1 marbud-own marbud-mproxy))
      =^  f  state  (n state %bat q:(gen-tx 2 marbud-own marbud-tproxy))
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-t1 marbud-transfer-no-breach))
      ^-  [[@ @] [@ @] [@ @] [@ @] [@ @]]
      own:(~(got by points.state) ~marbud)
    ::
    %+  expect-eq
    ::  Tests that networking keys are not reset when transfering without breach
      !>
      [suit auth encr]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-own new-keys))
      =^  f  state  (n state %bat q:(gen-tx 1 marbud-own [%transfer-point (addr %marbud-key-0) |]))
      |1:keys.net:(~(got by points.state) ~marbud)
  ==
::
:: TODO: life+rift changes via transfer proxy
::
++  test-marbud-keys-life-rift  ^-  tang
  =/  new-keys-no-reset       [%configure-keys suit encr auth |]
  =/  new-keys-yes-reset      [%configure-keys suit encr auth &]
  =/  zero-keys-no-reset      [%configure-keys 0 0 0 |]
  =/  zero-keys-yes-reset     [%configure-keys 0 0 0 &]
  ::
  ;:  weld
    %+  expect-eq
    ::  breach=%.n
      !>  [0 1]                   :: [rift life]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-own new-keys-no-reset))
      [rift.net life.keys.net]:(~(got by points.state) ~marbud)
    ::
    %+  expect-eq
    ::  breach=%.y
      !>  [1 1]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-own new-keys-yes-reset))
      [rift.net life.keys.net]:(~(got by points.state) ~marbud)
    ::
    %+  expect-eq
    ::  networking keys set incremenets life, reset=%.y
    ::  then zero keys and transfer, should increment rift but not life
    ::
      !>  [2 2]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-own new-keys-yes-reset)) :: inc life and rift
      =^  f  state  (n state %bat q:(gen-tx 1 marbud-own zero-keys-no-reset)) :: inc life
      =^  f  state  (n state %bat q:(gen-tx 2 marbud-own zero-keys-yes-reset)) :: inc rift
      [rift.net life.keys.net]:(~(got by points.state) ~marbud)
    ::
  ==
::
++  test-marbud-transfer-life-rift  ^-  tang
  =/  new-keys-no-reset           [%configure-keys suit encr auth |]
  =/  new-keys-yes-reset          [%configure-keys suit encr auth &]
  =/  zero-keys-no-reset          [%configure-keys 0 0 0 |]
  =/  zero-keys-yes-reset         [%configure-keys 0 0 0 &]
  =/  marbud-transfer-no-breach   [%transfer-point (addr %marbud-key-1) |]
  =/  marbud-transfer-yes-breach  [%transfer-point (addr %marbud-key-1) &]
  ::
  ;:  weld
    %+  expect-eq
    ::  networking keys not set, reset=%.n
    ::
      !>  [0 0]                   :: [rift life]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-own marbud-transfer-no-breach))
      [rift.net life.keys.net]:(~(got by points.state) ~marbud)
    ::
    %+  expect-eq
    ::  networking keys not set, reset=%.y
    ::
      !>  [0 0]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-own marbud-transfer-no-breach))
      [rift.net life.keys.net]:(~(got by points.state) ~marbud)
    ::
    %+  expect-eq
    ::  networking keys set incremenets life, reset=%.y
    ::  then zero keys and transfer, should increment rift but not life
    ::  TODO: transferring and reset with already zeroed keys ought to incr rift but not life, right?
    ::  but currently the transfer w/ reset increments both life and rift, despite keys already being 0
    ::
      !>  [2 2]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-own new-keys-yes-reset)) :: inc life and rift
      =^  f  state  (n state %bat q:(gen-tx 1 marbud-own zero-keys-no-reset)) :: inc life
      =^  f  state  (n state %bat q:(gen-tx 2 marbud-own marbud-transfer-yes-breach)) :: inc rift
      [rift.net life.keys.net]:(~(got by points.state) ~marbud)
    ::
 ==
::
++  test-dopbud-l2-spawn  ^-  tang
  =/  pp-spawn        [%spawn ~palsep-picdun (addr %pp-key-0)]
  ::
  %+  expect-eq
    !>  [`@ux`(addr %pp-key-0) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-dopbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 dopbud-own pp-spawn))
    transfer-proxy.own:(~(got by points.state) ~palsep-picdun)
::
++  test-dopbud-l2-spawn-after-transfer  ^-  tang
  =/  pp-spawn        [%spawn ~palsep-picdun (addr %pp-key-0)]
  =/  lr-spawn        [%spawn ~laclur-rachul (addr %lr-key-0)]
  =/  dopbud-own-1    [~dopbud %dopbud-key-1 %own]
  ::
  %+  expect-eq
    !>  [`@ux`(addr %lr-key-0) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-dopbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 dopbud-own pp-spawn))
    =^  f  state  (n state (owner-changed:l1 ~dopbud (addr %dopbud-key-1)))
    =^  f  state  (n state %bat q:(gen-tx 1 dopbud-own-1 lr-spawn))
    transfer-proxy.own:(~(got by points.state) ~laclur-rachul)
::
::  ++  test-sambud-double-spawn  ^-  tang
::    ::
::    ::  TODO: Not sure of the right way to write this test yet. Current iteration
::    ::  doesn't even compile
::    ::
::    %-  expect-fail
::      |.
::      ?<
::        ?=  [`@ux`(addr %ld-key-1) 0]
::        =|  =^state:naive
::        =^  f  state  (init-sambud state)
::        =^  f  state  (n state (owner-changed:l1 ~lisdur-fodrys (addr %ld-key-0)))
::        =^  f  state  (n state (changed-spawn-proxy:l1 ~sambud deposit-address:naive))
::        =^  f  state  (n state %bat q:(spawn:l2 0 ~sambud %sambud-key-0 %own ~lisdur-fodrys (addr %ld-key-1)))
::        transfer-proxy.own:(~(got by points.state) ~lisdur-fodrys)
::      %.n
::
::  ++  test-sambud-double-spawn-w-proxy  ^-  tang
::    ::
::    ::  Same confusion as above
::    ::
::    %-  expect-fail
::      |.
::      =|  =^state:naive
::      =^  f  state  (init-sambud state)
::      =^  f  state  (n state (owner-changed:l1 ~lisdur-fodrys (addr %ld-key-0)))
::      =^  f  state  (n state (owner-changed:l1 ~sambud deposit-address:naive))
::      =^  f  state  (n state %bat q:(spawn:l2 0 ~sambud %sambud-key-0 %own ~lisdur-fodrys (addr %ld-key-1)))
::      state
::
++  test-linnup-torsyx-l2-transfer-ownership  ^-  tang
  =/  lt-spawn                [%spawn ~linnup-torsyx (addr %lt-key-0)]
  =/  lt-transfer-yes-breach  [%transfer-point (addr %lt-key-0) &]
  ::
  %+  expect-eq
    !>  [`@ux`(addr %lt-key-0) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 marbud-own lt-spawn))
    =^  f  state  (n state %bat q:(gen-tx 0 lt-xfr lt-transfer-yes-breach))
    owner.own:(~(got by points.state) ~linnup-torsyx)
::
++  test-palsep-picdun-l2-transfer-ownership  ^-  tang
  =/  pp-xfr                  [~palsep-picdun %pp-key-0 %transfer]
  =/  pp-spawn                [%spawn ~palsep-picdun (addr %pp-key-0)]
  =/  pp-transfer-yes-breach  [%transfer-point (addr %pp-key-0) &]
  %+  expect-eq
    !>  [`@ux`(addr %pp-key-0) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-dopbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 dopbud-own pp-spawn))
    =^  f  state  (n state %bat q:(gen-tx 0 pp-xfr pp-transfer-yes-breach))
    owner.own:(~(got by points.state) ~palsep-picdun)
::
++  test-linnup-torsyx-l2-escape-request  ^-  tang
  ::  TODO: Are you supposed to be able to request escape to a non-existent star?
  =/  lt-spawn                   [%spawn ~linnup-torsyx (addr %lt-key-0)]
  =/  lt-transfer-yes-breach     [%transfer-point (addr %lt-key-0) &]
  ::
  %+  expect-eq
    !>  [~ ~litbud]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (init-litbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 marbud-own lt-spawn))
    =^  f  state  (n state %bat q:(gen-tx 0 lt-xfr lt-transfer-yes-breach))
    =^  f  state  (n state %bat q:(gen-tx 0 lt-own [%escape ~litbud]))
    escape.net:(~(got by points.state) ~linnup-torsyx)
::
++  test-linnup-torsyx-l2-cancel-escape-request  ^-  tang
  =/  lt-spawn                   [%spawn ~linnup-torsyx (addr %lt-key-0)]
  =/  lt-transfer-yes-breach     [%transfer-point (addr %lt-key-0) &]
  ::
  %+  expect-eq
    !>  ~
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (init-litbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 marbud-own lt-spawn))
    =^  f  state  (n state %bat q:(gen-tx 0 lt-xfr lt-transfer-yes-breach))
    =^  f  state  (n state %bat q:(gen-tx 0 lt-own [%escape ~litbud]))
    =^  f  state  (n state %bat q:(gen-tx 1 lt-own [%cancel-escape ~litbud]))
    escape.net:(~(got by points.state) ~linnup-torsyx)
::
++  test-linnup-torsyx-l2-adopt-accept  ^-  tang
  =/  lt-spawn                  [%spawn ~linnup-torsyx (addr %lt-key-0)]
  =/  lt-transfer-yes-breach    [%transfer-point (addr %lt-key-0) &]
  ::
  %+  expect-eq
    !>  [~ %.y ~litbud]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (init-litbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 marbud-own lt-spawn))
    =^  f  state  (n state %bat q:(gen-tx 0 lt-xfr lt-transfer-yes-breach))
    =^  f  state  (n state %bat q:(gen-tx 0 lt-own [%escape ~litbud]))
    =^  f  state  (n state %bat q:(gen-tx 0 litbud-own [%adopt ~linnup-torsyx]))
    [escape.net sponsor.net]:(~(got by points.state) ~linnup-torsyx)
::
++  test-linnup-torsyx-l2-adopt-reject  ^-  tang
  ::  TODO: at the moment the default sponsor is always ~zod, but it should probably
  ::  be ~marbud here
  =/  lt-spawn                  [%spawn ~linnup-torsyx (addr %lt-key-0)]
  =/  lt-transfer-yes-breach    [%transfer-point (addr %lt-key-0) &]
  ::
  %+  expect-eq
    !>  ~
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (init-litbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 marbud-own lt-spawn))
    =^  f  state  (n state %bat q:(gen-tx 0 lt-xfr lt-transfer-yes-breach))
    =^  f  state  (n state %bat q:(gen-tx 0 lt-own [%escape ~litbud]))
    =^  f  state  (n state %bat q:(gen-tx 0 litbud-own [%reject ~linnup-torsyx]))
    escape.net:(~(got by points.state) ~linnup-torsyx)
::
++  test-linnup-torsyx-l2-detach  ^-  tang
  =/  lt-spawn                  [%spawn ~linnup-torsyx (addr %lt-key-0)]
  =/  lt-transfer-yes-breach    [%transfer-point (addr %lt-key-0) &]
  ::
  %+  expect-eq
    !>  [~ %.n ~marbud]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (init-litbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 marbud-own lt-spawn))
    =^  f  state  (n state %bat q:(gen-tx 0 lt-xfr lt-transfer-yes-breach))
    =^  f  state  (n state %bat q:(gen-tx 1 marbud-own [%detach ~linnup-torsyx]))
    [escape.net sponsor.net]:(~(got by points.state) ~linnup-torsyx)
::
::  TODO: signature format changed; regenerate
::
::  ++  test-metamask-signature  ^-  tang
::    =/  meta-owner=address
::      (hex-to-num:ethereum '0xb026b0AA6e686F2386051b31A03E5fB95513e1c0')
::    =/  tx  0x123.0000.0102.0a00.0001.0200
::    =/  sig
::      %-  hex-to-num:ethereum
::      ::  Must reverse endianness of tx to sign in metamask
::      ::
::      %^  cat  3
::        '0x5b85936ab7b9db8d72416648e6eb1b844a4545ddb7c7c646a74bc3a4fb001a2'
::      '8583bf12ca837b289036a6cc9e6359ed07dda2b87929b5dd7189a3057a395341f1c'
::    ::
::    %+  expect-eq
::      !>  [0x123 0]
::    ::
::      !>
::      =|  =^state:naive
::      =^  f  state  (init-marbud state)
::      ::  =^  f  state  (n state %bat q:(transfer-point:l2 0 ~marbud (key ~marbud) %own &))
::      ::  =^  f  state  (n state %bat q:(set-transfer-proxy:l2 1 ~marbud %own 0x123))
::      =^  f  state
::        %^  n  state  %bat
::        q:(transfer-point:l2 0 ~marbud %marbud-key-0 meta-owner %own &)
::      =^  f  state  (n state %bat (cat 3 sig tx))
::      transfer-proxy.own:(~(got by points.state) ~marbud)
--
