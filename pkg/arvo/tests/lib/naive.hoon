/+  *test, naive, ethereum
|%
++  address  @ux
++  n  |=([=^state:naive =^input:naive] (%*(. naive lac |) verifier +<))
::  TODO: does this uniquely produce the pubkey?
::
++  verifier
  ^-  ^verifier:naive
  |=  [dat=octs v=@ r=@ s=@]
  ?:  (gth v 3)  ~  ::  TODO: move to jet
  =/  result
    %-  mule
    |.
    =,  secp256k1:secp:crypto
    %-  address-from-pub:key:ethereum
    %-  serialize-point
    (ecdsa-raw-recover (keccak-256:keccak:crypto dat) v r s)
  ?-  -.result
    %|  ~
    %&  `p.result
  ==
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
  (n state (owner-changed:l1 ~bud (addr ~bud)))
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
::  ~sambud is for testing L1 stars attempting L2 actions
::
++  init-sambud
   |=  =^state:naive
   ^-  [effects:naive ^state:naive]
   =^  f1  state  (init-bud state)
   =^  f2  state  (n state (owner-changed:l1 ~sambud (addr %sambud-key-0)))
   [:(welp f1 f2) state]
::
++  sign-tx
  |=  [pk=@ nonce=@ud tx=octs]  ^-  octs
  =/  prepared-data=octs  [(add 4 p.tx) (can 3 4^nonce tx ~)]
  =/  sign-data
    =/  len  (rsh [3 2] (scot %ui p.prepared-data))
    %-  keccak-256:keccak:crypto
    :-  :(add 26 (met 3 len) p.prepared-data)
    %:  can  3
      26^'\19Ethereum Signed Message:\0a'
      (met 3 len)^len
      prepared-data
      ~
    ==
  =+  (ecdsa-raw-sign:secp256k1:secp:crypto sign-data pk)
  :-  :(add 1 32 32 p.tx)
  (can 3 1^v 32^s 32^r tx ~)
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
    |=  [=ship encr=@ auth=@ suite=@ life=@]
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
++  l2
  ::
  |%
  ::
  ++  spawn
    |=  [nonce=@ud parent=ship pk=@ proxy=@tas child=ship =address]  ^-  octs
    %^  sign-tx  pk  nonce
    (take-ship-address:bits %spawn parent proxy child address)
  ::
  ++  transfer-point
    |=  [nonce=@ud =ship pk=@ =address proxy=@tas reset=?]  ^-  octs
    %^  sign-tx  pk  nonce
    :-  :(add 1 4 1 4 20)
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
    |=  $:  nonce=@ud  =ship  pk=@  proxy=@tas
            breach=@  encrypt=@  auth=@  crypto-suite=@
        ==
    ^-  octs
    %^  sign-tx  pk  nonce
    :-  :(add 1 4 1 4 32 32 4)
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
    |=  [nonce=@ud child=ship pk=@ proxy=@tas parent=ship]  ^-  octs
    %^  sign-tx  pk  nonce
    (take-escape:bits %escape child proxy parent)
  ::
  ++  cancel-escape
    |=  [nonce=@ud child=ship pk=@ proxy=@tas parent=ship]  ^-  octs
    %^  sign-tx  pk  nonce
    (take-escape:bits %cancel-escape child proxy parent)
  ::
  ++  adopt
    |=  [nonce=@ud child=ship pk=@ proxy=@tas parent=ship]  ^-  octs
    %^  sign-tx  pk  nonce
    (take-escape:bits %adopt child proxy parent)
  ::
  ++  reject
    |=  [nonce=@ud child=ship pk=@ proxy=@tas parent=ship]  ^-  octs
    %^  sign-tx  pk  nonce
    (take-escape:bits %reject child proxy parent)
  ::
  ++  detach
    |=  [nonce=@ud child=ship pk=@ proxy=@tas parent=ship]  ^-  octs
    %^  sign-tx  pk  nonce
    (take-escape:bits %detach child proxy parent)
  ::
  ++  set-management-proxy
    |=  [nonce=@ud =ship pk=@ proxy=@tas =address]  ^-  octs
    %^  sign-tx  pk  nonce
    ^-  octs
    (take-ship-address:bits %set-management-proxy ship proxy ship address)
  ::
  ++  set-spawn-proxy
    |=  [nonce=@ud =ship pk=@ proxy=@tas =address]  ^-  octs
    %^  sign-tx  pk  nonce
    (take-ship-address:bits %set-spawn-proxy ship proxy ship address)
  ::
  ++  set-transfer-proxy
    |=  [nonce=@ud =ship pk=@ proxy=@tas =address]  ^-  octs
    %^  sign-tx  pk  nonce
    (take-ship-address:bits %set-transfer-proxy ship proxy ship address)
  ::
  ++  bits
    ::
    |%
    ::
    ::  TODO: Shouldn't need to pass all these arguments along - they should already be in the subject somewhere
    ::
    ++  take-escape
      |=  [action=@tas child=ship proxy=@tas parent=ship]  ^-  octs
      =/  op
        ?+  action  !!
          %escape         %3
          %cancel-escape  %4
          %adopt          %5
          %reject         %6
          %detach         %7
        ==
      :-  :(add 1 4 1 4 4)
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
      |=  [action=@tas from=ship proxy=@tas target=ship =address]  ^-  octs
      =/  op
        ?+  action  !!
          %spawn                    %1
          %set-management-proxy     %8
          %set-spawn-proxy          %9
          %set-transfer-proxy       %10
        ==
      :-  :(add 1 4 1 4 20)
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
      1^(can 0 3^proxy 5^0 ~)
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
    =^  f  state  (n state %bat q:(transfer-point:l2 0 ~marbud %marbud-key-0 (addr %marbud-key-0) %own |))
    =^  f  state  (n state %bat q:(transfer-point:l2 1 ~marbud %marbud-key-0 0x234 %own |))
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
    =^  f  state  (n state %bat q:(set-spawn-proxy:l2 0 ~marbud %marbud-key-0 %own 0x123))
    spawn-proxy.own:(~(got by points.state) ~marbud)
::
++  test-l2-set-transfer-proxy  ^-  tang
  %+  expect-eq
    !>  [0x123 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat q:(set-transfer-proxy:l2 0 ~marbud %marbud-key-0 %own 0x123))
    transfer-proxy.own:(~(got by points.state) ~marbud)
::
++  test-l2-set-management-proxy  ^-  tang
  %+  expect-eq
    !>  [0x123 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat q:(set-management-proxy:l2 0 ~marbud %marbud-key-0 %own 0x123))
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
  %+  expect-eq
    !>  [`@ux`(addr %ll-key-0) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat q:(spawn:l2 0 ~marbud %marbud-key-0 %own ~linnup-torsyx (addr %ll-key-0)))
    transfer-proxy.own:(~(got by points.state) ~linnup-torsyx)
::
++  test-marbud-l2-spawn-w-proxy  ^-  tang
  %+  expect-eq
    !>  [`@ux`(addr %ll-key-0) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat q:(set-spawn-proxy:l2 0 ~marbud %marbud-key-0 %own (addr %marbud-spawn-0)))
    =^  f  state  (n state %bat q:(spawn:l2 0 ~marbud %marbud-spawn-0 %spawn ~linnup-torsyx (addr %ll-key-0)))
    transfer-proxy.own:(~(got by points.state) ~linnup-torsyx)
::
++  test-dopbud-l2-spawn  ^-  tang
  %+  expect-eq
    !>  [`@ux`(addr %pp-key-0) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-dopbud state)
    =^  f  state  (n state %bat q:(spawn:l2 0 ~dopbud %dopbud-key-0 %own ~palsep-picdun (addr %pp-key-0)))
    transfer-proxy.own:(~(got by points.state) ~palsep-picdun)
::
++  test-dopbud-l2-spawn-after-transfer  ^-  tang
  %+  expect-eq
    !>  [`@ux`(addr %lr-key-0) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-dopbud state)
    =^  f  state  (n state %bat q:(spawn:l2 0 ~dopbud %dopbud-key-0 %own ~palsep-picdun (addr %pp-key-0)))
    =^  f  state  (n state (owner-changed:l1 ~dopbud (addr %dopbud-key-1)))
    =^  f  state  (n state %bat q:(spawn:l2 1 ~dopbud %dopbud-key-1 %own ~laclur-rachul (addr %lr-key-0)))
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
  %+  expect-eq
    !>  [`@ux`(addr %lt-key-0) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat q:(spawn:l2 0 ~marbud %marbud-key-0 %own ~linnup-torsyx (addr %lt-key-0)))
    =^  f  state  (n state %bat q:(transfer-point:l2 0 ~linnup-torsyx %lt-key-0 (addr %lt-key-0) %transfer &))
    owner.own:(~(got by points.state) ~linnup-torsyx)
::
++  test-palsep-picdun-l2-transfer-ownership  ^-  tang
  %+  expect-eq
    !>  [`@ux`(addr %pp-key-0) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-dopbud state)
    =^  f  state  (n state %bat q:(spawn:l2 0 ~dopbud %dopbud-key-0 %own ~palsep-picdun (addr %pp-key-0)))
    =^  f  state  (n state %bat q:(transfer-point:l2 0 ~palsep-picdun %pp-key-0 (addr %pp-key-0) %transfer &))
    owner.own:(~(got by points.state) ~palsep-picdun)
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
