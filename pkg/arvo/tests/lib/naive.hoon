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
  =/  prepared-data  (dad:naive 5 nonce tx)
  =/  sign-data
    %-  keccak-256:keccak:crypto
    %-  as-octs:mimes:html
    %^  cat  3  '\19Ethereum Signed Message:\0a'
    %^  cat  3  (rsh [3 2] (scot %ui (met 3 prepared-data)))
    prepared-data
  =+  (ecdsa-raw-sign:secp256k1:secp:crypto sign-data pk)
  (cat 3 (can 3 1^v 32^s 32^r ~) tx)
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
          %set-transfer-proxy       %10
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
++  test-metamask-signature  ^-  tang
  =/  meta-owner=address
    (hex-to-num:ethereum '0xb026b0AA6e686F2386051b31A03E5fB95513e1c0')
  =/  tx  0x123.0000.0102.0a00.0001.0200
  =/  sig
    %-  hex-to-num:ethereum
    :: %^  cat  3  '0xbcee11aad81466d8693571bdd020a2cc8ca7cd4a717bbfdedbe5d5296b596005'
    :: '211e6c1a804ea0489ac15ff1dca7a0803f61c2fb473701d100dc9c07bbe6ba6f1c'
    ::  '0xdede6cb45463d5822e2558cd0aec6835c6500acf928754f7147bc066eaa1f5bb5913d66292e0f5c368611dc8fe2a9635b4d692ee64684a73bb581f31ec6bbefa1c'
    ::  Must reverse endianness of tx to sign in metamask
    %^  cat  3
      '0x5b85936ab7b9db8d72416648e6eb1b844a4545ddb7c7c646a74bc3a4fb001a2'
    '8583bf12ca837b289036a6cc9e6359ed07dda2b87929b5dd7189a3057a395341f1c'
  ::
  %+  expect-eq
    !>  [0x123 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    ::  =^  f  state  (n state %bat (transfer-point:l2 0 ~marbud (key ~marbud) %own &))
    ::  =^  f  state  (n state %bat (set-transfer-proxy:l2 1 ~marbud %own 0x123))
    =^  f  state  (n state %bat (transfer-point:l2 0 ~marbud meta-owner %own &))
    =^  f  state  (n state %bat (cat 3 sig tx))
    transfer-proxy.own:(~(got by points.state) ~marbud)
--
