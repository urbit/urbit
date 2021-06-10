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
::  ~rut is for "full testing"
::  ~rigrut is L1 star
::  ~larsyx-mapmeg is L1 planet under ~rigrut
::  ~holrut is L1 star w/ L2 spawn proxy
::  ~rabsum-ravtyd is L1 planet under ~holrut
::  ~dovmul-mogryt is L2 planet under ~holrut made w/ %own proxy
::  ~pidted-dacnum is L2 planet under ~holrut made w/ %spawn proxy predeposited
::  TODO: L2 planet ~nacbes-mogmev made with L2 spawn proxy postdeposited (currently doesnt work)
::
::  ~losrut is L2 star
::  ~radres-tinnyl is L1 planet under ~losrut
::  ~pinpun-pilsun is L2 planet under ~losrut made w/ %own proxy
::  ~habtyc-nibpyx is L2 planet under ~losrut made w/ %spawn proxy predeposited
::  ~disryt-nolpet is L2 planet under ~losrut made w/ %spawn proxy postdeposited
::
::  nonces in the end state (0 if not stated):
::  ~holrut %own 1
::  ~losrut %own 2
::  ~losrut %spawn 1
::
++  init-rut-full
  |=  =^state:naive
  ^-  [effects:naive ^state:naive]
  =/  dm-spawn  [[~holrut %own] %spawn ~dovmul-mogryt (addr %holrut-dm-key-0)]
  =/  dm-xfer  [[~dovmul-mogryt %transfer] %transfer-point (addr %holrut-dm-key-0) &]
  =/  pd-spawn  [[~holrut %spawn] %spawn ~pidted-dacnum (addr %holrut-pd-key-0)]
  =/  pd-xfer  [[~pidted-dacnum %transfer] %transfer-point (addr %holrut-pd-key-0) &]
  =/  pp-spawn  [[~losrut %own] %spawn ~pinpun-pilsun (addr %losrut-pp-key-0)]
  =/  pp-xfer  [[~pinpun-pilsun %transfer] %transfer-point (addr %losrut-pp-key-0) &]
  =/  hn-spawn  [[~losrut %spawn] %spawn ~habtyc-nibpyx (addr %losrut-hn-key-0)]
  =/  hn-xfer  [[~habtyc-nibpyx %transfer] %transfer-point (addr %losrut-hn-key-0) &]
  =/  dn-spawn  [[~losrut %spawn] %spawn ~disryt-nolpet (addr %losrut-dn-key-0)]
  =/  dn-xfer  [[~disryt-nolpet %transfer] %transfer-point (addr %losrut-dn-key-0) &]
  =/  losrut-sproxy  [[~losrut %spawn] %set-spawn-proxy (addr %losrut-skey-1)]
  =^  f1  state  (n state (owner-changed:l1 ~rut (addr %rut-key-0)))
  =^  f2  state  (n state (owner-changed:l1 ~rigrut (addr %rigrut-key-0)))
  =^  f3  state  (n state (owner-changed:l1 ~holrut (addr %holrut-key-0)))
  =^  f4  state  (n state (owner-changed:l1 ~losrut (addr %losrut-key-0)))
  =^  f5  state  (n state (owner-changed:l1 ~larsyx-mapmeg (addr %rigrut-lm-key-0)))
  =^  f6  state  (n state (owner-changed:l1 ~rabsum-ravtyd (addr %holrut-rr-key-0)))
  =^  f7  state  (n state (owner-changed:l1 ~radres-tinnyl (addr %losrut-rt-ket-0)))
  =^  f8  state  (n state (changed-spawn-proxy:l1 ~holrut (addr %holrut-skey)))
  =^  f8  state  (n state (changed-spawn-proxy:l1 ~losrut (addr %losrut-skey-0)))
  =^  f8  state  (n state (changed-spawn-proxy:l1 ~holrut deposit-address:naive))
  =^  f9  state  (n state %bat q:(gen-tx 0 dm-spawn %holrut-key-0))
  =^  f10  state  (n state %bat q:(gen-tx 0 pd-spawn %holrut-skey))
  =^  f11  state  (n state (owner-changed:l1 ~losrut deposit-address:naive))
  =^  f12  state  (n state %bat q:(gen-tx 0 pp-spawn %losrut-key-0))
  =^  f13  state  (n state %bat q:(gen-tx 0 hn-spawn %losrut-skey-0))
  =^  f14  state  (n state %bat q:(gen-tx 1 losrut-sproxy %losrut-skey-0))
  =^  f15  state  (n state %bat q:(gen-tx 2 dn-spawn %losrut-skey-1))
  =^  f16  state  (n state %bat q:(gen-tx 0 dm-xfer %holrut-dm-key-0))
  =^  f17  state  (n state %bat q:(gen-tx 0 pd-xfer %holrut-pd-key-0))
  =^  f18  state  (n state %bat q:(gen-tx 0 pp-xfer %losrut-pp-key-0))
  =^  f19  state  (n state %bat q:(gen-tx 0 hn-xfer %losrut-hn-key-0))
  =^  f20  state  (n state %bat q:(gen-tx 0 dn-xfer %losrut-dn-key-0))
  [:(welp f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20) state]
::
::  TODO: add an "evil galaxy" whose points attempt to perform actions
::  on ~rut's points
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
  =^  f1  state  (init-bud state)
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
::  generates all possible transactions and maps them to whether they ought
::  to succeed
::
++  l2-event-gen
  |%
  ::
  +$  rank     ?(%galaxy %star %planet)
  +$  tx-type  $?  %transfer-point
                   %spawn
                   %configure-keys
                   %escape
                   %cancel-escape
                   %adopt
                   %reject
                   %detach
                   %set-management-proxy
                   %set-spawn-proxy
                   %set-transfer-proxy
               ==
  +$  event    [=rank owner=? nonce=? =dominion:naive =proxy:naive =tx-type]
  +$  event-list  (list event)
  +$  success-map  (map event ?)
  +$  event-jar  (jar @p event)
  ::
  ++  make-success-map
    |=  =event-list  ^-  success-map
    =|  =success-map
    |^
    ?~  event-list  success-map
    =/  current-event  i.event-list
    ::
    ?:  ?|  =(owner.current-event %.n)
            =(nonce.current-event %.n)
        ==
        (add-event-check current-event %.n)
    ::
    ::  galaxies and stars can do sponsorship options regardless of
    ::  dominion (though see TODO below on edge cases)
    ::
    ?:  ?&  =(rank.current-event ?(%galaxy %star))
            =(tx-type.current-event ?(%adopt %reject %detach))
            =(proxy.current-event ?(%own %manage))
        ==
        (add-event-check current-event %.y)
    ::
    ::  otherwise, all events from %l1 points should fail
    ::
    ?:  =(dominion.current-event %l1)
    (add-event-check current-event %.n)
    ::
    ::  planets cannot be sponsors
    ?:  ?&  =(rank.current-event %planet)
            =(tx-type.current-event ?(%adopt %reject %detach))
        ==
        (add-event-check current-event %.n)
    ::
    ::  planets cant use %spawn proxy
    ::
    ?:  ?&  =(dominion.current-event %spawn)
            =(rank.current-event %planet)
        ==
        (add-event-check current-event %.n)
    ::
    :: %spawn stars can only %spawn with %own and %spawn proxies
    ?:  ?&  =(dominion.current-event %spawn) :: this implies rank=%star
            !=(proxy.current-event ?(%own %spawn))
        ==
        (add-event-check current-event %.n)
    ::
    =/  final-check  :+  dominion.current-event
                       proxy.current-event
                     tx-type.current-event
    (add-event-check current-event (tx-succeed final-check))
    ::
    ++  add-event-check
      |=  [=event suc=?]
      %=  ^$
          success-map  (~(put by success-map) event suc)
          event-list   +.event-list
      ==
    ::
    ::  galaxies and stars can %adopt %reject %detach regardless of
    ::  dominion. planets cannot do any of these. though L2 sponsorship
    ::  actions should only be possible if the sponsee is on L2, so this
    ::  actually needs to check the content of the tx...
    ::  TODO: gonna leave sponsorship tests aside for now because
    ::  they're more complicated
    ::
    ::  ++  sponsorship-check
    ::    |=  [=rank =proxy:naive =tx-type]  ^-  ?
    ::    %.y
    ::
    ::  checks to see if a given proxy+event combo should work, assuming that
    ::  the pk and nonce are correct
    ::
    ++  tx-succeed
      |=  [=dominion:naive =proxy:naive =tx-type]  ^-  ?
      ?:  =(proxy %own)
        %.y
      ?:  =(proxy %vote)
        %.n
      ::  planet case already excluded
      ?-  tx-type
        ?(%spawn %set-spawn-proxy)
          ?+  proxy    %.n
            %spawn     %.y
            %manage    %.n
            %vote      %.n
          ==
        ?(%transfer-point %set-transfer-proxy)
          ?.  =(dominion %l2)
            %.n
          ?+  proxy    %.n
            %spawn     %.n
            %manage    %.n
            %transfer  %.y
          ==
        $?   %configure-keys  %escape  %cancel-escape  %adopt
             %reject  %detach  %set-management-proxy
        ==
          ?.  =(dominion %l2)
            %.n
          ?+  proxy    %.n
            %spawn     %.n
            %manage    %.y
            %transfer  %.n
          ==
        ==
      ::
      --
  ::
  ++  filter-tx-type
    |=  [typs=(list =tx-type) =event-list]
    |^
    (skim event-list filter)
    ++  filter
      :: I think I can shorten this a bit with a fold or something
      |=  =event  ^-  ?
      =/  match=?  %.n
      |-
      ?~  typs  match
      =/  cur-typ  i.typs
      %=  $
        match  |(match =(cur-typ tx-type.event))
        typs   t.typs
      ==
    --
  ::
  ++  filter-proxy
    |=  [=proxy:naive =event-list]
    |^
    (skim event-list filter)
    ++  filter
      |=  =event
      =(proxy.event proxy)
    --
  ::
  ++  filter-rank
    |=  [=rank =event-list]
    |^
    (skim event-list filter)
    ++  filter
      |=  =event
      =(rank.event rank)
    --
  ::
  ++  filter-owner
    |=  [owner=? =event-list]
    |^
    (skim event-list filter)
    ++  filter
      |=  =event
      =(owner.event owner)
    --
  ::
  ++  filter-nonce
    |=  [nonce=? =event-list]
    |^
    (skim event-list filter)
    ++  filter
      |=  =event
      =(nonce.event nonce)
    --
  ::
  ++  filter-dominion
    |=  [=dominion:naive =event-list]
    |^
    (skim event-list filter)
    ++  filter
      |=  =event
      =(dominion.event dominion)
    --
  ::
  ++  make-event-list  ^-  event-list
    =|  =event-list
    =/  rank-i      1
    |-
    ?:  (gth rank-i 3)
      (remove-wrong-dominion event-list)
    =/  owner-i     0
    |-
    ?.  (lte owner-i 1)
      ^$(rank-i +(rank-i))
    =/  nonce-i     0
    |-
    ?.  (lte nonce-i 1)
      ^$(owner-i +(owner-i))
    =/  dominion-i  1
    |-
    ?.  (lte dominion-i 3)
      ^$(nonce-i +(nonce-i))
    =/  proxy-i     1
    |-
    ?.  (lte proxy-i 5)
      ^$(dominion-i +(dominion-i))
    =/  tx-type-i   1
    |-
    ?.  (lte tx-type-i 11)
      ^$(proxy-i +(proxy-i))
    %=  $
      tx-type-i     +(tx-type-i)
      event-list    :-  :*  (num-to-rank rank-i)
                            (num-to-flag owner-i)
                            (num-to-flag nonce-i)
                            (num-to-dominion dominion-i)
                            (num-to-proxy proxy-i)
                            (num-to-tx-type tx-type-i)
                        ==
                    event-list
    ==
  ::
  ++  remove-wrong-dominion
    |=  in=event-list
    =|  =event-list
    |-
    ?~  in  event-list
    =/  current-event  i.in
    ?:  ?&  =(rank.current-event %galaxy)
            !=(dominion.current-event %l1)
        ==
        $(in t.in)
    ?:  ?&  =(rank.current-event %planet)
            =(dominion.current-event %spawn)
        ==
        $(in t.in)
    %=  $
      in  t.in
      event-list  current-event^event-list
    ==
  ::
  ++  num-to-flag
    |=  val=@ud  ^-  ?
    ?+  val  !!
      %0  %.y
      %1  %.n
    ==
  ::
  ++  num-to-rank
    |=  val=@ud  ^-  rank
    ?+  val  !!
      %1  %galaxy
      %2  %star
      %3  %planet
    ==
  ::
  ++  num-to-dominion
    |=  val=@ud  ^-  dominion:naive
    ?+  val  !!
      %1  %l1
      %2  %l2
      %3  %spawn
    ==
  ::
  ++  num-to-proxy
    |=  val=@ud  ^-  proxy:naive
    ?+  val  !!
      %1  %own
      %2  %spawn
      %3  %manage
      %4  %vote
      %5  %transfer
    ==
  ::
  ++  num-to-tx-type
    |=  val=@ud  ^-  tx-type
    ?+  val  !!
      %1   %transfer-point
      %2   %spawn
      %3   %configure-keys
      %4   %escape
      %5   %cancel-escape
      %6   %adopt
      %7   %reject
      %8   %detach
      %9   %set-management-proxy
      %10  %set-spawn-proxy
      %11  %set-transfer-proxy
    ==
  ::
  ::  jar of events for rut. each @p is mapped to a list of events
  ::  it ought to test, and +success-map says whether or not that
  ::  event should succed or fail
  ::
  ++  gen-rut-proxy-jar
    ^-  (jar @p event)
    =/  filter  ;:  cork
                    (cury filter-owner %.y)
                    (cury filter-proxy %own)
                    (cury filter-nonce %.y)
                    %-  cury
                    :-  filter-tx-type
                    :*  %set-management-proxy
                        %set-spawn-proxy
                        %set-transfer-proxy
                        ~
                    ==
                ==
    =/  filtered-events  (filter make-event-list)
    =|  mgmt-jar=(jar @p event)
    |^
    ?~  filtered-events  mgmt-jar
    =/  current-event  i.filtered-events
    ?:  =(rank.current-event %galaxy)
      (list-in-jar (ly ~[~rut]) current-event)
    ?:  =(rank.current-event %star)
      ?-  dominion.current-event
        %l1     (list-in-jar (ly ~[~rigrut]) current-event)
        %spawn  (list-in-jar (ly ~[~holrut]) current-event)
        %l2     (list-in-jar (ly ~[~losrut]) current-event)
      ==
    ?:  =(rank.current-event %planet)
      ?+  dominion.current-event  !!
        %l1     %-  list-in-jar
                :-  %-  ly
                    :+  ~rabsum-ravtyd
                      ~radres-tinnyl
                    ~
                current-event
        %l2     %-  list-in-jar
                :-  %-  ly
                    :*  ~dovmul-mogryt
                        ~pidted-dacnum
                        ~pinpun-pilsun
                        ~habtyc-nibpyx
                        ~disryt-nolpet
                        ~
                    ==
                current-event
      ==
    $(filtered-events t.filtered-events)
    ::
    ++  list-in-jar
      |=  [ships=(list ship) =event]
      ^+  mgmt-jar
      =/  new-jar  mgmt-jar
      |-
      ?~  ships  %=  ^^$
                   mgmt-jar  new-jar
                   filtered-events  +.filtered-events
                 ==
      =.  new-jar  (~(add ja new-jar) i.ships event)
      $(ships t.ships)
    ::
    --
  ::
  ++  rut-default-args
    |=  [=ship =event]  ^-  skim-tx:naive
    |^  ^-  skim-tx:naive
    ?+  tx-type.event  !!
      :: %spawn
      :: %transfer-point
   ::   %configure-keys        [%configure-keys encr auth 1 |]
      :: %escape
      :: %cancel-escape
      :: %adopt
      :: %reject
      :: %detach
      %set-management-proxy  [%set-management-proxy (addr %proxy-test)]
      %set-spawn-proxy       [%set-spawn-proxy (addr %proxy-test)]
      %set-transfer-proxy    [%set-transfer-proxy (addr %proxy-test)]
    ==
    ::
    ::  ++  which-ship
    ::  ::  should only matter for spawn and sponsorship actions
    ::    ?+  ship  !!
    ::      ~rut
    ::      ~rigrut
    ::    ==
    ::  ::
::    ++  set-proxy  ^-  skim-tx:naive  [tx-type.event (addr %proxy-test)]
    ::  ++  transfer
    ::  ++  configure-keys
    ::  ++  sponsor  ~
    ::
    --
  ::
  --
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
++  marbud-own  [~marbud %own]  ::key %marbud-key-0
++  marbud-spn  [~marbud %spawn]  :: key %marbud-skey
++  marbud-mgt  [~marbud %manage]  :: key %marbud-mkey
++  marbud-xfr  [~marbud %transfer]  :: key %marbud-key-1
::
++  dopbud-own  [~dopbud %own] :: key %dopbud-key-0
::
++  litbud-own  [~litbud %own] :: key %litbud-key-0
::
++  lt-own      [~linnup-torsyx %own] :: key %lt-key-0
++  lt-xfr      [~linnup-torsyx %transfer] :: key %lt-key-0
::
::  rut tests
::
::
++  common-mgmt  %mgmt-key-0
++  common-spwn  %spwn-key-0
++  common-vote  %vote-key-0
++  common-ownr  %ownr-key-0
++  common-tran  %tran-key-0
++  rut-ship-list  %-  ly
                   :*  ~rut
                       ~holrut
                       ~rigrut
                       ~losrut
                       ~rabsum-ravtyd
                       ~disryt-nolpet
                       ~pinpun-pilsun
                       ~dovmul-mogryt
                       ~habtyc-nibpyx
                       ~pidreg-dacnum
                       ~radres-tinnyl
                       ~
                   ==
::
:: initial ownership keys for each point under ~rut
++  default-own-keys  %-  my:nl
                      :*  [~rut %rut-key-0]
                          [~holrut %holrut-key-0]
                          [~rigrut %rigrut-key-0]
                          [~losrut %losrut-key-0]
                          [~rabsum-ravtyd %holrut-rr-key-0]
                          [~disryt-nolpet %losrut-dn-key-0]
                          [~pinpun-pilsun %losrut-pp-key-0]
                          [~dovmul-mogryt %holrut-dm-key-0]
                          [~habtyc-nibpyx %losrut-hn-key-0]
                          [~pidreg-dacnum %holrut-pd-key-0]
                          [~radres-tinnyl %losrut-rt-key-0]
                          ~
                      ==
::
--
::
:: Tests
::
|%
::  new tests
::
++  test-rut-proxies  ^-  tang
  =,  l2-event-gen
  ::
  =/  event-jar  gen-rut-proxy-jar
  =|  =^state:naive
  =^  f  state  (init-rut-full state)
  =/  initial-state  state
  =/  ship-list  rut-ship-list
  =/  suc-map  (make-success-map make-event-list)
  ::
  |-  ^-  tang
  ?~  ship-list  ~
  %+  weld  $(ship-list t.ship-list)
  =/  cur-ship  i.ship-list
  ::
  %+  category  (scow %p cur-ship)
  =/  current-events  (~(get ja event-jar) cur-ship)
  |-  ^-  tang
  ?~  current-events  ~
  %+  weld  $(current-events t.current-events)
  =/  cur-event  i.current-events
  %+  category  (weld "dominion " (scow %tas dominion.cur-event))
  %+  category  (weld "proxy " (scow %tas proxy.cur-event))
  %+  category  (weld "tx-type " (scow %tas tx-type.cur-event))
  %+  category  (weld "owner? " (scow %f owner.cur-event))
  %+  category  (weld "correct nonce? " (scow %f nonce.cur-event))
  =/  state  initial-state
  %+  expect-eq
    !>  (~(got by suc-map) cur-event)
  ::
    !>
    |^
    =^    f
        state
      %-  n
      :+  initial-state  ::state?
        %bat
      =<  q
      %-  gen-tx
      :+  nonce.owner.own:(~(got by points.state) cur-ship)
        :-  [cur-ship proxy.cur-event]
        (rut-default-args cur-ship cur-event)
      (~(got by default-own-keys) cur-ship)
    ?+  tx-type.cur-event  !!
      %set-management-proxy  check-mgmt-proxy
      %set-spawn-proxy       check-spwn-proxy
      %set-transfer-proxy    check-xfer-proxy
    ==
    ::
    ++  check-mgmt-proxy
      .=  =<  address.management-proxy.own
          (~(got by points.state) cur-ship)
      (addr %proxy-test)
    ::
    ++  check-spwn-proxy
      .=  =<  address.spawn-proxy.own
          (~(got by points.state) cur-ship)
      (addr %proxy-test)
    ::
    ++  check-xfer-proxy
      .=  =<  address.transfer-proxy.own
          (~(got by points.state) cur-ship)
      (addr %proxy-test)
    ::
    --
::
++  test-marbud-l2-change-keys-new  ^-  tang
  =/  new-keys       [%configure-keys encr auth suit |]
  =|  =^state:naive
  =^  f  state  (init-marbud state)
  =/  marbud-point  (~(got by points.state) ~marbud)
  =/  new-marbud  marbud-point(keys.net [1 suit auth encr], nonce.owner.own 1)
  ::
  %+  expect-eq
    !>  state(points (~(put by points.state) ~marbud new-marbud))
  ::
    !>
    =^  f  state  (n state %bat q:(gen-tx 0 [marbud-own new-keys] %marbud-key-0))
    state
  ::
::  old tests
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
  =/  marbud-transfer    [marbud-own %transfer-point (addr %marbud-key-0) |]
  =/  marbud-transfer-2  [marbud-own %transfer-point (addr %marbud-key-1) |]
  ::
  %+  expect-eq
    !>  [(addr %marbud-key-1) 2]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 marbud-transfer %marbud-key-0))
    =^  f  state  (n state %bat q:(gen-tx 1 marbud-transfer-2 %marbud-key-0))
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
  =/  marbud-sproxy  [marbud-own %set-spawn-proxy (addr %marbud-skey)]
  ::
  %+  expect-eq
    !>  [(addr %marbud-skey) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 marbud-sproxy %marbud-key-0))
    spawn-proxy.own:(~(got by points.state) ~marbud)
::
++  test-l2-set-transfer-proxy  ^-  tang
  =/  marbud-tproxy  [marbud-own %set-transfer-proxy (addr %marbud-tkey)]
  ::
  %+  expect-eq
    !>  [(addr %marbud-tkey) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 marbud-tproxy %marbud-key-0))
    transfer-proxy.own:(~(got by points.state) ~marbud)
::
++  test-l2-set-management-proxy  ^-  tang
  =/  marbud-mproxy  [marbud-own %set-management-proxy (addr %marbud-mkey)]
  ::
  %+  expect-eq
    !>  [(addr %marbud-mkey) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 marbud-mproxy %marbud-key-0))
    management-proxy.own:(~(got by points.state) ~marbud)
::
++  test-l2-dopbud-spawn-proxy-deposit  ^-  tang
  %+  expect-eq
    !>  %spawn
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-dopbud state)
    dominion:(~(got by points.state) ~dopbud)
::
++  test-l2-sambud-spawn-proxy-predeposit  ^-  tang
  %+  expect-eq
    !>  [(addr %sambud-skey) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-sambud state)
    =^  f  state  (n state (changed-spawn-proxy:l1 ~sambud (addr %sambud-skey)))
    =^  f  state  (n state (changed-spawn-proxy:l1 ~sambud deposit-address:naive))
    spawn-proxy.own:(~(got by points.state) ~sambud)
::
++  test-l2-sambud-own-spawn-proxy-postdeposit  ^-  tang
  =/  sambud-sproxy  [[~sambud %own] %set-spawn-proxy (addr %sambud-skey-0)]
  %+  expect-eq
    !>  [(addr %sambud-skey-0) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-sambud state)
    =^  f  state  (n state (changed-spawn-proxy:l1 ~sambud deposit-address:naive))
    =^  f  state  (n state %bat q:(gen-tx 0 sambud-sproxy %sambud-key-0))
    spawn-proxy.own:(~(got by points.state) ~sambud)
::
++  test-l2-sambud-spawn-spawn-proxy-postdeposit  ^-  tang
  =/  sambud-sproxy  [[~sambud %spawn] %set-spawn-proxy (addr %sambud-skey-1)]
  %+  expect-eq
    !>  [(addr %sambud-skey-1) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-sambud state)
    =^  f  state  (n state (changed-spawn-proxy:l1 ~sambud (addr %sambud-skey-0)))
    =^  f  state  (n state (changed-spawn-proxy:l1 ~sambud deposit-address:naive))
    =^  f  state  (n state %bat q:(gen-tx 0 sambud-sproxy %sambud-skey-0))
    spawn-proxy.own:(~(got by points.state) ~sambud)
::
++  test-l2-sambud-spawn-proxy-predeposit-spawn  ^-  tang
  =/  lf-spawn  [[~sambud %spawn] %spawn ~lisdur-fodrys (addr %lf-key-0)]
  %+  expect-eq
    !>  [`@ux`(addr %lf-key-0) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-sambud state)
    =^  f  state  (n state (changed-spawn-proxy:l1 ~sambud (addr %sambud-skey)))
    =^  f  state  (n state (changed-spawn-proxy:l1 ~sambud deposit-address:naive))
    =^  f  state  (n state %bat q:(gen-tx 0 lf-spawn %sambud-skey))
    transfer-proxy.own:(~(got by points.state) ~lisdur-fodrys)
::
++  test-marbud-l2-spawn  ^-  tang
  =/  marbud-sproxy  [marbud-own %set-spawn-proxy (addr %marbud-skey)]
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
      =^  f  state  (n state %bat q:(gen-tx 0 [marbud-own lt-spawn] %marbud-key-0))
      transfer-proxy.own:(~(got by points.state) ~linnup-torsyx)
    ::
    %+  expect-eq
    ::  Tests l2 spawning with spawn proxy
      !>  [`@ux`(addr %lt-key-0) 0]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-sproxy %marbud-key-0))
      =^  f  state  (n state %bat q:(gen-tx 0 [marbud-spn lt-spawn] %marbud-skey))
      transfer-proxy.own:(~(got by points.state) ~linnup-torsyx)
  ==
::
++  test-marbud-l2-double-spawn  ^-  tang
  ::  Attempts to spawn the same planet twice, once with ownership and once with spawn proxy
  =/  marbud-sproxy   [marbud-own %set-spawn-proxy (addr %marbud-skey)]
  =/  lt-spawn-0      [marbud-own %spawn ~linnup-torsyx (addr %lt-key-0)]
  =/  lt-spawn-1      [marbud-spn %spawn ~linnup-torsyx (addr %lt-key-1)]
  ::
  %-  expect-fail
    |.
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 marbud-sproxy %marbud-key-0))
    =^  f  state  (n state %bat q:(gen-tx 1 lt-spawn-0 %marbud-key-0))
    =^  f  state  (n state %bat q:(gen-tx 0 lt-spawn-1 %marbud-skey))
    state
::
::
++  test-marbud-l2-change-keys  ^-  tang
  =/  new-keys       [%configure-keys encr auth suit |]
  =/  marbud-mproxy  [marbud-own %set-management-proxy (addr %marbud-mkey)]
  ::
  ;:  weld
    %+  expect-eq
      !>  [suit auth encr]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 [marbud-own new-keys] %marbud-key-0))
      |1:keys.net:(~(got by points.state) ~marbud)
    ::
    %+  expect-eq
      !>  [suit auth encr]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-mproxy %marbud-key-0))
      =^  f  state  (n state %bat q:(gen-tx 0 [marbud-mgt new-keys] %marbud-mkey))
      |1:keys.net:(~(got by points.state) ~marbud)
    ::
    :: TODO: make sure nobody else can change these keys
  ==
::
:: TODO: transfer breach via transfer proxy
++  test-marbud-l2-proxies-transfer  ^-  tang
  =/  marbud-new-keys            [marbud-own %configure-keys encr auth suit |]
  =/  marbud-sproxy              [marbud-own %set-spawn-proxy (addr %marbud-skey)]
  =/  marbud-mproxy              [marbud-own %set-management-proxy (addr %marbud-mkey)]
  =/  marbud-tproxy              [marbud-own %set-transfer-proxy (addr %marbud-key-1)]
  =/  marbud-transfer-breach     [marbud-own %transfer-point (addr %marbud-key-1) &]
  =/  marbud-transfer-no-breach  [marbud-own %transfer-point (addr %marbud-key-1) |]
  =/  marbud-xfr-breach          [marbud-xfr %transfer-point (addr %marbud-key-1) &]
  =/  marbud-xfr-no-breach       [marbud-xfr %transfer-point (addr %marbud-key-1) |]
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
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-sproxy %marbud-key-0))
      =^  f  state  (n state %bat q:(gen-tx 1 marbud-mproxy %marbud-key-0))
      =^  f  state  (n state %bat q:(gen-tx 2 marbud-tproxy %marbud-key-0))
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-xfr-breach %marbud-key-1))
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
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-new-keys %marbud-key-0))
      =^  f  state  (n state %bat q:(gen-tx 1 marbud-transfer-breach %marbud-key-0))
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
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-sproxy %marbud-key-0))
      =^  f  state  (n state %bat q:(gen-tx 1 marbud-mproxy %marbud-key-0))
      =^  f  state  (n state %bat q:(gen-tx 2 marbud-tproxy %marbud-key-0))
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-xfr-no-breach %marbud-key-1))
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
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-new-keys %marbud-key-0))
      =^  f  state  (n state %bat q:(gen-tx 1 marbud-transfer-no-breach %marbud-key-0))
      |1:keys.net:(~(got by points.state) ~marbud)
  ==
::
:: TODO: life+rift changes via transfer proxy
::
++  test-marbud-life-rift  ^-  tang
  =/  new-keys-no-reset           [marbud-own %configure-keys encr auth suit |]
  =/  new-keys-yes-reset          [marbud-own %configure-keys encr auth suit &]
  =/  zero-keys-no-reset          [marbud-own %configure-keys 0 0 0 |]
  =/  zero-keys-yes-reset         [marbud-own %configure-keys 0 0 0 &]
  =/  marbud-transfer-no-breach   [marbud-own %transfer-point (addr %marbud-key-1) |]
  =/  marbud-transfer-yes-breach  [marbud-own %transfer-point (addr %marbud-key-1) &]
  =/  marbud-own-1                [~marbud %marbud-key-1 %own]
  ::
  ;:  weld
    %+  expect-eq
    ::  breach=%.n
      !>  [0 1]                   :: [rift life]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 new-keys-no-reset %marbud-key-0))
      =^  f  state  (n state %bat q:(gen-tx 1 new-keys-no-reset %marbud-key-0))
      [rift.net life.keys.net]:(~(got by points.state) ~marbud)
    ::
    %+  expect-eq
    ::  breach=%.y
      !>  [1 1]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 new-keys-yes-reset %marbud-key-0))
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
      =^  f  state  (n state %bat q:(gen-tx 0 new-keys-yes-reset %marbud-key-0)) :: inc life and rift
      =^  f  state  (n state %bat q:(gen-tx 1 zero-keys-no-reset %marbud-key-0)) :: inc life
      =^  f  state  (n state %bat q:(gen-tx 2 zero-keys-yes-reset %marbud-key-0)) :: inc rift
      [rift.net life.keys.net]:(~(got by points.state) ~marbud)
    ::
    %+  expect-eq
    ::  Keep the same keys while breaching via %configure-keys
    ::
      !>  [2 1]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 new-keys-yes-reset %marbud-key-0)) :: inc life and rift
      =^  f  state  (n state %bat q:(gen-tx 1 new-keys-yes-reset %marbud-key-0)) :: inc life and rift
      [rift.net life.keys.net]:(~(got by points.state) ~marbud)
    ::
    %+  expect-eq
    ::
      !>  [1 2]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 new-keys-no-reset %marbud-key-0))
      =^  f  state  (n state %bat q:(gen-tx 1 marbud-transfer-no-breach %marbud-key-0))
      ::  TODO: shouldn't the nonce by zero for the next tx?
      =^  f  state  (n state %bat q:(gen-tx 2 zero-keys-yes-reset %marbud-key-1))
      [rift.net life.keys.net]:(~(got by points.state) ~marbud)
    ::
    %+  expect-eq
    ::  set networking keys, then transfer and set networking keys with breach
    ::
      !>  [1 3]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 new-keys-no-reset %marbud-key-0)) :: inc life
      =^  f  state  (n state %bat q:(gen-tx 1 marbud-transfer-yes-breach %marbud-key-0)) :: inc life and rift
      ::  TODO: shouldn't the nonce by zero for the next tx?
      =^  f  state  (n state %bat q:(gen-tx 2 new-keys-no-reset %marbud-key-1)) ::inc life
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
      =^  f  state  (n state %bat q:(gen-tx 0 new-keys-yes-reset %marbud-key-0)) :: inc life and rift
      =^  f  state  (n state %bat q:(gen-tx 1 zero-keys-no-reset %marbud-key-0)) :: inc life
      =^  f  state  (n state %bat q:(gen-tx 2 marbud-transfer-yes-breach %marbud-key-0)) :: inc rift
      [rift.net life.keys.net]:(~(got by points.state) ~marbud)
    ::
 ==
::
++  test-dopbud-l2-spawn  ^-  tang
  =/  pp-spawn        [dopbud-own %spawn ~palsep-picdun (addr %pp-key-0)]
  ::
  %+  expect-eq
    !>  [`@ux`(addr %pp-key-0) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-dopbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 pp-spawn %dopbud-key-0))
    transfer-proxy.own:(~(got by points.state) ~palsep-picdun)
::
++  test-dopbud-l2-spawn-after-transfer  ^-  tang
  =/  pp-spawn        [dopbud-own %spawn ~palsep-picdun (addr %pp-key-0)]
  =/  lr-spawn        [dopbud-own %spawn ~laclur-rachul (addr %lr-key-0)]
  ::
  %+  expect-eq
    !>  [`@ux`(addr %lr-key-0) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-dopbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 pp-spawn %dopbud-key-0))
    =^  f  state  (n state (owner-changed:l1 ~dopbud (addr %dopbud-key-1)))
    =^  f  state  (n state %bat q:(gen-tx 1 lr-spawn %dopbud-key-1))
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
  =/  lt-spawn                [marbud-own %spawn ~linnup-torsyx (addr %lt-key-0)]
  =/  lt-transfer-yes-breach  [%transfer-point (addr %lt-key-0) &]
  ::
  %+  expect-eq
    !>  [`@ux`(addr %lt-key-0) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 lt-spawn %marbud-key-0))
    =^  f  state  (n state %bat q:(gen-tx 0 [lt-xfr lt-transfer-yes-breach] %lt-key-0))
    owner.own:(~(got by points.state) ~linnup-torsyx)
::
++  test-palsep-picdun-l2-transfer-ownership  ^-  tang
  =/  pp-xfr                  [~palsep-picdun %transfer]
  =/  pp-spawn                [dopbud-own %spawn ~palsep-picdun (addr %pp-key-0)]
  =/  pp-transfer-yes-breach  [pp-xfr %transfer-point (addr %pp-key-0) &]
  %+  expect-eq
    !>  [`@ux`(addr %pp-key-0) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-dopbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 pp-spawn %dopbud-key-0))
    =^  f  state  (n state %bat q:(gen-tx 0 pp-transfer-yes-breach %pp-key-0))
    owner.own:(~(got by points.state) ~palsep-picdun)
::
++  test-linnup-torsyx-l2-escape-request  ^-  tang
  =/  lt-spawn                   [marbud-own %spawn ~linnup-torsyx (addr %lt-key-0)]
  =/  lt-transfer-yes-breach     [lt-xfr %transfer-point (addr %lt-key-0) &]
  ::
  %+  expect-eq
    !>  [~ ~litbud]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (init-litbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 lt-spawn %marbud-key-0))
    =^  f  state  (n state %bat q:(gen-tx 0 lt-transfer-yes-breach %lt-key-0))
    =^  f  state  (n state %bat q:(gen-tx 0 [lt-own [%escape ~litbud]] %lt-key-0))
    escape.net:(~(got by points.state) ~linnup-torsyx)
::
++  test-linnup-torsyx-l2-cancel-escape-request  ^-  tang
  =/  lt-spawn                   [marbud-own %spawn ~linnup-torsyx (addr %lt-key-0)]
  =/  lt-transfer-yes-breach     [lt-xfr %transfer-point (addr %lt-key-0) &]
  ::
  %+  expect-eq
    !>  ~
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (init-litbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 lt-spawn %marbud-key-0))
    =^  f  state  (n state %bat q:(gen-tx 0 lt-transfer-yes-breach %lt-key-0))
    =^  f  state  (n state %bat q:(gen-tx 0 [lt-own [%escape ~litbud]] %lt-key-0))
    =^  f  state  (n state %bat q:(gen-tx 1 [lt-own [%cancel-escape ~litbud]] %lt-key-0))
    escape.net:(~(got by points.state) ~linnup-torsyx)
::
++  test-linnup-torsyx-l2-adopt-accept  ^-  tang
  =/  lt-spawn                  [marbud-own %spawn ~linnup-torsyx (addr %lt-key-0)]
  =/  lt-transfer-yes-breach    [lt-xfr %transfer-point (addr %lt-key-0) &]
  ::
  %+  expect-eq
    !>  [~ %.y ~litbud]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (init-litbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 lt-spawn %marbud-key-0))
    =^  f  state  (n state %bat q:(gen-tx 0 lt-transfer-yes-breach %lt-key-0))
    =^  f  state  (n state %bat q:(gen-tx 0 [lt-own [%escape ~litbud]] %lt-key-0))
    =^  f  state  (n state %bat q:(gen-tx 0 [litbud-own [%adopt ~linnup-torsyx]] %litbud-key-0))
    [escape.net sponsor.net]:(~(got by points.state) ~linnup-torsyx)
::
++  test-linnup-torsyx-l2-adopt-reject  ^-  tang
  ::  TODO: at the moment the default sponsor is always ~zod, but it should probably
  ::  be ~marbud here
  =/  lt-spawn                  [marbud-own %spawn ~linnup-torsyx (addr %lt-key-0)]
  =/  lt-transfer-yes-breach    [lt-xfr %transfer-point (addr %lt-key-0) &]
  ::
  %+  expect-eq
    !>  ~
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (init-litbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 lt-spawn %marbud-key-0))
    =^  f  state  (n state %bat q:(gen-tx 0 lt-transfer-yes-breach %lt-key-0))
    =^  f  state  (n state %bat q:(gen-tx 0 [lt-own [%escape ~litbud]] %lt-key-0))
    =^  f  state  (n state %bat q:(gen-tx 0 [litbud-own [%reject ~linnup-torsyx]] %litbud-key-0))
    escape.net:(~(got by points.state) ~linnup-torsyx)
::
++  test-linnup-torsyx-l2-detach  ^-  tang
  =/  lt-spawn                  [marbud-own %spawn ~linnup-torsyx (addr %lt-key-0)]
  =/  lt-transfer-yes-breach    [lt-xfr %transfer-point (addr %lt-key-0) &]
  ::
  %+  expect-eq
    !>  [~ %.n ~marbud]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (init-litbud state)
    =^  f  state  (n state %bat q:(gen-tx 0 lt-spawn %marbud-key-0))
    =^  f  state  (n state %bat q:(gen-tx 0 lt-transfer-yes-breach %lt-key-0))
    =^  f  state  (n state %bat q:(gen-tx 1 [marbud-own [%detach ~linnup-torsyx]] %marbud-key-0))
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
