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
::  ~red is for testing escapes.
::  ~rigred is L1 star
::  ~losred is L2 star
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
  =/  losrut-mproxy  [[~losrut %own] %set-management-proxy (addr %losrut-mkey-0)]
  =/  dm-mkey  [[~dovmul-mogryt %own] %set-management-proxy (addr %holrut-dm-mkey-0)]
  =/  pd-mkey  [[~pidted-dacnum %own] %set-management-proxy (addr %holrut-pd-mkey-0)]
  =/  pp-mkey  [[~pinpun-pilsun %own] %set-management-proxy (addr %losrut-pp-mkey-0)]
  =/  hn-mkey  [[~habtyc-nibpyx %own] %set-management-proxy (addr %losrut-hn-mkey-0)]
  =/  dn-mkey  [[~disryt-nolpet %own] %set-management-proxy (addr %losrut-dn-mkey-0)]
  =^  f1  state  (n state (owner-changed:l1 ~rut (addr %rut-key-0)))
  =^  f2  state  (n state (owner-changed:l1 ~rigrut (addr %rigrut-key-0)))
  =^  f3  state  (n state (owner-changed:l1 ~holrut (addr %holrut-key-0)))
  =^  f4  state  (n state (owner-changed:l1 ~losrut (addr %losrut-key-0)))
  =^  f5  state  (n state (owner-changed:l1 ~larsyx-mapmeg (addr %rigrut-lm-key-0)))
  =^  f6  state  (n state (owner-changed:l1 ~rabsum-ravtyd (addr %holrut-rr-key-0)))
  =^  f7  state  (n state (owner-changed:l1 ~radres-tinnyl (addr %losrut-rt-key-0)))
  =^  f8  state  (n state (changed-spawn-proxy:l1 ~holrut (addr %holrut-skey-0)))
  =^  f8  state  (n state (changed-spawn-proxy:l1 ~losrut (addr %losrut-skey-0)))
  =^  f8  state  (n state (changed-spawn-proxy:l1 ~holrut deposit-address:naive))
  =^  f9  state  (n state %bat q:(gen-tx 0 dm-spawn %holrut-key-0))
  =^  f10  state  (n state %bat q:(gen-tx 0 pd-spawn %holrut-skey-0))
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
  :: the following sets proxies for testing with various proxies
  =^  p1   state  (n state (changed-management-proxy:l1 ~rut (addr %rut-mkey-0)))
  =^  p2   state  (n state (changed-management-proxy:l1 ~rigrut (addr %rigrut-mkey-0)))
  =^  p3   state  (n state (changed-management-proxy:l1 ~larsyx-mapmeg (addr %rigrut-lm-mkey-0)))
  =^  p4   state  (n state (changed-management-proxy:l1 ~holrut (addr %holrut-mkey-0)))
  =^  p5   state  (n state (changed-management-proxy:l1 ~rabsum-ravtyd (addr %holrut-rr-mkey-0)))
  =^  p6   state  (n state (changed-management-proxy:l1 ~radres-tinnyl (addr %losrut-rt-mkey-0)))
  =^  p7   state  (n state %bat q:(gen-tx 0 dm-mkey %holrut-dm-key-0))
  =^  p8   state  (n state %bat q:(gen-tx 0 pd-mkey %holrut-pd-key-0))
  =^  p9   state  (n state %bat q:(gen-tx 0 pp-mkey %losrut-pp-key-0))
  =^  p10  state  (n state %bat q:(gen-tx 0 hn-mkey %losrut-hn-key-0))
  =^  p11  state  (n state %bat q:(gen-tx 0 dn-mkey %losrut-dn-key-0))
  =^  p12  state  (n state %bat q:(gen-tx 1 losrut-mproxy %losrut-key-0))
  :: end of ~rut points, beginning of ~red. TODO this should be removed
  :: once i move %escape to +test-red. or maybe %escape should stay here
  :: because its the simplest?
  =^  g1  state  (n state (owner-changed:l1 ~red (addr %red-key-0)))
  =^  g2  state  (n state (owner-changed:l1 ~rigred (addr %rigred-key-0)))
  =^  g3  state  (n state (owner-changed:l1 ~losred (addr %losred-key-0)))
  =^  g4  state  (n state (changed-management-proxy:l1 ~rigred (addr %rigred-mkey-0)))
  =^  g5  state  (n state (changed-management-proxy:l1 ~losred (addr %losred-mkey-0)))
  =^  g6  state  (n state (owner-changed:l1 ~losred deposit-address:naive))
  :-  ;:  welp
      f1  f2  f3  f4  f5  f6  f7  f8  f9  f10
      f11  f12  f13  f14  f15  f16  f17  f18
      f19  f20
      p1  p2  p3  p4  p5  p6  p7  p8  p9  p10
      p11  p12
      g1  g2  g3  g4  g5  g6
      ==
  state
::
:: +init-red-full adds another galaxy to the ~rut universe, ~red, and additional
:: points helpful for testing sponsorship actions. this has been separated from
:: ~rut because the concerns are different enough from the other actions that
:: its cleaner to do them separately
::
++  init-red-full
  |=  =^state:naive
  ^-  [effects:naive ^state:naive]
  =/  pp-escape  [[~pinpun-pilsun %own] %escape ~losred]
  =/  dm-escape  [[~dovmul-mogryt %own] %escape ~rigred]
  =/  lm-escape  [[~larsyx-mapmeg %own] %escape ~losred]
  =/  rm-escape  [[~rabsum-ravtyd %own] %escape ~rigred]
  =^  f1  state  (init-rut-full state)
  ::  TODO uncomment the below once %escape is moved to +test-red
  ::  =^  f21  state  (n state (owner-changed:l1 ~red (addr %red-key-0)))
  ::  =^  f22  state  (n state (owner-changed:l1 ~rigred (addr %rigred-key-0)))
  ::  =^  f23  state  (n state (owner-changed:l1 ~losred (addr %losred-key-0)))
  ::  =^  f24  state  (n state (owner-changed:l1 ~losred deposit-address:naive))
  ::  each pending escape will be followed by an adopt, reject, or cancel-escape
  ::  L1->L1
  =^  f2  state  (n state %bat q:(gen-tx 0 rm-escape %holrut-rr-key-0))
  ::  L2->L2
  =^  f3  state  (n state %bat q:(gen-tx 1 pp-escape %losrut-pp-key-0))
  ::  L2->L1
  =^  f4  state  (n state %bat q:(gen-tx 1 dm-escape %holrut-dm-key-0))
  ::  L1->L2
  =^  f5  state  (n state %bat q:(gen-tx 0 lm-escape %rigrut-lm-key-0))
  [:(welp f1 f2 f3 f4 f5) state]
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
    :: +make-success-map maps each event to whether or not that combination of factors
    :: ought to succeed or fail, for testing purposes. this is not a complete description atm
    :: for instance, it does not take into account whether you are trying to spawn a planet
    :: available to you or move to a sponsor of the correct rank.
    ::
    :: it is also done in a more verbose style than strictly necessary to make it easier
    :: to read through and determine why a particular event is labeled with %.y or %.n
    :: and to make it easier to do future modifications
    ::
    |=  =event-list  ^-  success-map
    =|  =success-map
    |^
    ?~  event-list  success-map
    =/  cur-event  i.event-list
    ::  check owner or nonce first
    ?:  ?|  =(owner.cur-event %.n)
            =(nonce.cur-event %.n)
        ==
        (add-event-check cur-event %.n)
    ::  check dominion next
    ?-  dominion.cur-event
      %l1     (add-event-check cur-event (l1-check cur-event))
      %spawn  (add-event-check cur-event (spawnd-check cur-event))
      %l2     (add-event-check cur-event (l2-check cur-event))
    ==
    ::
    ++  add-event-check
      |=  [=event suc=?]
      %=  ^$
          success-map  (~(put by success-map) event suc)
          event-list   +.event-list
      ==
    ::
    ++  l1-check
      |^
      |=  cur-event=event  ^-  ?
      ?-  proxy.cur-event
        %own       (manage-own-check cur-event)
        %spawn     %.n
        %manage    (manage-own-check cur-event)
        %vote      %.n
        %transfer  %.n
      ==
      ::
      ++  manage-own-check
        |^
        |=  cur-event=event  ^-  ?
        ?-  rank.cur-event
          %galaxy  (galaxy-check cur-event)
          %star    (star-check cur-event)
          %planet  (planet-check cur-event)
        ==
        ++  galaxy-check
          |=  cur-event=event  ^-  ?
          ?+  tx-type.cur-event  %.n
            %adopt   %.y
            %reject  %.y
            %detach  %.y
          ==
        ++  star-check
          |=  cur-event=event  ^-  ?
          ?+  tx-type.cur-event  %.n
            %adopt          %.y
            %reject         %.y
            %detach         %.y
            %escape         %.y
            %cancel-escape  %.y
          ==
        ++  planet-check
          |=  cur-event=event  ^-  ?
          ?+  tx-type.cur-event  %.n
            %escape         %.y
            %cancel-escape  %.y
          ==
        ::
        --  :: +manage-own-check
      ::
      --  ::  +l1-check
    ::
    ++  spawnd-check
      |^
      |=  cur-event=event  ^-  ?
      ?-  rank.cur-event
        %galaxy  %.n
        %star    (star-check cur-event)
        %planet  %.n
      ==
      ++  star-check
        |^
        |=  cur-event=event  ^-  ?
        ?-  proxy.cur-event
          %own       (ownp-check cur-event)
          %manage    (managep-check cur-event)
          %spawn     (spawnp-check cur-event)
          %vote      %.n
          %transfer  %.n
        ==
        ++  ownp-check
          |=  cur-event=event  ^-  ?
          ?+  tx-type.cur-event  %.n
            %spawn            %.y
            %adopt            %.y
            %reject           %.y
            %detach           %.y
            %escape           %.y
            %cancel-escape    %.y
            %set-spawn-proxy  %.y
          ==
        ++  managep-check
          |=  cur-event=event  ^-  ?
          ?+  tx-type.cur-event  %.n
            %adopt          %.y
            %reject         %.y
            %detach         %.y
            %escape         %.y
            %cancel-escape  %.y
          ==
        ++  spawnp-check
          |=  cur-event=event  ^-  ?
          ?+  tx-type.cur-event  %.n
            %spawn            %.y
            %set-spawn-proxy  %.y
          ==
        --  ::  +star-check
      ::
      --  :: +spawnd-check
    ::
    ++  l2-check
      |^
      |=  cur-event=event  ^-  ?
      ?-  rank.cur-event
        %galaxy  %.n
        %star    (star-check cur-event)
        %planet  (planet-check cur-event)
      ==
      ++  star-check
        |^
        |=  cur-event=event  ^-  ?
        ?-  proxy.cur-event
          %own       %.y
          %manage    (managep-check cur-event)
          %spawn     (spawnp-check cur-event)
          %vote      %.n
          %transfer  (transferp-check cur-event)
        ==
        ++  managep-check
          |=  cur-event=event  ^-  ?
          ?-  tx-type.cur-event
            %configure-keys        %.y
            %escape                %.y
            %cancel-escape         %.y
            %adopt                 %.y
            %reject                %.y
            %detach                %.y
            %set-management-proxy  %.y
            %set-spawn-proxy       %.n
            %set-transfer-proxy    %.n
            %transfer-point        %.n
            %spawn                 %.n
          ==
        ++  spawnp-check
          |=  cur-event=event  ^-  ?
          ?+  tx-type.cur-event  %.n
            %spawn            %.y
            %set-spawn-proxy  %.y
          ==
        ++  transferp-check
          |=  cur-event=event  ^-  ?
          ?+  tx-type.cur-event  %.n
            %transfer-point      %.y
            %set-transfer-proxy  %.n
          ==
        --  :: +star-check
      ++  planet-check
        |^
        |=  cur-event=event  ^-  ?
        ?-  proxy.cur-event
          %own       (ownp-check cur-event)
          %manage    (managep-check cur-event)
          %spawn     %.n
          %vote      %.n
          %transfer  (transferp-check cur-event)
        ==
        ++  ownp-check
          |=  cur-event=event  ^-  ?
          ?-  tx-type.cur-event
            %transfer-point        %.y
            %spawn                 %.n
            %configure-keys        %.y
            %escape                %.y
            %cancel-escape         %.y
            %adopt                 %.n
            %reject                %.n
            %detach                %.n
            %set-management-proxy  %.y
            %set-spawn-proxy       %.n
            %set-transfer-proxy    %.y
          ==
        ++  managep-check
          |=  cur-event=event  ^-  ?
          ?+  tx-type.cur-event    %.n
            %configure-keys        %.y
            %escape                %.y
            %cancel-escape         %.y
            %set-management-proxy  %.y
          ==
        ++  transferp-check
          |=  cur-event=event  ^-  ?
          ?+  tx-type.cur-event  %.y
            %transfer-point      %.y
            %set-transfer-proxy  %.y
          ==
        ::
        --  ::  +planet-check
      ::
      --  ::  +l2-check
    ::
    --  ::  make-success-map
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
  ++  gen-rut-jar
    ^-  (jar @p event)
    =/  filter  ;:  cork
                    (cury filter-owner %.n)
                    (cury filter-proxy %own)
                    (cury filter-nonce %.y)
                    (cury filter-rank %planet)
                    (cury filter-dominion %l1)
                    %-  cury
                    :-  filter-tx-type
                    :*  %spawn
                        %transfer-point
                        %configure-keys
                        %set-management-proxy
                        %set-spawn-proxy  :: planets can set spawn proxy atm
                        %set-transfer-proxy
                        %escape
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
                    :^    ~larsyx-mapmeg
                        ~rabsum-ravtyd
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
                       ~larsyx-mapmeg
                       ~rabsum-ravtyd
                       ~disryt-nolpet
                       ~pinpun-pilsun
                       ~dovmul-mogryt
                       ~habtyc-nibpyx
                       ~pidted-dacnum
                       ~radres-tinnyl
                       ~
                   ==
::
:: initial keys for each point under ~rut
++  default-own-keys  %-  my:nl
                      :*  [~rut %rut-key-0]
                          [~holrut %holrut-key-0]
                          [~rigrut %rigrut-key-0]
                          [~losrut %losrut-key-0]
                          [~larsyx-mapmeg %rigrut-lm-key-0]
                          [~rabsum-ravtyd %holrut-rr-key-0]
                          [~disryt-nolpet %losrut-dn-key-0]
                          [~pinpun-pilsun %losrut-pp-key-0]
                          [~dovmul-mogryt %holrut-dm-key-0]
                          [~habtyc-nibpyx %losrut-hn-key-0]
                          [~pidted-dacnum %holrut-pd-key-0]
                          [~radres-tinnyl %losrut-rt-key-0]
                          ~
                      ==
::
++  default-manage-keys  %-  my:nl
                         :*  [~rut %rut-mkey-0]
                             [~holrut %holrut-mkey-0]
                             [~rigrut %rigrut-mkey-0]
                             [~losrut %losrut-mkey-0]
                             [~larsyx-mapmeg %rigrut-lm-mkey-0]
                             [~rabsum-ravtyd %holrut-rr-mkey-0]
                             [~disryt-nolpet %losrut-dn-mkey-0]
                             [~pinpun-pilsun %losrut-pp-mkey-0]
                             [~dovmul-mogryt %holrut-dm-mkey-0]
                             [~habtyc-nibpyx %losrut-hn-mkey-0]
                             [~pidted-dacnum %holrut-pd-mkey-0]
                             [~radres-tinnyl %losrut-rt-mkey-0]
                             ~
                         ==
++  default-spawn-keys  %-  my:nl
                        :*  [~holrut %holrut-skey-0]
                            [~losrut %losrut-skey-0]
                            ~
                        ==
::
::  sponsorship tests
++  losrut-own   [~losrut %own]
++  losrut-mgmt  [~losrut %manage]
++  holrut-own   [~holrut %own]
++  holrut-mgmt  [~holrut %manage]
++  rigrut-own   [~rigrut %own]
++  rigrut-mgmt  [~rigrut %manage]
++  losred-own   [~losred %own]
++  losred-mgmt  [~losred %manage]
++  rigred-own   [~rigred %own]
++  rigred-mgmt  [~rigred %manage]
::
--
::
:: Tests
::
|%
::  new tests
::
::  this test spawns a "full galaxy" containing all varieties of points. it then
::  saves this initial state, and runs single transaction batches for all possible
::  L2 "event types". it compares the entire new state to the entire initial state and checks for
::  the expected state change. it then resets the state to the initial state and
::  tries the next event in on the list.
::
::  more specifically, there is a $jar called event-jar that maps ships to lists of
::  events it should try. it then picks off a ship, tries all the events in the list
::  associated to it as described above, and then moves on to the next ship, until
::  the jar is empty.
::
::  this arm does not test any L1 transactions beyond the ones needed to spawn the
::  galaxy (+init-rut).
::
++  test-rut  ^-  tang
  =,  l2-event-gen
  ::
  =/  event-jar  gen-rut-jar
  =|  =^state:naive
  =^  f  state  (init-rut-full state)
  =/  initial-state  state
  =/  ship-list  rut-ship-list
  =/  suc-map  (make-success-map make-event-list)
  ~&  event-jar
  ::
  |-  ^-  tang
  ?~  ship-list  ~
  %+  weld  $(ship-list t.ship-list)
  =/  cur-ship  i.ship-list
  %+  category  (scow %p cur-ship)
  =/  current-events  (~(get ja event-jar) cur-ship)
  ::
  |-  ^-  tang
  ?~  current-events  ~
  %+  weld  $(current-events t.current-events)
  =/  cur-event  i.current-events
  %+  category  (weld "dominion " (scow %tas dominion.cur-event))
  %+  category  (weld "proxy " (scow %tas proxy.cur-event))
  %+  category  (weld "tx-type " (scow %tas tx-type.cur-event))
  %+  category  (weld "owner? " (scow %f owner.cur-event))
  %+  category  (weld "correct nonce? " (scow %f nonce.cur-event))
  ::
  =/  cur-point  (~(got by points.initial-state) cur-ship)
  =*  own  own.cur-point
  =/  cur-nonce
    ?-  proxy.cur-event
      %own       nonce.owner.own
      %spawn     nonce.spawn-proxy.own
      %manage    nonce.management-proxy.own
      %vote      nonce.voting-proxy.own
      %transfer  nonce.transfer-proxy.own
    ==
  :: wrong nonce and/or wrong owner do not increment nonce
  =/  new-nonce  ?:  &(nonce.cur-event owner.cur-event)
                   +(cur-nonce)
                 cur-nonce
  ::
  =/  state  initial-state
  =/  expect-state  initial-state
  |^
  %+  expect-eq
    !>
    |^  ^-  ^state:naive
    ?.  (~(got by suc-map) cur-event)
      %-  alter-state
      ?-  proxy.cur-event
        %own       cur-point(nonce.owner.own new-nonce)
        %spawn     cur-point(nonce.spawn-proxy.own new-nonce)
        %manage    cur-point(nonce.management-proxy.own new-nonce)
        %vote      cur-point(nonce.voting-proxy.own new-nonce)
        %transfer  cur-point(nonce.transfer-proxy.own new-nonce)
      ==
    ?+  tx-type.cur-event  !!
      %transfer-point        set-xfer
      %configure-keys        set-keys
      %set-management-proxy  set-mgmt-proxy
      %set-spawn-proxy       set-spwn-proxy
      %set-transfer-proxy    set-xfer-proxy
      %spawn                 (new-point which-spawn)
      %escape                (set-escape which-escape-l1)
    ==
    ::
    ++  set-keys  ^-  ^state:naive
      =/  new-keys
      %=  cur-point
        life.keys.net    +(life.keys.net:(~(got by points.initial-state) cur-ship))
        suite.keys.net   suit
        auth.keys.net    auth
        crypt.keys.net   encr
      ==
      (alter-state new-keys)
    ::
    ++  set-xfer  ^-  ^state:naive
      =/  new-xfer
      %=  cur-point
        address.owner.own  (addr %transfer-test)
      ==
      (alter-state new-xfer)
    ::
    ++  set-mgmt-proxy  ^-  ^state:naive
      =/  new-mgmt
      %=  cur-point
        address.management-proxy.own  (addr %proxy-test)
      ==
      (alter-state new-mgmt)
    ::
    ++  set-spwn-proxy  ^-  ^state:naive
      =/  new-spwn
      %=  cur-point
        address.spawn-proxy.own  (addr %proxy-test)
      ==
      (alter-state new-spwn)
    ::
    ++  set-xfer-proxy  ^-  ^state:naive
      =/  new-xfer
      %=  cur-point
        address.transfer-proxy.own  (addr %proxy-test)
      ==
      (alter-state new-xfer)
    ::
    ++  set-escape
      |=  =ship  ^-  ^state:naive
      =/  new-escp
      %=  cur-point
        escape.net  (some ship)
      ==
      (alter-state new-escp)
    ::
    ++  new-point
      :: TODO clean up this horrifying gate
      |=  =ship  ^-  ^state:naive
      =|  new-point=point:naive
      =/  spawned
      %=  new-point
        dominion  %l2
        address.owner.own  (addr (~(got by default-own-keys) cur-ship))
        address.transfer-proxy.own  (addr %spawn-test)
        sponsor.net  [has=%.y who=cur-ship]
      ==
      =/  expect-state  (alter-state cur-point)  :: this updates the nonce of the spawner
      %=  expect-state
        points  (~(put by points.expect-state) ship spawned)
      ==
    ::
    ++  alter-state
      :: this updates the expect-state with the new point, and takes
      :: care of incrementing the nonce as well.
      |=  alt-point=point:naive  ^-  ^state:naive
      =/  updated-point=point:naive
      ?-  proxy.cur-event
        %own       alt-point(nonce.owner.own new-nonce)
        %spawn     alt-point(nonce.spawn-proxy.own new-nonce)
        %manage    alt-point(nonce.management-proxy.own new-nonce)
        %vote      alt-point(nonce.voting-proxy.own new-nonce)
        %transfer  alt-point(nonce.transfer-proxy.own new-nonce)
      ==
      %=  expect-state
        points  (~(put by points.expect-state) cur-ship updated-point)
      ==
    ::
    --  :: end of expected state
  ::  actual state
    !>
    |^  ^-  ^state:naive
    =^    f
        state
      %-  n
      :+  state
        %bat
      =<  q
      %-  gen-tx
      :+  ?:  nonce.cur-event
            cur-nonce
          999 :: wrong nonce
        :-  :-  cur-ship
            proxy.cur-event
        def-args
      ?:  owner.cur-event
        ?+  proxy.cur-event  %wrong-key
          %own     (~(got by default-own-keys) cur-ship)
          %manage  (~(got by default-manage-keys) cur-ship)
          %spawn   ?:  =(rank.cur-event %star)
                     (~(got by default-spawn-keys) cur-ship)
                   %wrong-key
        ==
      %wrong-key :: if not owner then use wrong key
    state
    ::
    ++  def-args
      ^-  skim-tx:naive
      |^
      ?+  tx-type.cur-event  !!
        %spawn                 [%spawn which-spawn (addr %spawn-test)]
        %transfer-point        [%transfer-point (addr %transfer-test) |]
        %configure-keys        [%configure-keys encr auth suit |]
        %escape                [%escape which-escape-l2]
        :: %cancel-escape
        :: %adopt
        :: %reject
        :: %detach
        %set-management-proxy  [%set-management-proxy (addr %proxy-test)]
        %set-spawn-proxy       [%set-spawn-proxy (addr %proxy-test)]
        %set-transfer-proxy    [%set-transfer-proxy (addr %proxy-test)]
      ==
      ::
      --  :: +def-args
    ::
    --  :: end of actual state
    ::
  ++  encr    (shax 'You will forget that you ever read this sentence.')
  ++  auth    (shax 'You cant know that this sentence is true.')
  ++  suit    1
  ::
  ++  which-spawn  ^-  ship
    ?+  cur-ship  !!
      %~rut            ~hasrut
      %~rigrut         ~batbec-tapmep
      %~larsyx-mapmeg  ~nocryl-tobned
      %~holrut         ~namtuc-ritnux
      %~rabsum-ravtyd  ~docsec-wanlug
      %~dovmul-mogryt  ~docsec-wanlug
      %~pidted-dacnum  ~docsec-wanlug
      %~losrut         ~mishus-loplus
      %~radres-tinnyl  ~tapfur-fitsep
      %~pinpun-pilsun  ~tapfur-fitsep
      %~habtyc-nibpyx  ~tapfur-fitsep
      %~disryt-nolpet  ~tapfur-fitsep
    ==
  ::
  ++  which-escape-l1  ^-  ship
  :: currently unused
  :: escaping to a L1 point
    ?-  rank.cur-event
      %galaxy  ~red
      %star    ~red
      %planet  ~rigred
    ==
  ::
  ++  which-escape-l2  ^-  ship
  :: escaping to a L2 point
    ?-  rank.cur-event
      %galaxy  ~red
      %star    ~red
      %planet  ~losred
    ==
  ::
  --  :: end of +expect-eq
::
::  the following are sponsorship tests. they ought to eventually be consolidated
::  into one large test, but for now it will be easier to tell which one is failing
::  by splitting them up
::
::  the following are L2 sponsorship tests. the syntax is test-red-X-Y-action. X is the
::  layer of the sponsee, Y is the layer of the sponsor
::
++  test-red-l2-l2-adopt  ^-  tang
  =/  pp-adopt  [losred-own %adopt ~pinpun-pilsun]
  =/  pp-m-adopt  [losred-mgmt %adopt ~pinpun-pilsun]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~losred]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 pp-adopt %losred-key-0))
      [escape.net sponsor.net]:(~(got by points.state) ~pinpun-pilsun)
    ::
    %+  expect-eq
      !>  [~ %.y ~losred]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 pp-m-adopt %losred-mkey-0))
      [escape.net sponsor.net]:(~(got by points.state) ~pinpun-pilsun)
  ==
::
++  test-red-l1-l2-adopt
  =/  lm-adopt  [losred-own %adopt ~larsyx-mapmeg]
  =/  lm-m-adopt  [losred-mgmt %adopt ~larsyx-mapmeg]
  ::
  ;:  weld
  %+  expect-eq
    !>  [~ %.y ~losred]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-red-full state)
    =^  f  state  (n state %bat q:(gen-tx 0 lm-adopt %losred-key-0))
    [escape.net sponsor.net]:(~(got by points.state) ~larsyx-mapmeg)
  ::
  %+  expect-eq
    !>  [~ %.y ~losred]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-red-full state)
    =^  f  state  (n state %bat q:(gen-tx 0 lm-m-adopt %losred-mkey-0))
    [escape.net sponsor.net]:(~(got by points.state) ~larsyx-mapmeg)
  ==
::
++  test-red-l2-l1-adopt
  =/  dm-adopt  [rigred-own %adopt ~dovmul-mogryt]
  =/  dm-m-adopt  [rigred-mgmt %adopt ~dovmul-mogryt]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~rigred]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 dm-adopt %rigred-key-0))
      [escape.net sponsor.net]:(~(got by points.state) ~dovmul-mogryt)
    ::
    %+  expect-eq
      !>  [~ %.y ~rigred]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 dm-m-adopt %rigred-mkey-0))
      [escape.net sponsor.net]:(~(got by points.state) ~dovmul-mogryt)
  ==
::
++  test-red-l1-l1-adopt
  =/  rr-adopt  [rigred-own %adopt ~rabsum-ravtyd]
  =/  rr-m-adopt  [rigred-mgmt %adopt ~rabsum-ravtyd]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~rigred]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 rr-adopt %rigred-key-0))
      [escape.net sponsor.net]:(~(got by points.state) ~rabsum-ravtyd)
    ::
    %+  expect-eq
      !>  [~ %.y ~rigred]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 rr-m-adopt %rigred-mkey-0))
      [escape.net sponsor.net]:(~(got by points.state) ~rabsum-ravtyd)
  ==
::
::  the following tests L2 %rejects
++  test-red-l2-l2-reject  ^-  tang
  =/  pp-reject  [losred-own %reject ~pinpun-pilsun]
  =/  pp-m-reject  [losred-mgmt %reject ~pinpun-pilsun]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 pp-reject %losred-key-0))
      [escape.net sponsor.net]:(~(got by points.state) ~pinpun-pilsun)
    ::
    %+  expect-eq
      !>  [~ %.y ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 pp-m-reject %losred-mkey-0))
      [escape.net sponsor.net]:(~(got by points.state) ~pinpun-pilsun)
  ==
::
++  test-red-l2-l1-reject  ^-  tang
  =/  dm-reject  [rigred-own %reject ~dovmul-mogryt]
  =/  dm-m-reject  [rigred-mgmt %reject ~dovmul-mogryt]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 dm-reject %rigred-key-0))
      [escape.net sponsor.net]:(~(got by points.state) ~dovmul-mogryt)
    ::
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 dm-m-reject %rigred-mkey-0))
      [escape.net sponsor.net]:(~(got by points.state) ~dovmul-mogryt)
  ==
::
++  test-red-l1-l2-reject  ^-  tang
  =/  lm-reject  [losred-own %reject ~larsyx-mapmeg]
  =/  lm-m-reject  [losred-mgmt %reject ~larsyx-mapmeg]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~rigrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 lm-reject %losred-key-0))
      [escape.net sponsor.net]:(~(got by points.state) ~larsyx-mapmeg)
    ::
    %+  expect-eq
      !>  [~ %.y ~rigrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 lm-m-reject %losred-mkey-0))
      [escape.net sponsor.net]:(~(got by points.state) ~larsyx-mapmeg)
  ==
::
++  test-red-l1-l1-reject  ^-  tang
  =/  rr-reject  [rigred-own %reject ~rabsum-ravtyd]
  =/  rr-m-reject  [rigred-mgmt %reject ~rabsum-ravtyd]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 rr-reject %rigred-key-0))
      [escape.net sponsor.net]:(~(got by points.state) ~rabsum-ravtyd)
    ::
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 rr-m-reject %rigred-mkey-0))
      [escape.net sponsor.net]:(~(got by points.state) ~rabsum-ravtyd)
  ==
::
::  the following tests L2 %cancel-escape
::
++  test-red-l2-l2-cancel-escape  ^-  tang
  =/  pp-cancel-escape  [[~pinpun-pilsun %own] %cancel-escape ~losred]
  =/  pp-m-cancel-escape  [[~pinpun-pilsun %manage] %cancel-escape ~losred]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 2 pp-cancel-escape %losrut-pp-key-0))
      [escape.net sponsor.net]:(~(got by points.state) ~pinpun-pilsun)
    ::
    %+  expect-eq
      !>  [~ %.y ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 pp-m-cancel-escape %losrut-pp-mkey-0))
      [escape.net sponsor.net]:(~(got by points.state) ~pinpun-pilsun)
  ==
::
++  test-red-l2-l1-cancel-escape  ^-  tang
  =/  dm-cancel-escape  [[~dovmul-mogryt %own] %cancel-escape ~rigred]
  =/  dm-m-cancel-escape  [[~dovmul-mogryt %manage] %cancel-escape ~rigred]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 2 dm-cancel-escape %holrut-dm-key-0))
      [escape.net sponsor.net]:(~(got by points.state) ~dovmul-mogryt)
    ::
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 dm-m-cancel-escape %holrut-dm-mkey-0))
      [escape.net sponsor.net]:(~(got by points.state) ~dovmul-mogryt)
  ==
::
++  test-red-l1-l2-cancel-escape  ^-  tang
  =/  lm-cancel-escape  [[~larsyx-mapmeg %own] %cancel-escape ~losred]
  =/  lm-m-cancel-escape  [[~larsyx-mapmeg %manage] %cancel-escape ~losred]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~rigrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 1 lm-cancel-escape %rigrut-lm-key-0))
      [escape.net sponsor.net]:(~(got by points.state) ~larsyx-mapmeg)
    ::
    %+  expect-eq
      !>  [~ %.y ~rigrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 lm-m-cancel-escape %rigrut-lm-mkey-0))
      [escape.net sponsor.net]:(~(got by points.state) ~larsyx-mapmeg)
  ==
::
++  test-red-l1-l1-cancel-escape  ^-  tang
  =/  rr-cancel-escape  [[~rabsum-ravtyd %own] %cancel-escape ~rigred]
  =/  rr-m-cancel-escape  [[~rabsum-ravtyd %manage] %cancel-escape ~rigred]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 1 rr-cancel-escape %holrut-rr-key-0))
      [escape.net sponsor.net]:(~(got by points.state) ~rabsum-ravtyd)
    ::
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-red-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 rr-m-cancel-escape %holrut-rr-mkey-0))
      [escape.net sponsor.net]:(~(got by points.state) ~rabsum-ravtyd)
  ==
::
::  the following tests L2 %detach. the format test-rut-X-Y-detach means
::  X is the layer of the sponsor, Y is the layer of the sponsee
::
++  test-rut-l2-l2-detach  ^-  tang
  =/  pp-detach  [losrut-own %detach ~pinpun-pilsun]
  =/  pp-m-detach  [losrut-mgmt %detach ~pinpun-pilsun]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.n ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-rut-full state)
      =^  f  state  (n state %bat q:(gen-tx 2 pp-detach %losrut-key-0))
      [escape.net sponsor.net]:(~(got by points.state) ~pinpun-pilsun)
    ::
    %+  expect-eq
      !>  [~ %.n ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-rut-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 pp-m-detach %losrut-mkey-0))
      [escape.net sponsor.net]:(~(got by points.state) ~pinpun-pilsun)
  ==
::
++  test-rut-l2-l1-detach  ^-  tang
  =/  rt-detach  [losrut-own %detach ~radres-tinnyl]
  =/  rt-m-detach  [losrut-mgmt %detach ~radres-tinnyl]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.n ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-rut-full state)
      =^  f  state  (n state %bat q:(gen-tx 2 rt-detach %losrut-key-0))
      [escape.net sponsor.net]:(~(got by points.state) ~radres-tinnyl)
    ::
    %+  expect-eq
      !>  [~ %.n ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-rut-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 rt-m-detach %losrut-mkey-0))
      [escape.net sponsor.net]:(~(got by points.state) ~radres-tinnyl)
  ==
::
++  test-rut-l1-l2-detach  ^-  tang
  =/  dm-detach  [holrut-own %detach ~dovmul-mogryt]
  =/  dm-m-detach  [holrut-mgmt %detach ~dovmul-mogryt]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.n ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-rut-full state)
      =^  f  state  (n state %bat q:(gen-tx 1 dm-detach %holrut-key-0))
      [escape.net sponsor.net]:(~(got by points.state) ~dovmul-mogryt)
    ::
    %+  expect-eq
      !>  [~ %.n ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-rut-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 dm-m-detach %holrut-mkey-0))
      [escape.net sponsor.net]:(~(got by points.state) ~dovmul-mogryt)
  ==
::
++  test-rut-l1-l1-detach  ^-  tang
  =/  lm-detach  [rigrut-own %detach ~larsyx-mapmeg]
  =/  lm-m-detach  [rigrut-mgmt %detach ~larsyx-mapmeg]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.n ~rigrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-rut-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 lm-detach %rigrut-key-0))
      [escape.net sponsor.net]:(~(got by points.state) ~larsyx-mapmeg)
    ::
    %+  expect-eq
      !>  [~ %.n ~rigrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-rut-full state)
      =^  f  state  (n state %bat q:(gen-tx 0 lm-m-detach %rigrut-mkey-0))
      [escape.net sponsor.net]:(~(got by points.state) ~larsyx-mapmeg)
  ==
::
::  the following tests are for sponsorship actions between two L1 points
++  test-red-l1-escape-l2-adopt  ^-  tang
  =/  rr-adopt  [rigred-own %adopt ~rabsum-ravtyd]
  %+  expect-eq
    !>  [~ %.y ~rigred]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-red-full state)
    =^  f  state  (n state (escape-requested:l1 ~rabsum-ravtyd ~rigred))
    =^  f  state  (n state %bat q:(gen-tx 0 rr-adopt %rigred-key-0))
    [escape.net sponsor.net]:(~(got by points.state) ~rabsum-ravtyd)
::
++  test-red-l2-escape-l1-adopt  ^-  tang
  =/  rr-escape  [[~rabsum-ravtyd %own] %escape ~rigred]
  %+  expect-eq
    !>  [[~ ~rigred] %.y ~holrut]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-red-full state)
    =^  f  state  (n state %bat q:(gen-tx 1 rr-escape %holrut-rr-key-0))
    =^  f  state  (n state (escape-accepted:l1 ~rigred ~rabsum-ravtyd))
    [escape.net sponsor.net]:(~(got by points.state) ~rabsum-ravtyd)
::
++  test-rut-l1-adoption-on-l2-wrong-key-or-nonce
  =/  rr-escape  [[~rabsum-ravtyd %own] %escape ~rigred]
  =/  rr-adopt   [rigred-own %adopt ~rabsum-ravtyd]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-rut-full state)
      =^  f  state  (n state %bat q:(gen-tx 1 rr-escape %wrong-key))
      =^  f  state  (n state %bat q:(gen-tx 0 rr-adopt %rigred-key-0))
      [escape.net sponsor.net]:(~(got by points.state) ~rabsum-ravtyd)
    ::
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-rut-full state)
      =^  f  state  (n state %bat q:(gen-tx 999 rr-escape %holrut-rr-key-0))
      =^  f  state  (n state %bat q:(gen-tx 0 rr-adopt %rigred-key-0))
      [escape.net sponsor.net]:(~(got by points.state) ~rabsum-ravtyd)
  ==
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
    !>  [(addr %sambud-skey-1) 1]
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
++  test-linnup-torsyx-spawn  ^-  tang
  :: try to spawn a L2 planet with a L2 planet
  :: this test is deprecated, covered by +test-rut
  =/  rt-spawn                  [lt-own %spawn ~radres-tinnyl (addr %rt-key-0)]
  =/  lt-spawn                  [marbud-own %spawn ~linnup-torsyx (addr %lt-key-0)]
  =/  lt-transfer-yes-breach    [lt-xfr %transfer-point (addr %lt-key-0) &]
  ::
  =|  =^state:naive
  =^  f  state  (init-marbud state)
  =^  f  state  (init-litbud state)
  =^  f  state  (n state %bat q:(gen-tx 0 lt-spawn %marbud-key-0))
  =^  f  state  (n state %bat q:(gen-tx 0 lt-transfer-yes-breach %lt-key-0))
  =/  lt-point  (~(got by points.state) ~linnup-torsyx)
  =/  new-lt  lt-point(nonce.owner.own 1)
  =/  no-op-state  state(points (~(put by points.state) ~linnup-torsyx new-lt))
  ::
  %+  expect-eq
    !>  no-op-state
  ::
    !>
    =^  f  state  (n state %bat q:(gen-tx 0 rt-spawn %lt-key-0))
    state
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
  =|  =^state:naive
  =^  f  state  (init-marbud state)
  =^  f  state  (n state %bat q:(gen-tx 0 marbud-sproxy %marbud-key-0))
  =^  f  state  (n state %bat q:(gen-tx 1 lt-spawn-0 %marbud-key-0))
  =/  marbud-point  (~(got by points.state) ~marbud)
  =/  new-marbud  marbud-point(nonce.spawn-proxy.own 1)
  =/  no-op-state  state(points (~(put by points.state) ~marbud new-marbud))
  ::
  %+  expect-eq
    !>  no-op-state
  ::
    !>
    =^  f  state  (n state %bat q:(gen-tx 0 lt-spawn-1 %marbud-skey))
    state
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
      =^  f  state  (n state %bat q:(gen-tx 2 new-keys-no-reset %marbud-key-1)) ::inc life
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
