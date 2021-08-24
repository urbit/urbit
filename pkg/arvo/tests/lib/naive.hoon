/+  *test, naive, ethereum, azimuth, *naive-transactions
::
|%
:: This gate passes a state and input to naive.hoon for both L1 and L2
:: transactions. Every transaction implemented in this test suite utilizes it.
::
++  n  |=([=^state:naive =^input:naive] (%*(. naive lac &) verifier 1.337 +<))
::
++  orm   ((on ship point:naive) por:naive)
++  addr  address-from-prv:key:ethereum
::
::  The next section of this core generates "universes" of Azimuth points, each
::  of which is intended for particular test(s). There are more of these
::  than strictly necessary - we utilize different galaxies/etc to perform
::  different kinds of tests, and using different @p's helps to remember
::  what the galaxy was set up to test when reading the tests.
::
::  ~zod is for testing potential padding issues caused by leading or trailing
::  zeroes.
::
++  init-zod
  |=  =^state:naive
  ^-  [effects:naive ^state:naive]
  =^  f1  state  (n state (owner-changed:l1 ~zod (addr %zod-key-0)))
  =^  f2  state  (n state (owner-changed:l1 ~dopzod (addr %dopzod-key-0)))
  =^  f3  state  (n state (changed-spawn-proxy:l1 ~zod (addr %zod-skey-0)))
  =^  f4  state  (n state (changed-spawn-proxy:l1 ~zod deposit-address:naive))
  =^  f5  state  (n state (owner-changed:l1 ~dopzod deposit-address:naive))
  [:(welp f1 f2 f3 f4 f5) state]
::
::  ~bud is so that we aren't testing something impossible in Azimuth, like a
::  star spawned before its sponsor galaxy
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
::  ~rut and the accompanying points beneath is for testing nearly every
::  sort of properly-formed L2 transactions unrelated to sponsorship
::  actions, each submitted as a single-transaction batch. In particular
::  it tests %transfer-point, %configure-keys, %spawn, %set-management-proxy
::  %set-spawn-proxy, and %set-transfer-proxy. See +test-rut for more
::  information.
::
::  ~rut is %l1 galaxy
::  ~tyl is %spawn galxy
::
::  ~rigrut is %l1 star
::  ~larsyx-mapmeg is %l1 planet under ~rigrut
::  ~holrut is %spawn star
::  ~rabsum-ravtyd is %l1 planet under ~holrut
::  ~dovmul-mogryt is %l2 planet under ~holrut made w/ %own proxy
::  ~pidted-dacnum is %l2 planet under ~holrut made w/ %spawn proxy predeposited
::
::  ~losrut is %l2 star
::  ~radres-tinnyl is %l1 planet under ~losrut
::  ~pinpun-pilsun is %l2 planet under ~losrut made w/ %own proxy
::  ~habtyc-nibpyx is %l2 planet under ~losrut made w/ %spawn proxy predeposited
::  ~disryt-nolpet is %l2 planet under ~losrut made w/ %spawn proxy postdeposited
::
++  init-rut-full
  |=  =^state:naive
  ^-  [effects:naive ^state:naive]
  ::
  =/  dm-spawn
    [[~holrut %own] %spawn ~dovmul-mogryt (addr %holrut-dm-key-0)]
  =/  pd-spawn
    [[~holrut %spawn] %spawn ~pidted-dacnum (addr %holrut-pd-key-0)]
  ::
  =/  pp-spawn
    [[~losrut %own] %spawn ~pinpun-pilsun (addr %losrut-pp-key-0)]
  =/  hn-spawn
    [[~losrut %spawn] %spawn ~habtyc-nibpyx (addr %losrut-hn-key-0)]
  =/  dn-spawn
    [[~losrut %spawn] %spawn ~disryt-nolpet (addr %losrut-dn-key-0)]
  =/  losrut-sproxy
    [[~losrut %spawn] %set-spawn-proxy (addr %losrut-skey-1)]
  =/  losrut-mproxy
    [[~losrut %own] %set-management-proxy (addr %losrut-mkey-0)]
  =/  losrut-tproxy
    [[~losrut %own] %set-transfer-proxy (addr %losrut-tkey-0)]
  ::
  =/  dm-xfer
    [[~dovmul-mogryt %transfer] %transfer-point (addr %holrut-dm-key-0) &]
  =/  dm-tproxy
    [[~dovmul-mogryt %own] %set-transfer-proxy (addr %dm-tkey-0)]
  =/  dm-mproxy
    [[~dovmul-mogryt %own] %set-management-proxy (addr %holrut-dm-mkey-0)]
  ::
  =/  pd-xfer
    [[~pidted-dacnum %transfer] %transfer-point (addr %holrut-pd-key-0) &]
  =/  pd-tproxy
    [[~pidted-dacnum %own] %set-transfer-proxy (addr %pd-tkey-0)]
  =/  pd-mproxy
    [[~pidted-dacnum %own] %set-management-proxy (addr %holrut-pd-mkey-0)]
  ::
  ::
  =/  pp-xfer
    [[~pinpun-pilsun %transfer] %transfer-point (addr %losrut-pp-key-0) &]
  =/  pp-tproxy
    [[~pinpun-pilsun %own] %set-transfer-proxy (addr %pp-tkey-0)]
  =/  pp-mproxy
    [[~pinpun-pilsun %own] %set-management-proxy (addr %losrut-pp-mkey-0)]
  ::
  =/  hn-xfer
    [[~habtyc-nibpyx %transfer] %transfer-point (addr %losrut-hn-key-0) &]
  =/  hn-tproxy
    [[~habtyc-nibpyx %own] %set-transfer-proxy (addr %hn-tkey-0)]
  =/  hn-mproxy
    [[~habtyc-nibpyx %own] %set-management-proxy (addr %losrut-hn-mkey-0)]
  ::
  =/  dn-xfer
    [[~disryt-nolpet %transfer] %transfer-point (addr %losrut-dn-key-0) &]
  =/  dn-tproxy
    [[~disryt-nolpet %own] %set-transfer-proxy (addr %dn-tkey-0)]
  =/  dn-mproxy
    [[~disryt-nolpet %own] %set-management-proxy (addr %losrut-dn-mkey-0)]
  ::
  =^  f1   state
    (n state (owner-changed:l1 ~rut (addr %rut-key-0)))
  =^  f2   state
    (n state (owner-changed:l1 ~rigrut (addr %rigrut-key-0)))
  =^  f3   state
    (n state (owner-changed:l1 ~holrut (addr %holrut-key-0)))
  =^  f4   state
    (n state (owner-changed:l1 ~losrut (addr %losrut-key-0)))
  =^  f5   state
    (n state (owner-changed:l1 ~larsyx-mapmeg (addr %rigrut-lm-key-0)))
  =^  f6   state
    (n state (owner-changed:l1 ~rabsum-ravtyd (addr %holrut-rr-key-0)))
  =^  f7   state
    (n state (owner-changed:l1 ~radres-tinnyl (addr %losrut-rt-key-0)))
  =^  f8   state
    (n state (changed-spawn-proxy:l1 ~rut (addr %rut-skey-0)))
  =^  f9   state
    (n state (changed-spawn-proxy:l1 ~holrut (addr %holrut-skey-0)))
  =^  f10  state
    (n state (changed-spawn-proxy:l1 ~losrut (addr %losrut-skey-0)))
  =^  f11  state
    (n state (changed-spawn-proxy:l1 ~holrut deposit-address:naive))
  ::
  =^  f12  state
    (n state %bat q:(gen-tx 0 dm-spawn %holrut-key-0))
  =^  f13  state
    (n state %bat q:(gen-tx 0 pd-spawn %holrut-skey-0))
  =^  f14  state
    (n state (owner-changed:l1 ~losrut deposit-address:naive))
  =^  f15  state
    (n state %bat q:(gen-tx 0 pp-spawn %losrut-key-0))
  =^  f16  state
    (n state %bat q:(gen-tx 0 hn-spawn %losrut-skey-0))
  =^  f17  state
    (n state %bat q:(gen-tx 1 losrut-sproxy %losrut-skey-0))
  =^  f18  state
    (n state %bat q:(gen-tx 2 dn-spawn %losrut-skey-1))
  =^  f19  state
    (n state %bat q:(gen-tx 0 dm-xfer %holrut-dm-key-0))
  =^  f20  state
    (n state %bat q:(gen-tx 0 pd-xfer %holrut-pd-key-0))
  =^  f21  state
    (n state %bat q:(gen-tx 0 pp-xfer %losrut-pp-key-0))
  =^  f22  state
    (n state %bat q:(gen-tx 0 hn-xfer %losrut-hn-key-0))
  =^  f23  state
    (n state %bat q:(gen-tx 0 dn-xfer %losrut-dn-key-0))
  ::
  =^  p1   state
    (n state (changed-management-proxy:l1 ~rut (addr %rut-mkey-0)))
  =^  p2   state
    (n state (changed-management-proxy:l1 ~rigrut (addr %rigrut-mkey-0)))
  =^  p3   state
    (n state (changed-management-proxy:l1 ~larsyx-mapmeg (addr %rigrut-lm-mkey-0)))
  =^  p4   state
    (n state (changed-management-proxy:l1 ~holrut (addr %holrut-mkey-0)))
  =^  p5   state
    (n state (changed-management-proxy:l1 ~rabsum-ravtyd (addr %holrut-rr-mkey-0)))
  =^  p6   state
    (n state (changed-management-proxy:l1 ~radres-tinnyl (addr %losrut-rt-mkey-0)))
  =^  p7   state
    (n state %bat q:(gen-tx 0 dm-mproxy %holrut-dm-key-0))
  =^  p8   state
    (n state %bat q:(gen-tx 0 pd-mproxy %holrut-pd-key-0))
  =^  p9   state
    (n state %bat q:(gen-tx 0 pp-mproxy %losrut-pp-key-0))
  =^  p10  state
    (n state %bat q:(gen-tx 0 hn-mproxy %losrut-hn-key-0))
  =^  p11  state
    (n state %bat q:(gen-tx 0 dn-mproxy %losrut-dn-key-0))
  =^  p12  state
    (n state %bat q:(gen-tx 1 losrut-mproxy %losrut-key-0))
  =^  p13  state
    (n state (changed-transfer-proxy:l1 ~rut (addr %rut-tkey-0)))
  =^  p14  state
    (n state (changed-transfer-proxy:l1 ~rigrut (addr %rigrut-tkey-0)))
  =^  p15  state
    (n state (changed-transfer-proxy:l1 ~larsyx-mapmeg (addr %lm-tkey-0)))
  =^  p16  state
    (n state (changed-transfer-proxy:l1 ~holrut (addr %holrut-tkey-0)))
  =^  p17  state
    (n state (changed-transfer-proxy:l1 ~rabsum-ravtyd (addr %rr-tkey-0)))
  =^  p18  state
    (n state (changed-transfer-proxy:l1 ~radres-tinnyl (addr %rt-tkey-0)))
  =^  p19  state
    (n state %bat q:(gen-tx 2 losrut-tproxy %losrut-key-0))
  =^  p20  state
    (n state %bat q:(gen-tx 1 dm-tproxy %holrut-dm-key-0))
  =^  p21  state
    (n state %bat q:(gen-tx 1 pd-tproxy %holrut-pd-key-0))
  =^  p22  state
    (n state %bat q:(gen-tx 1 pp-tproxy %losrut-pp-key-0))
  =^  p23  state
    (n state %bat q:(gen-tx 1 hn-tproxy %losrut-hn-key-0))
  =^  p24  state
    (n state %bat q:(gen-tx 1 dn-tproxy %losrut-dn-key-0))
  =^  t1   state
    (n state (owner-changed:l1 ~tyl (addr %tyl-key-0)))
  =^  t2   state
    (n state (changed-spawn-proxy:l1 ~tyl (addr %tyl-skey-0)))
  =^  t3   state
    (n state (changed-spawn-proxy:l1 ~tyl deposit-address:naive))
  =^  t4   state
    (n state (changed-management-proxy:l1 ~tyl (addr %tyl-mkey-0)))
  =^  t5   state
    (n state (changed-transfer-proxy:l1 ~tyl (addr %tyl-tkey-0)))
  ::
  :-  ;:  welp
      f1  f2  f3  f4  f5  f6  f7  f8  f9  f10
      f11  f12  f13  f14  f15  f16  f17  f18
      f19  f20  f21  f22  f23
      p1  p2  p3  p4  p5  p6  p7  p8  p9  p10
      p11  p12  p13  p14  p15  p16  p17  p18
      p19  p20  p21  p22  p23  p24
      t1  t2  t3  t4  t5
      ==
  state
::
:: +init-red-full adds another galaxy to the ~rut universe, ~red, and additional
:: points used for testing sponsorship actions.
::
++  init-red-full
  |=  =^state:naive
  ^-  [effects:naive ^state:naive]
  =/  pp-escape  [[~pinpun-pilsun %own] %escape ~losred]
  =/  dm-escape  [[~dovmul-mogryt %own] %escape ~rigred]
  =/  lm-escape  [[~larsyx-mapmeg %own] %escape ~losred]
  =/  rr-escape  [[~rabsum-ravtyd %own] %escape ~rigred]
  =^  f1   state  (init-rut-full state)
  =^  f2   state  (n state (owner-changed:l1 ~red (addr %red-key-0)))
  =^  f3   state  (n state (owner-changed:l1 ~rigred (addr %rigred-key-0)))
  =^  f4   state  (n state (owner-changed:l1 ~losred (addr %losred-key-0)))
  =^  f5   state
    (n state (changed-management-proxy:l1 ~rigred (addr %rigred-mkey-0)))
  =^  f6   state
    (n state (changed-management-proxy:l1 ~losred (addr %losred-mkey-0)))
  =^  f7   state  (n state (owner-changed:l1 ~losred deposit-address:naive))
  ::  each pending escape will be followed by a %adopt, %reject, or
  ::  %cancel-escape
  ::  L1->L1
  ::
  =^  f8   state  (n state %bat q:(gen-tx 0 rr-escape %holrut-rr-key-0))
  ::  L2->L2
  ::
  =^  f9   state  (n state %bat q:(gen-tx 2 pp-escape %losrut-pp-key-0))
  ::  L2->L1
  ::
  =^  f10  state  (n state %bat q:(gen-tx 2 dm-escape %holrut-dm-key-0))
  ::  L1->L2
  ::
  =^  f11  state  (n state %bat q:(gen-tx 0 lm-escape %rigrut-lm-key-0))
  [:(welp f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11) state]
::
::  ~dopbud is for testing L1 ownership with L2 spawn proxy
::
++  init-dopbud
  |=  =^state:naive
  ^-  [effects:naive ^state:naive]
  =^  f1  state  (init-bud state)
  =^  f2  state  (n state (owner-changed:l1 ~dopbud (addr %dopbud-key-0)))
  =^  f3  state
    (n state (changed-spawn-proxy:l1 ~dopbud deposit-address:naive))
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
  =^  f1  state  (n state (owner-changed:l1 ~litbud (addr %litbud-key-0)))
  =^  f2  state  (n state (owner-changed:l1 ~litbud deposit-address:naive))
  [:(welp f1 f2) state]
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
::  +l2-event-gen is a core used to generate all permutations of
::  [=rank owner=? nonce=? =dominion:naive =proxy:naive =tx-type]
::  as well as whether such an event ought to succeed as a L2 transaction,
::  assuming that arguments are appropriate. The gates in this core are
::  only utilized by +test-rut, but the types are used elsewhere in addition.
::
::  We note that while +test-rut only tests actions unrelated to sponsorship,
::  +l2-event-gen deals with all L2 transaction types for the sake of potential
::  future needs.
::
++  l2-event-gen
  |%
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
  +$  full-tx  [nonce=@ =tx:naive pk=@]
  +$  tx-list  (list full-tx)
  ::
  ++  make-success-map
    :: +make-success-map maps each $event to a flag denoting whether or not such
    :: a L2 transaction ought to succeed or fail, assuming that the arguments for
    :: the transaction are appropriate for the transaction (e.g. ~marzod
    :: attempting to spawn ~wicdev-wisryt).
    ::
    :: This is done in a more verbose style than strictly necessary to make it
    :: easier to read through and determine why a particular $event maps to %.y
    :: or %.n
    ::
    |=  =event-list  ^-  success-map
    =|  =success-map
    |^
    ?~  event-list  success-map
    =/  cur-event  i.event-list
    ?:  ?|  =(owner.cur-event %.n)     :: if owner or nonce are wrong then the
            =(nonce.cur-event %.n)     :: event fails
        ==
        (add-event-check cur-event %.n)
    ::                                 :: we first switch on dominion since
    ?-    dominion.cur-event           :: it cleaves the largest differences
        %l1                            :: in what is permitted
      (add-event-check cur-event (l1-check cur-event))
      ::
        %spawn
      (add-event-check cur-event (spawnd-check cur-event))
      ::
        %l2
      (add-event-check cur-event (l2-check cur-event))
    ==
    ::
    ++  add-event-check
      |=  [=event suc=?]
      %=  ^$
          event-list   +.event-list
          success-map  (~(put by success-map) event suc)
      ==
    ::
    ++  l1-check                                :: checks for %l1 dominion
      |^
      |=  cur-event=event  ^-  ?
      :: Switch on which proxy is attempting the transaction.
      :: L1 points are allowed to perform L2 sponsorship actions, which can be
      :: performed with either %own or %manage proxies.
      ::
      ?-  proxy.cur-event
        %own       (manage-own-check cur-event) :: sponsorship tx allowed
        %spawn     %.n                          :: cannot do sponsorship tx
        %manage    (manage-own-check cur-event) :: sponsorship tx allowed
        %vote      %.n                          :: cannot do any L2 tx
        %transfer  %.n                          :: cannot do sponsorship tx
      ==
      ::
      ++  manage-own-check                      :: %own and %manage are
        |^                                      :: identical in %l1
        |=  cur-event=event  ^-  ?
        ?-  rank.cur-event                      :: switch on rank
          %galaxy  (galaxy-check cur-event)     :: each rank has different
          %star    (star-check cur-event)       :: allowed actions
          %planet  (planet-check cur-event)
        ==
        ++  galaxy-check
          |=  cur-event=event  ^-  ?
          ?+  tx-type.cur-event  %.n            :: galaxies do not have sponsors
            %adopt               %.y            :: can adopt on L2
            %reject              %.y            :: can reject on L2
            %detach              %.y            :: can detach on L2
          ==
        ++  star-check
          |=  cur-event=event  ^-  ?
          ?+  tx-type.cur-event  %.n            :: may only do L2 sponsorship tx
            %adopt               %.y            :: can %adopt on L2
            %reject              %.y            :: can %reject on L2
            %detach              %.y            :: can %detach on L2
            %escape              %.y            :: can %escape on L2
            %cancel-escape       %.y            :: can %cancel-escape on L2
          ==
        ++  planet-check
          |=  cur-event=event  ^-  ?
          ?+  tx-type.cur-event  %.n            :: planets do not have sponsees
            %escape              %.y            :: can %escape on L2
            %cancel-escape       %.y            :: can %cancel-escape on L2
          ==
        ::
        --                                      :: end +manage-own-check in %l1
      ::
      --                                        :: end +l1-check
    ::
    ++  spawnd-check                            :: checks for %spawn dominion
      |^
      |=  cur-event=event  ^-  ?
      ?-  rank.cur-event                        :: switch on rank
        %galaxy  (galaxy-check cur-event)       :: galaxies can be on %spawn
        %star    (star-check cur-event)         :: stars can be on %spawn
        %planet  %.n                            :: planets cannot be on %spawn
      ==
      ++  star-check                            :: %spawn dominion star check
        |^
        |=  cur-event=event  ^-  ?
        ?-  proxy.cur-event                     :: switch on proxy
          %own       (ownp-check cur-event)     :: can do sponsorship and spawn
          %manage    (managep-check cur-event)  :: can do sponsorship tx
          %spawn     (spawnp-check cur-event)   :: can do spawn tx
          %vote      %.n                        :: stars have no %vote proxy
          %transfer  %.n                        :: cannot sponsor/spawn
        ==
        ++  ownp-check
          |=  cur-event=event  ^-  ?
          ?+  tx-type.cur-event  %.n            :: only sponsorship/spawn tx
            %spawn               %.y            :: can %spawn on L2
            %adopt               %.y            :: can %adopt on L2
            %reject              %.y            :: can %reject on L2
            %detach              %.y            :: can %detach on L2
            %escape              %.y            :: can %escape on L2
            %cancel-escape       %.y            :: can %cancel-escape on L2
            %set-spawn-proxy     %.y            :: can %set-spawn-proxy on L2
          ==
        ++  managep-check                       :: %configure-keys disallowed
          |=  cur-event=event  ^-  ?            :: for %spawn dominion
          ?+  tx-type.cur-event  %.n            :: only sponsorship actions
            %adopt               %.y            :: can %adopt on L2
            %reject              %.y            :: can %reject on L2
            %detach              %.y            :: can %detach on L2
            %escape              %.y            :: can %escape on L2
            %cancel-escape       %.y            :: can %cancel-escape on L2
          ==
        ++  spawnp-check
          |=  cur-event=event  ^-  ?
          ?+  tx-type.cur-event  %.n            :: only spawn tx allowed
            %spawn               %.y            :: can %spawn on L2
            %set-spawn-proxy     %.y            :: can %set-spawn-proxy on L2
          ==
        --                                      :: end +star-check in %spawn
      ::
      ++  galaxy-check                          :: %spawn dominion galaxy check
        |^
        |=  cur-event=event  ^-  ?
        ?-  proxy.cur-event                     :: switch on proxy
          %own       (ownp-check cur-event)     :: can do sponsorship and spawn
          %manage    (managep-check cur-event)  :: can do sponsorship tx
          %spawn     (spawnp-check cur-event)   :: can do spawn tx
          %vote      %.n                        :: no L2 %vote proxy allowed
          %transfer  %.n                        :: cannot sponsor/spawn
        ==
        ++  ownp-check
          |=  cur-event=event  ^-  ?
          ?+  tx-type.cur-event  %.n            :: only sponsorship/spawn tx
            %spawn               %.y            :: can %spawn on L2
            %adopt               %.y            :: can %adopt on L2
            %reject              %.y            :: can %reject on L2
            %detach              %.y            :: can %detach on L2
            %set-spawn-proxy     %.y            :: can %set-spawn-proxy on L2
          ==
        ++  managep-check                       :: %configure-keys disallowed
          |=  cur-event=event  ^-  ?            :: for %spawn dominion
          ?+  tx-type.cur-event  %.n            :: only sponsorship actions
            %adopt               %.y            :: can %adopt on L2
            %reject              %.y            :: can %reject on L2
            %detach              %.y            :: can %detach on L2
          ==
        ++  spawnp-check
          |=  cur-event=event  ^-  ?
          ?+  tx-type.cur-event  %.n            :: only spawn tx allowed
            %spawn               %.y            :: can %spawn on L2
            %set-spawn-proxy     %.y            :: can %set-spawn-proxy on L2
          ==
        --                                      :: end +galaxy-check in %spawn
      ::
      --                                        :: end +spawnd-check
    ::
    ++  l2-check                                :: checks for %l2 dominion
      |^
      |=  cur-event=event  ^-  ?
      ?-  rank.cur-event                        :: switch on rank
        %galaxy  %.n                            :: no %l2 dominion galaxies
        %star    (star-check cur-event)         :: stars can be on %l2
        %planet  (planet-check cur-event)       :: planets can be on %l2
      ==
      ++  star-check
        |^
        |=  cur-event=event  ^-  ?
        ?-  proxy.cur-event                     :: switch on proxy
          %own       %.y                        :: all L2 tx allowed
          %manage    (managep-check cur-event)  :: %manage proxy tx allowed
          %spawn     (spawnp-check cur-event)   :: %spawn tx allowed
          %vote      %.n                        :: stars have no %vote proxy
          %transfer  (xferp-check cur-event)    :: %transfer proxy tx allowed
        ==
        ++  managep-check
          |=  cur-event=event  ^-  ?
          ?-  tx-type.cur-event                 :: switch on tx-type
            %configure-keys        %.y          :: permitted tx identical to L1
            %escape                %.y          :: management proxy permissions
            %cancel-escape         %.y
            %adopt                 %.y
            %reject                %.y
            %detach                %.y
            %set-management-proxy  %.y
            %set-spawn-proxy       %.n          :: disallowed events given
            %set-transfer-proxy    %.n          :: explicit cases to make it
            %transfer-point        %.n          :: more clear that this is the
            %spawn                 %.n          :: same as L1
          ==
        ++  spawnp-check
          |=  cur-event=event  ^-  ?
          ?+  tx-type.cur-event  %.n            :: permitted tx identical to L1
            %spawn               %.y            :: spawn proxy permissions
            %set-spawn-proxy     %.y
          ==
        ++  xferp-check
          |=  cur-event=event  ^-  ?            :: permitted tx identical to L1
          ?+  tx-type.cur-event  %.n            :: transfer proxy permissions
            %transfer-point      %.y
            %set-transfer-proxy  %.y
          ==
        --                                      :: end +star-check in %l2
      ++  planet-check                          :: checks for %l2 planets
        |^
        |=  cur-event=event  ^-  ?
        ?-  proxy.cur-event                     :: switch on proxy
          %own       (ownp-check cur-event)     :: permitted tx identical to L1
          %manage    (managep-check cur-event)  :: permitted tx identical to L1
          %spawn     %.n                        :: planets have no %spawn proxy
          %vote      %.n                        :: planets have no %vote proxy
          %transfer  (xferp-check cur-event)    :: permitted tx identical to L1
        ==
        ++  ownp-check
          |=  cur-event=event  ^-  ?
          ?-  tx-type.cur-event                 :: permitted tx identical to L1
            %transfer-point        %.y          :: ownership proxy permissions
            %configure-keys        %.y
            %set-management-proxy  %.y
            %set-transfer-proxy    %.y
            %escape                %.y
            %cancel-escape         %.y
            %spawn                 %.n
            %adopt                 %.n
            %reject                %.n
            %detach                %.n
            %set-spawn-proxy       %.n
          ==
        ++  managep-check
          |=  cur-event=event  ^-  ?
          ?+  tx-type.cur-event    %.n          :: permitted tx identical to L1
            %configure-keys        %.y          :: management proxy permissions
            %escape                %.y
            %cancel-escape         %.y
            %set-management-proxy  %.y
          ==
        ++  xferp-check
          |=  cur-event=event  ^-  ?            :: permitted tx identica to L1
          ?+  tx-type.cur-event  %.n            :: transfer proxy permissions
            %transfer-point      %.y
            %set-transfer-proxy  %.y
          ==
        ::
        --                                      :: end %l2 +planet-check
      ::
      --                                        :: end +l2-check
    ::
    --                                          :: end +make-success-map
  ::
  ::  creates a list of all values of $event for use by +test-rut
  ::
  ++  make-event-list  ^-  event-list
    =|  =event-list
    =+  rank-i=1
    |^
    ?:  (gth rank-i 3)
      (remove-wrong-dominion event-list)
    =+  owner-i=0
    |-
    ?.  (lte owner-i 1)
      ^$(rank-i +(rank-i))
    =+  nonce-i=0
    |-
    ?.  (lte nonce-i 1)
      ^$(owner-i +(owner-i))
    =+  dominion-i=1
    |-
    ?.  (lte dominion-i 3)
      ^$(nonce-i +(nonce-i))
    =+  proxy-i=1
    |-
    ?.  (lte proxy-i 5)
      ^$(dominion-i +(dominion-i))
    =+  tx-type-i=1
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
    --  :: end +make-event-list
  ::
  :: used to remove values of +make-event-list that have planets in the %spawn
  :: dominion or galaxies in the %l2 dominion
  ::
  :: TODO: Why does this not work when I put it inside the above trap?
  ::
  ++  remove-wrong-dominion
    |=  in=event-list  ^-  event-list
    =|  out=event-list
    |-
    ?~  in  out
    ?:  ?&  =(rank.i.in %galaxy)
            =(dominion.i.in %l2)
        ==
        $(in t.in)
    ?:  ?&  =(rank.i.in %planet)
            =(dominion.i.in %spawn)
        ==
        $(in t.in)
    %=  $
      in  t.in
      out  i.in^out
    ==
  ::
  ::  jar of events for +test-rut. each @p is mapped to a list of events
  ::  it ought to attempt. This is done according to rank.
  ::
  ::  This gate may be modified to filter out a subset of events you wish to
  ::  test by commenting out or modifying parts of the filter gate. Remember that
  ::  +test-rut is only designed to test %spawn, %transfer-point,
  ::  %configure-keys, %set-management-proxy, %set-spawn-proxy, and
  ::  %set-transfer-proxy. Adding in other transaction types will result in a
  ::  crash.
  ::
  ++  gen-rut-jar
    ^-  (jar @p event)
    =/  filter  ;:  cork
                    ::(cury filter-owner %.y)
                    ::(cury filter-proxy %spawn)
                    ::(cury filter-nonce %.y)
                    ::(cury filter-rank %galaxy)
                    ::(cury filter-dominion %l1)
                    %-  cury
                    :-  filter-tx-type
                    :*  %spawn
                        %transfer-point
                        %configure-keys
                        %set-management-proxy
                        %set-spawn-proxy
                        %set-transfer-proxy
                        ~
                    ==
                ==
    =/  filtered-events  (filter make-event-list)
    =|  rut-jar=(jar @p event)
    |^
    ?~  filtered-events  rut-jar
    =/  current-event  i.filtered-events
    ?:  =(rank.current-event %galaxy)
      ?+  dominion.current-event  !!
        %l1     (list-in-jar (ly ~[~rut]) current-event)
        %spawn  (list-in-jar (ly ~[~tyl]) current-event)
      ==
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
                    :*  ~larsyx-mapmeg
                        ~rabsum-ravtyd
                        ~radres-tinnyl
                        ~
                    ==
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
    ::  adds event to the list associated to each value of ships in rut-jar
    ::
    ++  list-in-jar
      |=  [ships=(list ship) =event]
      ^+  rut-jar
      |-
      ?~  ships
        ^^$(filtered-events +.filtered-events)
      =.  rut-jar  (~(add ja rut-jar) i.ships event)
      $(ships t.ships)
  --  :: end +gen-rut-jar
  ::
  ++  filter-tx-type
    |=  [typs=(list =tx-type) =event-list]
    %+  skim
      event-list
    |=(=event (lien typs |=(=tx-type =(tx-type tx-type.event))))
  ::
  ++  filter-proxy
    |=  [=proxy:naive =event-list]
    (skim event-list |=(=event =(proxy.event proxy)))
  ::
  ++  filter-rank
    |=  [=rank =event-list]
    (skim event-list |=(=event =(rank.event rank)))
  ::
  ++  filter-owner
    |=  [owner=? =event-list]
    (skim event-list |=(=event =(owner.event owner)))
  ::
  ++  filter-nonce
    |=  [nonce=? =event-list]
    (skim event-list |=(=event =(nonce.event nonce)))
  ::
  ++  filter-dominion
    |=  [=dominion:naive =event-list]
    (skim event-list |=(=event =(dominion.event dominion)))
  ::
  :: Takes in a list of full-tx and turns them into a batch=@ to be submitted to
  :: +n. The ordering on the list is the order in which the transactions are
  :: processed.
  ::
  ++  tx-list-to-batch
  |=  =tx-list  ^-  @
  (can 3 (turn tx-list gen-tx))
  ::
  --  :: end +l2-event-gen
::
:: This core generates L1 transaction logs. It is important to keep in mind
:: that Azimuth already handles the logic on what is or is not allowed on L1,
:: so if you tell naive.hoon that ~zod adopted ~hodler without ~hodler having
:: escaped to ~zod first, ~zod will be the new sponsor of ~hodler anyways. In
:: the real world, such an action would have been blocked by the smart contract
:: and never made its way to naive.hoon.
::
++  l1
  |%
  :: This gate takes in raw information for L1 transactions and formats them
  :: appropriately for use with +n.
  ::
  ++  log
    |=  [log-name=@ux data=@ux topics=(list @)]
    ^-  ^input:naive
    [%log *@ux data log-name topics]
  ::
  ++  owner-changed
    |=  [=ship =address]
    (log owner-changed:log-names:naive *@ux ship address ~)
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
  ++  changed-dns
    |=  [data=@]
    (log changed-dns:log-names:naive data ~)
  ::
  ++  approval-for-all
    |=  [owner=address operator=address approved=@]
    (log approval-for-all:log-names:naive approved owner operator ~)
  ::
  --  :: end +l1
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
                       ~tyl
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
                          [~tyl %tyl-key-0]
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
                             [~tyl %tyl-mkey-0]
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
                        :*  [~rut %rut-skey-0]
                            [~tyl %tyl-skey-0]
                            [~holrut %holrut-skey-0]
                            [~losrut %losrut-skey-1]
                            [~rigrut %rigrut-skey-0]
                            ~
                        ==
::
++  default-xfer-keys  %-  my:nl
                       :*  [~rut %rut-tkey-0]
                           [~tyl %tyl-tkey-0]
                           [~rigrut %rigrut-tkey-0]
                           [~larsyx-mapmeg %lm-tkey-0]
                           [~holrut %holrut-tkey-0]
                           [~rabsum-ravtyd %rr-tkey-0]
                           [~radres-tinnyl %rt-tkey-0]
                           [~losrut %losrut-tkey-0]
                           [~dovmul-mogryt %dm-tkey-0]
                           [~pinpun-pilsun %pp-tkey-0]
                           [~pidted-dacnum %pd-tkey-0]
                           [~habtyc-nibpyx %hn-tkey-0]
                           [~disryt-nolpet %dn-tkey-0]
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
=/  init-rut-simple  (init-rut-full *^state:naive)
=/  init-red-simple  (init-red-full *^state:naive)
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
  =^  f  state  init-rut-simple
  =/  initial-state  state
  =/  ship-list  rut-ship-list
  =/  suc-map  (make-success-map make-event-list)
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
  %+  category  (weld "success map " (scow %f (~(got by suc-map) cur-event)))
  ::
  =/  cur-point  (got:orm points.initial-state cur-ship)
  =*  own  own.cur-point
  =/  cur-nonce
    ?-  proxy.cur-event
      %own       nonce.owner.own
      %spawn     nonce.spawn-proxy.own
      %manage    nonce.management-proxy.own
      %vote      nonce.voting-proxy.own
      %transfer  nonce.transfer-proxy.own
    ==
    =/  new-nonce  ?:  &(nonce.cur-event owner.cur-event)
                   ?-  proxy.cur-event
                     ?(%own %manage)  +(cur-nonce)
                     %spawn           ?-  rank.cur-event
                                        %galaxy  ?-  dominion.cur-event
                                                   ?(%l1 %spawn)  +(cur-nonce)
                                                   %l2            cur-nonce
                                                 ==
                                        %star    ?-  dominion.cur-event
                                                   %l1            cur-nonce
                                                   ?(%spawn %l2)  +(cur-nonce)
                                                 ==
                                        %planet  cur-nonce
                                      ==
                     %transfer        ?~  address.transfer-proxy.own
                                        cur-nonce
                                      +(cur-nonce)
                     %vote            cur-nonce
                   ==
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
    ==
    ::
    ++  set-keys  ^-  ^state:naive
      =/  new-keys
      %=  cur-point
        life.keys.net    +(life.keys.net:(got:orm points.initial-state cur-ship))
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
        address.transfer-proxy.own  0x0
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
        points  (put:orm points.expect-state ship spawned)
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
        points  (put:orm points.expect-state cur-ship updated-point)
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
          %own       (~(got by default-own-keys) cur-ship)
          %manage    (~(got by default-manage-keys) cur-ship)
          %spawn     ?:  |(=(rank.cur-event %galaxy) =(rank.cur-event %star))
                       (~(got by default-spawn-keys) cur-ship)
                     %wrong-key
          %transfer  (~(got by default-xfer-keys) cur-ship)
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
      %~tyl            ~hastyl
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
  --  :: end of +expect-eq
::
::  the following are sponsorship tests. they ought to eventually be consolidated
::  into one large test, but for now it will be easier to tell which one is failing
::  by splitting them up
::
::  the following are L2 sponsorship tests. the syntax is test-galaxy-X-Y-action. X is the
::  layer of the sponsee, Y is the layer of the sponsor.
::
::  Each row of the following table has one or more tests that cover it.
::  The corresponding row is listed as a comment in that test. Thus
::  you can grep for the line and fine the appropriate test. A few of
::  the tests cannot be performed here - there are the ones marked by !!
::  but we include what the tests would look like anyways as a comment
::
::  * on the left means all possible states, on the right it means no change.
::  !! means that case can never happen per L1 contract
::  L1-cancel can be triggered by "cancel escape" by the child or "reject" by the sponsor
::  A1 and A2 are arbitrary but distinct ships one class above the main ship
::  Event        | E_1 | E_2 | S_1 | S_2 | -> | E_1 | E_2 | S_1 | S_2
::  L1-escape A1 | *   | *   | *   | *   | -> | A1  | A1  | *   | *
::  L1-cancel A1 | ~   | *   | *   | *   | -> !! :: no cancel if not escaping
::  L1-cancel A1 | A1  | *   | *   | *   | -> | ~   | ~   | *   | *
::  L1-adopt  A1 | A1  | *   | *   | *   | -> | ~   | ~   | A1  | A2
::  L1-adopt  A1 | ~   | *   | *   | *   | -> !! :: no adopt if not escaping
::  L1-adopt  A1 | A2  | *   | *   | *   | -> !! :: no adopt if not escaping
::  L1-detach A1 | *   | *   | A1  | A1  | -> | *   | *   | ~   | ~
::  L1-detach A1 | *   | *   | A1  | A2  | -> | *   | *   | ~   | A2
::  L1-detach A1 | *   | *   | A1  | ~   | -> | *   | *   | ~   | ~
::  L2-escape A1 | *   | *   | *   | *   | -> | *   | A1  | *   | *
::  L2-cancel A1 | *   | *   | *   | *   | -> | *   | ~   | *   | *
::  L2-adopt  A1 | *   | A1  | *   | *   | -> | *   | ~   | *   | A1
::  L2-adopt  A1 | *   | A2  | *   | *   | -> | *   | A2  | *   | *
::  L2-adopt  A1 | *   | ~   | *   | *   | -> | *   | ~   | *   | *
::  L2-reject A1 | *   | A1  | *   | *   | -> | *   | ~   | *   | *
::  L2-reject A1 | *   | A2  | *   | *   | -> | *   | A2  | *   | *
::  L2-reject A1 | *   | ~   | *   | *   | -> | *   | ~   | *   | *
::  L2-detach A1 | *   | *   | *   | A1  | -> | *   | *   | *   | ~
::  L2-detach A1 | *   | *   | *   | A2  | -> | *   | *   | *   | A2
::  L2-detach A1 | *   | *   | *   | ~   | -> | *   | *   | *   | ~
::
::  the following test L1 escape actions
::
++  test-rut-l1-l1-escape-l1  ^-  tang
::  L1-escape A1 | *   | *   | *   | *   | -> | A1  | A1  | *   | *
  %+  expect-eq
    !>  [[~ ~rigred] %.y ~holrut]
  ::
    !>
    =|  =^state:naive
    =^  f  state  init-rut-simple
    =^  f  state  (n state (escape-requested:l1 ~rabsum-ravtyd ~rigred))
    [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
::
++  test-rut-l1-l2-escape-l1  ^-  tang
  ::  L1-escape A1 | *   | *   | *   | *   | -> | A1  | A1  | *   | *
  :: An L1 ship can L1 escape to an L2 ship, but the L2 ship must
  :: adopt on L2
  %+  expect-eq
    !>  [[~ ~losred] %.y ~rigrut]
  ::
    !>
    =|  =^state:naive
    =^  f  state  init-rut-simple
    =^  f  state  (n state (escape-requested:l1 ~larsyx-mapmeg ~losred))
    [escape.net sponsor.net]:(got:orm points.state ~larsyx-mapmeg)
::
++  test-rut-l2-lx-escape-l1  ^-  tang
  ::  L2 ships can't escape on L1
  ;:  weld
    ::  escaping to L1 ship
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-rut-simple
      =^  f  state  (n state (escape-requested:l1 ~dovmul-mogryt ~rigred))
      [escape.net sponsor.net]:(got:orm points.state ~dovmul-mogryt)
    ::
    ::  escaping to L2 ship
    %+  expect-eq
      !>  [~ %.y ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-rut-simple
      =^  f  state  (n state (escape-requested:l1 ~pinpun-pilsun ~losred))
      [escape.net sponsor.net]:(got:orm points.state ~pinpun-pilsun)
  ==
::
++  test-red-l2-l2-adopt-l2-1  ^-  tang
  ::  L2-adopt  A1 | *   | A1  | *   | *   | -> | *   | ~   | *   | A1
  =/  pp-adopt  [losred-own %adopt ~pinpun-pilsun]
  =/  pp-m-adopt  [losred-mgmt %adopt ~pinpun-pilsun]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~losred]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 pp-adopt %losred-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~pinpun-pilsun)
    ::
    %+  expect-eq
      !>  [~ %.y ~losred]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 pp-m-adopt %losred-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~pinpun-pilsun)
  ==
::
++  test-red-l1-l2-adopt-l2-1
  ::  L2-adopt  A1 | *   | A1  | *   | *   | -> | *   | ~   | *   | A1
  =/  lm-adopt  [losred-own %adopt ~larsyx-mapmeg]
  =/  lm-m-adopt  [losred-mgmt %adopt ~larsyx-mapmeg]
  ::
  ;:  weld
  %+  expect-eq
    !>  [~ %.y ~losred]
  ::
    !>
    =|  =^state:naive
    =^  f  state  init-red-simple
    =^  f  state  (n state %bat q:(gen-tx 0 lm-adopt %losred-key-0))
    [escape.net sponsor.net]:(got:orm points.state ~larsyx-mapmeg)
  ::
  %+  expect-eq
    !>  [~ %.y ~losred]
  ::
    !>
    =|  =^state:naive
    =^  f  state  init-red-simple
    =^  f  state  (n state %bat q:(gen-tx 0 lm-m-adopt %losred-mkey-0))
    [escape.net sponsor.net]:(got:orm points.state ~larsyx-mapmeg)
  ==
::
++  test-red-l2-l1-adopt-l2-1
  ::  L2-adopt  A1 | *   | A1  | *   | *   | -> | *   | ~   | *   | A1
  =/  dm-adopt  [rigred-own %adopt ~dovmul-mogryt]
  =/  dm-m-adopt  [rigred-mgmt %adopt ~dovmul-mogryt]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~rigred]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 dm-adopt %rigred-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~dovmul-mogryt)
    ::
    %+  expect-eq
      !>  [~ %.y ~rigred]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 dm-m-adopt %rigred-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~dovmul-mogryt)
  ==
::
++  test-red-l1-l1-adopt-l2-1
  ::  L2-adopt  A1 | *   | A1  | *   | *   | -> | *   | ~   | *   | A1
  =/  rr-adopt  [rigred-own %adopt ~rabsum-ravtyd]
  =/  rr-m-adopt  [rigred-mgmt %adopt ~rabsum-ravtyd]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~rigred]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 rr-adopt %rigred-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
    ::
    %+  expect-eq
      !>  [~ %.y ~rigred]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 rr-m-adopt %rigred-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
  ==
::
++  test-red-l1-l1-adopt-l2-2
  ::  L2-adopt  A1 | *   | A2  | *   | *   | -> | *   | A2  | *   | *
  =/  rr-adopt  [losred-own %adopt ~rabsum-ravtyd]
  =/  rr-m-adopt  [losred-mgmt %adopt ~rabsum-ravtyd]
  ::
  ;:  weld
    %+  expect-eq
      !>  [[~ ~rigred] %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 rr-adopt %losred-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
    ::
    %+  expect-eq
      !>  [[~ ~rigred] %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 rr-m-adopt %losred-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
  ==
::
++  test-rut-l1-l1-adopt-l2-3  ^-  tang
  ::  L2-adopt  A1 | *   | ~   | *   | *   | -> | *   | ~   | *   | *
  ::
  =/  rr-h-detach  [1 [holrut-own %detach ~rabsum-ravtyd] %holrut-key-0]
  =/  rr-h-m-detach  [0 [holrut-mgmt %detach ~rabsum-ravtyd] %holrut-mkey-0]
  =/  rr-adopt  [0 [losred-own %adopt ~rabsum-ravtyd] %losred-key-0]
  =/  rr-m-adopt  [0 [losred-mgmt %adopt ~rabsum-ravtyd] %losred-mkey-0]
  ::
  =,  l2-event-gen
  =/  rr-batch-1=tx-list  (limo ~[rr-h-detach rr-adopt])
  =/  rr-batch-2=tx-list  (limo ~[rr-h-m-detach rr-m-adopt])
  ::
  =/  init-state=^state:naive  +:init-rut-simple
  ;:  weld
    %+  expect-eq
      !>  [~ %.n ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (n init-state %bat q:(gen-tx rr-h-detach))
      =^  f  state  (n state %bat q:(gen-tx rr-adopt))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
    ::
    %+  expect-eq
      !>  [~ %.n ~holrut]

      !>
      =|  =^state:naive
      =^  f  state  (n init-state %bat (tx-list-to-batch rr-batch-1))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
   ::
    %+  expect-eq
      !>  [~ %.n ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (n init-state %bat q:(gen-tx rr-h-m-detach))
      =^  f  state  (n state %bat q:(gen-tx rr-m-adopt))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
    ::
    %+  expect-eq
      !>  [~ %.n ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (n init-state %bat (tx-list-to-batch rr-batch-2))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
  ==
::
::  the following tests L2 %rejects
++  test-red-l2-l2-reject-l2-1  ^-  tang
  ::  L2-reject A1 | *   | A1  | *   | *   | -> | *   | ~   | *   | *
  =/  pp-reject  [losred-own %reject ~pinpun-pilsun]
  =/  pp-m-reject  [losred-mgmt %reject ~pinpun-pilsun]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 pp-reject %losred-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~pinpun-pilsun)
    ::
    %+  expect-eq
      !>  [~ %.y ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 pp-m-reject %losred-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~pinpun-pilsun)
  ==
::
++  test-red-l2-l1-reject-l2-1  ^-  tang
  ::  L2-reject A1 | *   | A1  | *   | *   | -> | *   | ~   | *   | *
  =/  dm-reject  [rigred-own %reject ~dovmul-mogryt]
  =/  dm-m-reject  [rigred-mgmt %reject ~dovmul-mogryt]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 dm-reject %rigred-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~dovmul-mogryt)
    ::
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 dm-m-reject %rigred-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~dovmul-mogryt)
  ==
::
++  test-red-l1-l2-reject-l2-1  ^-  tang
  ::  L2-reject A1 | *   | A1  | *   | *   | -> | *   | ~   | *   | *
  =/  lm-reject  [losred-own %reject ~larsyx-mapmeg]
  =/  lm-m-reject  [losred-mgmt %reject ~larsyx-mapmeg]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~rigrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 lm-reject %losred-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~larsyx-mapmeg)
    ::
    %+  expect-eq
      !>  [~ %.y ~rigrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 lm-m-reject %losred-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~larsyx-mapmeg)
  ==
::
++  test-red-l1-l1-reject-l2-1  ^-  tang
  ::  L2-reject A1 | *   | A1  | *   | *   | -> | *   | ~   | *   | *
  =/  rr-reject  [rigred-own %reject ~rabsum-ravtyd]
  =/  rr-m-reject  [rigred-mgmt %reject ~rabsum-ravtyd]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 rr-reject %rigred-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
    ::
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 rr-m-reject %rigred-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
  ==
++  test-red-l2-l2-reject-l2-2  ^-  tang
  ::  L2-reject A1 | *   | A2  | *   | *   | -> | *   | A2  | *   | *
  =/  pp-reject  [losrut-own %reject ~pinpun-pilsun]
  =/  pp-m-reject  [losrut-mgmt %reject ~pinpun-pilsun]
  ::
  ;:  weld
    %+  expect-eq
      !>  [[~ ~losred] %.y ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 2 pp-reject %losrut-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~pinpun-pilsun)
    ::
    %+  expect-eq
      !>  [[~ ~losred] %.y ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 pp-m-reject %losrut-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~pinpun-pilsun)
  ==
::
++  test-red-l2-l1-reject-l2-2  ^-  tang
  ::  L2-reject A1 | *   | A2  | *   | *   | -> | *   | A2  | *   | *
  =/  dm-reject  [holrut-own %reject ~dovmul-mogryt]
  =/  dm-m-reject  [holrut-mgmt %reject ~dovmul-mogryt]
  ::
  ;:  weld
    %+  expect-eq
      !>  [[~ ~rigred] %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 1 dm-reject %holrut-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~dovmul-mogryt)
    ::
    %+  expect-eq
      !>  [[~ ~rigred] %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 dm-m-reject %holrut-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~dovmul-mogryt)
  ==
::
++  test-red-l1-l2-reject-l2-2  ^-  tang
  ::  L2-reject A1 | *   | A2  | *   | *   | -> | *   | A2  | *   | *
  =/  lm-reject  [rigrut-own %reject ~larsyx-mapmeg]
  =/  lm-m-reject  [rigrut-mgmt %reject ~larsyx-mapmeg]
  ::
  ;:  weld
    %+  expect-eq
      !>  [[~ ~losred] %.y ~rigrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 lm-reject %rigrut-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~larsyx-mapmeg)
    ::
    %+  expect-eq
      !>  [[~ ~losred] %.y ~rigrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 lm-m-reject %rigrut-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~larsyx-mapmeg)
  ==
::
++  test-red-l1-l1-reject-l2-2  ^-  tang
  ::  L2-reject A1 | *   | A2  | *   | *   | -> | *   | A2  | *   | *
  =/  rr-reject  [holrut-own %reject ~rabsum-ravtyd]
  =/  rr-m-reject  [holrut-mgmt %reject ~rabsum-ravtyd]
  ::
  ;:  weld
    %+  expect-eq
      !>  [[~ ~rigred] %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 1 rr-reject %holrut-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
    ::
    %+  expect-eq
      !>  [[~ ~rigred] %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 rr-m-reject %holrut-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
  ==
::
++  test-red-l1-l1-reject-l2-3  ^-  tang
  ::  L2-reject A1 | *   | ~   | *   | *   | -> | *   | ~   | *   | *
  =/  rt-reject  [holrut-own %reject ~radres-tinnyl]
  =/  rt-m-reject  [holrut-mgmt %reject ~radres-tinnyl]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 1 rt-reject %holrut-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~radres-tinnyl)
    ::
    %+  expect-eq
      !>  [~ %.y ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 rt-m-reject %holrut-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~radres-tinnyl)
  ==
::
::  the following tests L2 %cancel-escape
::
++  test-red-l2-l2-cancel-escape-l2  ^-  tang
  ::  L2-cancel A1 | *   | *   | *   | *   | -> | *   | ~   | *   | *
  =/  pp-cancel-escape  [[~pinpun-pilsun %own] %cancel-escape ~losred]
  =/  pp-m-cancel-escape  [[~pinpun-pilsun %manage] %cancel-escape ~losred]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 3 pp-cancel-escape %losrut-pp-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~pinpun-pilsun)
    ::
    %+  expect-eq
      !>  [~ %.y ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 pp-m-cancel-escape %losrut-pp-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~pinpun-pilsun)
  ==
::
++  test-red-l2-l1-cancel-escape-l2  ^-  tang
  ::  L2-cancel A1 | *   | *   | *   | *   | -> | *   | ~   | *   | *
  =/  dm-cancel-escape  [[~dovmul-mogryt %own] %cancel-escape ~rigred]
  =/  dm-m-cancel-escape  [[~dovmul-mogryt %manage] %cancel-escape ~rigred]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 3 dm-cancel-escape %holrut-dm-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~dovmul-mogryt)
    ::
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 dm-m-cancel-escape %holrut-dm-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~dovmul-mogryt)
  ==
::
++  test-red-l1-l2-cancel-escape-l2  ^-  tang
  ::  L2-cancel A1 | *   | *   | *   | *   | -> | *   | ~   | *   | *
  =/  lm-cancel-escape  [[~larsyx-mapmeg %own] %cancel-escape ~losred]
  =/  lm-m-cancel-escape  [[~larsyx-mapmeg %manage] %cancel-escape ~losred]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~rigrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 1 lm-cancel-escape %rigrut-lm-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~larsyx-mapmeg)
    ::
    %+  expect-eq
      !>  [~ %.y ~rigrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 lm-m-cancel-escape %rigrut-lm-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~larsyx-mapmeg)
  ==
::
++  test-red-l1-l1-cancel-escape-l2  ^-  tang
  ::  L2-cancel A1 | *   | *   | *   | *   | -> | *   | ~   | *   | *
  =/  rr-cancel-escape  [[~rabsum-ravtyd %own] %cancel-escape ~rigred]
  =/  rr-m-cancel-escape  [[~rabsum-ravtyd %manage] %cancel-escape ~rigred]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 1 rr-cancel-escape %holrut-rr-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
    ::
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-red-simple
      =^  f  state  (n state %bat q:(gen-tx 0 rr-m-cancel-escape %holrut-rr-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
  ==
::
::  the following tests L2 %detach. the format test-rut-X-Y-detach means
::  X is the layer of the sponsor, Y is the layer of the sponsee
::
++  test-rut-l2-l2-detach-l2-1  ^-  tang
  ::  L2-detach A1 | *   | *   | *   | A1  | -> | *   | *   | *   | ~
  =/  pp-detach  [losrut-own %detach ~pinpun-pilsun]
  =/  pp-m-detach  [losrut-mgmt %detach ~pinpun-pilsun]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.n ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-rut-simple
      =^  f  state  (n state %bat q:(gen-tx 3 pp-detach %losrut-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~pinpun-pilsun)
    ::
    %+  expect-eq
      !>  [~ %.n ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-rut-simple
      =^  f  state  (n state %bat q:(gen-tx 0 pp-m-detach %losrut-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~pinpun-pilsun)
  ==
::
++  test-rut-l2-l1-detach-l2-1  ^-  tang
  ::  L2-detach A1 | *   | *   | *   | A1  | -> | *   | *   | *   | ~
  =/  rt-detach  [losrut-own %detach ~radres-tinnyl]
  =/  rt-m-detach  [losrut-mgmt %detach ~radres-tinnyl]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.n ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-rut-simple
      =^  f  state  (n state %bat q:(gen-tx 3 rt-detach %losrut-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~radres-tinnyl)
    ::
    %+  expect-eq
      !>  [~ %.n ~losrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-rut-simple
      =^  f  state  (n state %bat q:(gen-tx 0 rt-m-detach %losrut-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~radres-tinnyl)
  ==
::
++  test-rut-l1-l2-detach-l2-1  ^-  tang
  ::  L2-detach A1 | *   | *   | *   | A1  | -> | *   | *   | *   | ~
  =/  dm-detach  [holrut-own %detach ~dovmul-mogryt]
  =/  dm-m-detach  [holrut-mgmt %detach ~dovmul-mogryt]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.n ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-rut-simple
      =^  f  state  (n state %bat q:(gen-tx 1 dm-detach %holrut-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~dovmul-mogryt)
    ::
    %+  expect-eq
      !>  [~ %.n ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-rut-simple
      =^  f  state  (n state %bat q:(gen-tx 0 dm-m-detach %holrut-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~dovmul-mogryt)
  ==
::
++  test-rut-l1-l1-detach-l2-1  ^-  tang
  ::  L2-detach A1 | *   | *   | *   | A1  | -> | *   | *   | *   | ~
  =/  lm-detach  [rigrut-own %detach ~larsyx-mapmeg]
  =/  lm-m-detach  [rigrut-mgmt %detach ~larsyx-mapmeg]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.n ~rigrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-rut-simple
      =^  f  state  (n state %bat q:(gen-tx 0 lm-detach %rigrut-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~larsyx-mapmeg)
    ::
    %+  expect-eq
      !>  [~ %.n ~rigrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-rut-simple
      =^  f  state  (n state %bat q:(gen-tx 0 lm-m-detach %rigrut-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~larsyx-mapmeg)
  ==
::
++  test-rut-l1-l1-detach-l2-2  ^-  tang
  ::  L2-detach A1 | *   | *   | *   | A2  | -> | *   | *   | *   | A2
  ::  makes sure that you cannot detach someone who your arent sponsoring
  ::
  =/  rr-detach  [rigrut-own %detach ~rabsum-ravtyd]
  =/  rr-m-detach  [rigrut-mgmt %detach ~rabsum-ravtyd]
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-rut-simple
      =^  f  state  (n state %bat q:(gen-tx 0 rr-detach %rigrut-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
    ::
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  init-rut-simple
      =^  f  state  (n state %bat q:(gen-tx 0 rr-m-detach %rigrut-mkey-0))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
  ==
::
++  test-rut-l1-l1-detach-l2-3  ^-  tang
  ::  L2-detach A1 | *   | *   | *   | ~   | -> | *   | *   | *   | ~
  ::  makes sure detach on someone without a sponsor is a no-op
  ::
  =/  rr-h-detach  [1 [holrut-own %detach ~rabsum-ravtyd] %holrut-key-0]
  =/  rr-h-m-detach  [0 [holrut-mgmt %detach ~rabsum-ravtyd] %holrut-mkey-0]
  =/  rr-detach  [0 [rigrut-own %detach ~rabsum-ravtyd] %rigrut-key-0]
  =/  rr-m-detach  [0 [rigrut-mgmt %detach ~rabsum-ravtyd] %rigrut-mkey-0]
  ::
  =,  l2-event-gen
  =/  rr-detach-batch-1=tx-list  (limo rr-h-detach rr-detach ~)
  =/  rr-detach-batch-2=tx-list  (limo rr-h-m-detach rr-m-detach ~)
  ::
  =/  init-state=^state:naive  +:init-rut-simple
  ;:  weld
    %+  expect-eq
      !>  [~ %.n ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (n init-state %bat q:(gen-tx rr-h-detach))
      =^  f  state  (n state %bat q:(gen-tx rr-detach))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
    ::
    %+  expect-eq
      !>  [~ %.n ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (n init-state %bat (tx-list-to-batch rr-detach-batch-1))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
    ::
    %+  expect-eq
      !>  [~ %.n ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (n init-state %bat q:(gen-tx rr-h-m-detach))
      =^  f  state  (n state %bat q:(gen-tx rr-m-detach))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
    ::
    %+  expect-eq
      !>  [~ %.n ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (n init-state %bat (tx-list-to-batch rr-detach-batch-2))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)

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
    =^  f  state  init-red-simple
    =^  f  state  (n state (escape-requested:l1 ~rabsum-ravtyd ~rigred))
    =^  f  state  (n state %bat q:(gen-tx 0 rr-adopt %rigred-key-0))
    [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
::
::  The following test fails but only because ecliptic.sol only allows
::  an adopt when its valid to do so.
::
::  ++  test-red-l2-escape-l1-adopt  ^-  tang
::    ::  shouldn't be possible to accept a L2 escape with a L1 adopt
::    %+  expect-eq
::      !>  [[~ ~rigred] %.y ~holrut]
::    ::
::      !>
::      =|  =^state:naive
::      =^  f  state  init-red-simple
::      =^  f  state  (n state (escape-accepted:l1 ~rabsum-ravtyd ~rigred))
::      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
::
++  test-rut-l1-adoption-on-l2-wrong-key-or-nonce
  =/  rr-escape  [[~rabsum-ravtyd %own] %escape ~rigred]
  =/  rr-adopt   [rigred-own %adopt ~rabsum-ravtyd]
  ::
  =/  init-state  +:init-rut-simple
  ::
  ;:  weld
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (n init-state %bat q:(gen-tx 1 rr-escape %wrong-key))
      =^  f  state  (n state %bat q:(gen-tx 0 rr-adopt %rigred-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
    ::
    %+  expect-eq
      !>  [~ %.y ~holrut]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (n init-state %bat q:(gen-tx 999 rr-escape %holrut-rr-key-0))
      =^  f  state  (n state %bat q:(gen-tx 0 rr-adopt %rigred-key-0))
      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
  ==
::
++  test-own-sponsor-l2-escape
  ::  You can't escape to your own sponsor on L1, so you shouldn't be able
  ::  to on L2 either
  ::
  =/  rr-escape  [[~rabsum-ravtyd %own] %escape ~holrut]
  ::
  %+  expect-eq
    !>  [[~ ~holrut] %.y ~holrut]
  ::
    !>
    =|  =^state:naive
    =^  f  state  init-rut-simple
    =^  f  state  (n state %bat q:(gen-tx 0 rr-escape %holrut-rr-key-0))
    [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
::
++  test-rut-l1-detach-1
  ::  L1-detach A1 | *   | *   | A1  | A1  | -> | *   | *   | ~   | ~
  ::  this checks that if you have the same sponsor on L1 and L2, then
  ::  a L1 detach makes you lose both
  ::
  ::  ~rabsum-ravtyd is a L1 planet under a L1 star so would theortically
  ::  already be sponsored by ~holrut on L1. this already appears in
  ::  the L2 state as being sponsored by ~holrut, but we will go through
  ::  with adopting ~rabsum-ravtyd on L2 anyways before the L1 detach
  ::
  ::  TODO: escaping to your own sponsor shouldn't be allowed since it isn't
  ::  on L1, so I should remove the lines that have RR escaping to its
  ::  own sponsor
  ::
  =/  rr-escape  [[~rabsum-ravtyd %own] %escape ~holrut]
  =/  rr-adopt   [holrut-own %adopt ~rabsum-ravtyd]
  %+  expect-eq
    !>  [~ %.n ~holrut]
  ::
    !>
    =|  =^state:naive
    =^  f  state  init-rut-simple
    =^  f  state  (n state %bat q:(gen-tx 0 rr-escape %holrut-rr-key-0))
    =^  f  state  (n state %bat q:(gen-tx 1 rr-adopt %holrut-key-0))
    =^  f  state  (n state (lost-sponsor:l1 ~rabsum-ravtyd ~holrut))
    [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
::
++  test-red-l1-detach-2
  ::  this takes a L1 planet with L1 sponsor that acquires a L2 sponsor
  ::  and is then detached by their L1 sponsor
  ::
  ::  L1-detach A1 | *   | *   | A1  | A2  | -> | *   | *   | ~   | A2
  ::
  =/  lm-adopt  [losred-own %adopt ~larsyx-mapmeg]
  ::
  %+  expect-eq
    !>  [~ %.y ~losred]
  ::
    !>
    =|  =^state:naive
    =^  f  state  init-red-simple
    =^  f  state  (n state %bat q:(gen-tx 0 lm-adopt %losred-key-0))
    =^  f  state  (n state (lost-sponsor:l1 ~larsyx-mapmeg ~rigrut))
   [escape.net sponsor.net]:(got:orm points.state ~larsyx-mapmeg)
::
++  test-rut-l1-detach-3
  ::  L1-detach A1 | *   | *   | A1  | ~   | -> | *   | *   | ~   | ~
  ::  Since we don't see L1 state explicitly, we can't really test
  ::  this transition here. But I've included it for completeness sake
  =/  rr-detach  [holrut-own %detach ~rabsum-ravtyd]
  ::
  %+  expect-eq
    !>  [~ %.n ~holrut]
  ::
    !>
    =|  =^state:naive
    =^  f  state  init-rut-simple
    =^  f  state  (n state %bat q:(gen-tx 1 rr-detach %holrut-key-0))
    =^  f  state  (n state (escape-requested:l1 ~rabsum-ravtyd ~holrut))
    =^  f  state  (n state (escape-accepted:l1 ~rabsum-ravtyd ~holrut))
    =^  f  state  (n state (lost-sponsor:l1 ~rabsum-ravtyd ~holrut))
    [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
::
::  This test is commented since ecliptic.sol will not allow a cancel
::  if not escaping, so this row of the table cannot be tested here.
::  ++  test-rut-l1-cancel-1
::    ::  L1-cancel A1 | ~   | *   | *   | *   | -> !! :: no cancel if not escaping
::    ::  Note we're using ~rut so there are no initial escapes
::    ::
::    %+  expect-eq
::      !>  ~
::    ::
::      !>
::      =|  =^state:naive
::      =^  f  state  init-rut-simple
::      =^  f  state  (n state (escape-canceled:l1 ~rabsum-ravtyd ~rigred))
::      escape.net:(got:orm points.state ~rabsum-ravtyd)
::
++  test-rut-l1-cancel-2
  ::  L1-cancel A1 | A1  | *   | *   | *   | -> | ~   | ~   | *   | *
  %+  expect-eq
    !>  ~
  ::
    !>
    =|  =^state:naive
    =^  f  state  init-rut-simple
    =^  f  state  (n state (escape-requested:l1 ~rabsum-ravtyd ~rigred))
    =^  f  state  (n state (escape-canceled:l1 ~rabsum-ravtyd ~rigred))
    escape.net:(got:orm points.state ~rabsum-ravtyd)
::
++  test-rut-l1-adopt-1
  ::  L1-adopt  A1 | A1  | *   | *   | *   | -> | ~   | ~   | A1  | A2
  %+  expect-eq
    !>  [~ %.y ~rigred]
  ::
    !>
    =|  =^state:naive
    =^  f  state  init-rut-simple
    =^  f  state  (n state (escape-requested:l1 ~rabsum-ravtyd ~rigred))
    =^  f  state  (n state (escape-accepted:l1 ~rabsum-ravtyd ~rigred))
    [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
::
::  These commented out tests fail, but it is because L1 adopt is only
::  accepted if the ecliptic.sol allows it. So these rows of the table
::  cannot be tested here.
::
::  ++  test-rut-l1-adopt-2
::    ::  L1-adopt  A1 | ~   | *   | *   | *   | -> !! :: no adopt if not escaping
::    %+  expect-eq
::      !>  [~ %.y ~holrut]
::    ::
::      !>
::      =|  =^state:naive
::      =^  f  state  init-rut-simple
::      =^  f  state  (n state (escape-accepted:l1 ~rabsum-ravtyd ~rigred))
::      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
::  ::
::  ++  test-rut-l1-adopt-3
::    :: L1-adopt  A1 | A2  | *   | *   | *   | -> !! :: no adopt if not escaping
::    %+  expect-eq
::      !>  [[~ ~rigrut] %.y ~holrut]
::    ::
::      !>
::      =|  =^state:naive
::      =^  f  state  init-rut-simple
::      =^  f  state  (n state (escape-requested:l1 ~rabsum-ravtyd ~rigrut))
::      =^  f  state  (n state (escape-accepted:l1 ~rabsum-ravtyd ~rigred))
::      [escape.net sponsor.net]:(got:orm points.state ~rabsum-ravtyd)
::
++  test-marbud-l2-change-keys-whole-state  ^-  tang
  =/  new-keys       [%configure-keys encr auth suit |]
  =|  =^state:naive
  =^  f  state  (init-marbud state)
  =/  marbud-point  (got:orm points.state ~marbud)
  =/  new-marbud  marbud-point(keys.net [1 suit auth encr], nonce.owner.own 1)
  ::
  %+  expect-eq
    !>  state(points (put:orm points.state ~marbud new-marbud))
  ::
    !>
    =^  f  state  (n state %bat q:(gen-tx 0 [marbud-own new-keys] %marbud-key-0))
    state
  ::
::  old tests. Some of these test the same things as +test-rut but
::  they've been left in since the circumstances are slightly different
::  in a way that ought to be unimportant but you never know.
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
    dominion:(got:orm points.state ~marbud)
::
++  test-transfer-batch  ^-  tang
  =/  marbud-transfer    [0 [marbud-own %transfer-point (addr %marbud-key-0) |] %marbud-key-0]
  =/  marbud-transfer-2  [1 [marbud-own %transfer-point (addr %marbud-key-1) |] %marbud-key-0]
  ::
  =,  l2-event-gen
  =/  marbud-batch=tx-list  (limo marbud-transfer marbud-transfer-2 ~)
  ::
  ;:  weld
    %+  expect-eq
      !>  [(addr %marbud-key-1) 2]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx marbud-transfer))
      =^  f  state  (n state %bat q:(gen-tx marbud-transfer-2))
      owner.own:(got:orm points.state ~marbud)
    ::
    %+  expect-eq
      !>  [(addr %marbud-key-1) 2]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat (tx-list-to-batch marbud-batch))
      owner.own:(got:orm points.state ~marbud)
  ==
::
++  test-l1-changed-spawn-proxy  ^-  tang
  %+  expect-eq
    !>  [(addr %bud-skey) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-bud state)
    =^  f  state  (n state (changed-spawn-proxy:l1 ~bud (addr %bud-skey)))
    spawn-proxy.own:(got:orm points.state ~bud)
::
++  test-l1-changed-transfer-proxy  ^-  tang
  %+  expect-eq
    !>  [(addr %bud-key-1) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-bud state)
    =^  f  state  (n state (changed-transfer-proxy:l1 ~bud (addr %bud-key-1)))
    transfer-proxy.own:(got:orm points.state ~bud)
::
++  test-l1-changed-management-proxy  ^-  tang
  %+  expect-eq
    !>  [(addr %bud-mkey) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-bud state)
    =^  f  state  (n state (changed-management-proxy:l1 ~bud (addr %bud-mkey)))
    management-proxy.own:(got:orm points.state ~bud)
::
++  test-l1-changed-voting-proxy  ^-  tang
  %+  expect-eq
    !>  [(addr %bud-vkey) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-bud state)
    =^  f  state  (n state (changed-voting-proxy:l1 ~bud (addr %bud-vkey)))
    voting-proxy.own:(got:orm points.state ~bud)
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
    |1:keys.net:(got:orm points.state ~bud)
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
    escape.net:(got:orm points.state ~sambud)
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
    escape.net:(got:orm points.state ~sambud)
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
    [escape.net sponsor.net]:(got:orm points.state ~sambud)
::
++  test-l1-star-lost-sponsor  ^-  tang
  %+  expect-eq
    !>  [~ %.n ~bud]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-sambud state)
    =^  f  state  (n state (lost-sponsor:l1 ~sambud ~bud))
    [escape.net sponsor.net]:(got:orm points.state ~sambud)
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
    spawn-proxy.own:(got:orm points.state ~marbud)
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
    transfer-proxy.own:(got:orm points.state ~marbud)
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
    management-proxy.own:(got:orm points.state ~marbud)
::
++  test-l2-dopbud-spawn-proxy-deposit  ^-  tang
  %+  expect-eq
    !>  %spawn
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-dopbud state)
    dominion:(got:orm points.state ~dopbud)
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
    spawn-proxy.own:(got:orm points.state ~sambud)
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
    spawn-proxy.own:(got:orm points.state ~sambud)
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
    spawn-proxy.own:(got:orm points.state ~sambud)
::
++  test-l2-sambud-spawn-proxy-predeposit-spawn  ^-  tang
  =/  l2-sproxy  [[~sambud %spawn] %set-spawn-proxy (addr %sambud-skey-1)]
  =/  lf-spawn  [[~sambud %spawn] %spawn ~lisdur-fodrys (addr %lf-key-0)]
  ;:  weld
    %+  expect-eq
      !>  [`@ux`(addr %lf-key-0) 0]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-sambud state)
      =^  f  state  (n state (changed-spawn-proxy:l1 ~sambud (addr %sambud-skey-0)))
      =^  f  state  (n state (changed-spawn-proxy:l1 ~sambud deposit-address:naive))
      =^  f  state  (n state %bat q:(gen-tx 0 lf-spawn %sambud-skey-0))
      transfer-proxy.own:(got:orm points.state ~lisdur-fodrys)
    ::
    %+  expect-eq
      !>  [`@ux`(addr %lf-key-0) 0]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-sambud state)
      =^  f  state  (n state (changed-spawn-proxy:l1 ~sambud (addr %sambud-skey-0)))
      =^  f  state  (n state (changed-spawn-proxy:l1 ~sambud deposit-address:naive))
      =^  f  state  (n state %bat q:(gen-tx 0 l2-sproxy %sambud-skey-0))
      =^  f  state  (n state %bat q:(gen-tx 1 lf-spawn %sambud-skey-1))
      transfer-proxy.own:(got:orm points.state ~lisdur-fodrys)
  ==
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
  =/  lt-point  (got:orm points.state ~linnup-torsyx)
  =/  new-lt  lt-point(nonce.owner.own 1)
  =/  no-op-state  state(points (put:orm points.state ~linnup-torsyx new-lt))
  ::
  %+  expect-eq
    !>  no-op-state
  ::
    !>
    =^  f  state  (n state %bat q:(gen-tx 0 rt-spawn %lt-key-0))
    state
::
++  test-marbud-l2-spawn  ^-  tang
  =/  marbud-sproxy  [0 [marbud-own %set-spawn-proxy (addr %marbud-skey)] %marbud-key-0]
  =/  lt-spawn       [%spawn ~linnup-torsyx (addr %lt-key-0)]
  =,  l2-event-gen
  =/  spawn-batch=tx-list  (limo marbud-sproxy [0 [marbud-spn lt-spawn] %marbud-skey] ~)
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
      transfer-proxy.own:(got:orm points.state ~linnup-torsyx)
    ::
    %+  expect-eq
    ::  Tests l2 spawning with spawn proxy
      !>  [`@ux`(addr %lt-key-0) 0]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx marbud-sproxy))
      =^  f  state  (n state %bat q:(gen-tx 0 [marbud-spn lt-spawn] %marbud-skey))
      transfer-proxy.own:(got:orm points.state ~linnup-torsyx)
    ::
    %+  expect-eq
    ::  Tests l2 spawning with spawn proxy as a batch
      !>  [`@ux`(addr %lt-key-0) 0]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat (tx-list-to-batch spawn-batch))
      transfer-proxy.own:(got:orm points.state ~linnup-torsyx)
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
  =/  marbud-point  (got:orm points.state ~marbud)
  =/  new-marbud  marbud-point(nonce.spawn-proxy.own 1)
  =/  no-op-state  state(points (put:orm points.state ~marbud new-marbud))
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
      |1:keys.net:(got:orm points.state ~marbud)
    ::
    %+  expect-eq
      !>  [suit auth encr]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 marbud-mproxy %marbud-key-0))
      =^  f  state  (n state %bat q:(gen-tx 0 [marbud-mgt new-keys] %marbud-mkey))
      |1:keys.net:(got:orm points.state ~marbud)
    ::
  ==
::
++  test-marbud-l2-proxies-transfer  ^-  tang
  =/  marbud-new-keys            [0 [marbud-own %configure-keys encr auth suit |] %marbud-key-0]
  =/  marbud-sproxy              [0 [marbud-own %set-spawn-proxy (addr %marbud-skey)] %marbud-key-0]
  =/  marbud-mproxy              [1 [marbud-own %set-management-proxy (addr %marbud-mkey)] %marbud-key-0]
  =/  marbud-tproxy              [2 [marbud-own %set-transfer-proxy (addr %marbud-key-1)] %marbud-key-0]
  =/  marbud-transfer-breach     [1 [marbud-own %transfer-point (addr %marbud-key-1) &] %marbud-key-0]
  =/  marbud-transfer-no-breach  [1 [marbud-own %transfer-point (addr %marbud-key-1) |] %marbud-key-0]
  =/  marbud-xfr-breach          [0 [marbud-xfr %transfer-point (addr %marbud-key-1) &] %marbud-key-1]
  =/  marbud-xfr-no-breach       [0 [marbud-xfr %transfer-point (addr %marbud-key-1) |] %marbud-key-1]
  ::
  =,  l2-event-gen
  =/  test1=tx-list  (limo marbud-sproxy marbud-mproxy marbud-tproxy marbud-xfr-breach ~)
  =/  test2=tx-list  (limo marbud-new-keys marbud-transfer-breach ~)
  =/  test3=tx-list  (limo marbud-sproxy marbud-mproxy marbud-tproxy marbud-xfr-no-breach ~)
  =/  test4=tx-list  (limo marbud-new-keys marbud-transfer-no-breach ~)
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
      =^  f  state  (n state %bat q:(gen-tx marbud-sproxy))
      =^  f  state  (n state %bat q:(gen-tx marbud-mproxy))
      =^  f  state  (n state %bat q:(gen-tx marbud-tproxy))
      =^  f  state  (n state %bat q:(gen-tx marbud-xfr-breach))
      ^-  [[@ @] [@ @] [@ @] [@ @] [@ @]]
      own:(got:orm points.state ~marbud)
    ::
    %+  expect-eq
    ::  batch version
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
      =^  f  state  (n state %bat (tx-list-to-batch test1))
      ^-  [[@ @] [@ @] [@ @] [@ @] [@ @]]
      own:(got:orm points.state ~marbud)
    ::
    %+  expect-eq
    ::  Tests that networking keys are reset on transfer with breach
      !>
      [0 0 0]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx marbud-new-keys))
      =^  f  state  (n state %bat q:(gen-tx marbud-transfer-breach))
      |1:keys.net:(got:orm points.state ~marbud)
    ::
    %+  expect-eq
    ::  batch version
      !>
      [0 0 0]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat (tx-list-to-batch test2))
      |1:keys.net:(got:orm points.state ~marbud)
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
      =^  f  state  (n state %bat q:(gen-tx marbud-sproxy))
      =^  f  state  (n state %bat q:(gen-tx marbud-mproxy))
      =^  f  state  (n state %bat q:(gen-tx marbud-tproxy))
      =^  f  state  (n state %bat q:(gen-tx marbud-xfr-no-breach))
      ^-  [[@ @] [@ @] [@ @] [@ @] [@ @]]
      own:(got:orm points.state ~marbud)
    ::
    %+  expect-eq
    ::  batch version
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
      =^  f  state  (n state %bat (tx-list-to-batch test3))
      ^-  [[@ @] [@ @] [@ @] [@ @] [@ @]]
      own:(got:orm points.state ~marbud)
    ::
    %+  expect-eq
    ::  Tests that networking keys are not reset when transfering without breach
      !>
      [suit auth encr]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx marbud-new-keys))
      =^  f  state  (n state %bat q:(gen-tx marbud-transfer-no-breach))
      |1:keys.net:(got:orm points.state ~marbud)
    ::
    %+  expect-eq
    ::  batch version
      !>
      [suit auth encr]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat (tx-list-to-batch test4))
      |1:keys.net:(got:orm points.state ~marbud)
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
      [rift.net life.keys.net]:(got:orm points.state ~marbud)
    ::
    %+  expect-eq
    ::  breach=%.y
      !>  [1 1]
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state %bat q:(gen-tx 0 new-keys-yes-reset %marbud-key-0))
      [rift.net life.keys.net]:(got:orm points.state ~marbud)
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
      [rift.net life.keys.net]:(got:orm points.state ~marbud)
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
      [rift.net life.keys.net]:(got:orm points.state ~marbud)
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
      [rift.net life.keys.net]:(got:orm points.state ~marbud)
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
      [rift.net life.keys.net]:(got:orm points.state ~marbud)
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
      [rift.net life.keys.net]:(got:orm points.state ~marbud)
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
    transfer-proxy.own:(got:orm points.state ~palsep-picdun)
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
    transfer-proxy.own:(got:orm points.state ~laclur-rachul)
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
    owner.own:(got:orm points.state ~linnup-torsyx)
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
    owner.own:(got:orm points.state ~palsep-picdun)
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
    escape.net:(got:orm points.state ~linnup-torsyx)
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
    escape.net:(got:orm points.state ~linnup-torsyx)
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
    [escape.net sponsor.net]:(got:orm points.state ~linnup-torsyx)
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
    escape.net:(got:orm points.state ~linnup-torsyx)
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
    [escape.net sponsor.net]:(got:orm points.state ~linnup-torsyx)
::
::  Fuzz tests. These just feed the L2 contract various forms of garbage. None of them
::  should alter the state of the PKI.
::
++  test-fuzz-octs
::  this test just throws completely random octs at naive.hoon
::
  =+  [seed=`@`%test-fuzz-octs i=0]
  =|  =^state:naive
  =^  f  state  init-red-simple
  =/  init-state  state
  |-  ^-  tang
  ?:  =(i 10)  ~
  %+  weld  $(seed (shas `@`%versace seed), i +(i))
  =/  state  init-state
  =/  rng  ~(. og seed)
  =^  proxy  rng  (raws:rng 8)
  =^  ship  rng  (raws:rng 32)
  =^  action  rng  (raws:rng 8)
  =^  junk-length  rng  (rads:rng 200)
  ::  increment junk-length to prevent 0 case
  =^  junk  rng  (raws:rng +(junk-length))
  =^  nonce  rng  (rads:rng 999)
  =^  key  rng  (raws:rng 256)
  =/  fuzz=octs
    %:  cad:naive  3
      1^proxy
      4^ship
      1^action
      (met 3 junk)^junk
      ~
    ==
  =/  random-tx
    %^  sign-tx  key  nonce  fuzz
  ::
  %+  expect-eq
    !>  init-state
  ::  ::
    !>
    =^  f  state  (n state %bat q:random-tx)
    state
::
++  test-fuzz-valid-ship-key
::  this test uses a valid ship and key but otherwise
::  hands the contract garbage
::
  =+  [seed=`@`%test-fuzz-valid-ship-key i=0]
  =|  =^state:naive
  =^  f  state  init-red-simple
  =/  init-state  state
  |-  ^-  tang
  ?:  =(i 100)  ~
  %+  weld  $(seed (shas `@`%iceberg-simpson seed), i +(i))
  =/  state  init-state
  =/  rng  ~(. og seed)
  =/  ship  ~pinpun-pilsun
  =^  proxy  rng  (raws:rng 8)
  =^  action  rng  (raws:rng 8)
  =^  junk-length  rng  (rads:rng 200)
  ::  increment junk-length to prevent 0 case
  =^  junk  rng  (raws:rng +(junk-length))
  =^  nonce  rng  (rads:rng 999)
  =/  fuzz=octs
    %:  cad:naive  3
      1^proxy
      4^ship
      1^action
      (met 3 junk)^junk
      ~
    ==
  =/  random-tx
    %^  sign-tx  %losrut-pp-key-0  nonce  fuzz
  ::
  %+  expect-eq
    !>  init-state
  ::  ::
    !>
    =^  f  state  (n state %bat q:random-tx)
    state
::
++  test-fuzz-valid-ship-key-proxy-nonce
::  this test uses a valid ship, key, proxy, nonce but otherwise
::  hands the contract garbage
::
  =+  [seed=`@`%test-fuzz-valid-ship-key-proxy-nonce i=0]
  =|  =^state:naive
  =^  f  state  init-red-simple
  =/  init-state  state
  |-  ^-  tang
  ?:  =(i 100)  ~
  %+  weld  $(seed (shas `@`%tiptoe seed), i +(i))
  =/  state  init-state
  =/  rng  ~(. og seed)
  =/  ship=@p  ~pinpun-pilsun
  =^  action  rng  (raws:rng 8)
  =^  junk-length  rng  (rads:rng 200)
  ::  increment junk-length to prevent case of 0
  =^  junk  rng  (raws:rng +(junk-length))
  =/  fuzz=octs
    %:  cad:naive  3
      1^(can 0 3^%0 5^0 ~)  :: %own proxy
      4^ship
      1^action
      (met 3 junk)^junk
      ~
    ==
  =/  random-tx
    %^  sign-tx  %losrut-pp-key-0  1  fuzz
  ::
  %+  expect-eq
    !>  init-state
  ::
    !>
    =^  f  state  (n state %bat q:random-tx)
    state
::
::  I think the following test ought to be trying something else:
::  checking to see if valid transactions followed by garbage still
::  process the valid tx
::  ++  test-fuzz-after-tx
::  ::  this creates a valid transaction of each type but then adds
::  ::  random bits to the end of it
::  ::
::    =+  [seed=`@`%test-fuzz-after-tx1 i=0]
::    =|  =^state:naive
::    =^  f  state  init-red-simple
::    =/  init-state  state
::    |-  ^-  tang
::    ?:  =(i 11)  ~  :: 10 attempts for each transaction type
::    %+  weld  $(seed (shas `@`%howmuchisfour seed), i +(i))
::    =+  j=0
::    |^  ^-  tang
::    ?:  =(j 11)  ~  :: there are 10 transaction types
::    %+  weld  $(seed (shas `@`%eris seed), j +(j))
::    =/  rng  ~(. og seed)
::    =^  junk-length  rng  (rads:rng 200)
::    ::increment to prevent zero-length junk
::    =^  junk  rng  (raws:rng +(junk-length))
::    =/  tx-octs=octs
::      ?+  j  !!
::        %0  do-spawn
::        %1  do-transfer-point
::        %2  do-configure-keys
::        %3  do-escape
::        %4  do-cancel-escape
::        %5  do-adopt
::        %6  do-reject
::        %7  do-detach
::        %8  do-set-management-proxy
::        %9  do-set-spawn-proxy
::        %10  do-set-transfer-proxy
::      ==
::    =/  fuzz  (mix (lsh [3 (met 3 q:tx-octs)] junk) q:tx-octs)
::    =/  fuzz-octs=octs  [(met 3 fuzz) fuzz]
::    ::  the conditionals that follow are to ensure the correct key and
::    ::  nonce are used.
::    =/  random-tx
::      ?:  =(j 4)
::        %^  sign-tx  %holrut-rr-key-0  1  fuzz-octs
::      ?:  |(=(j 5) =(j 6))
::        %^  sign-tx  %rigred-key-0  0  fuzz-octs
::      %^  sign-tx  %losrut-key-0  2  fuzz-octs
::    ::
::    =/  state  init-state
::    %+  category  (weld "fuzz tx type " (scow %ud j))
::    %+  expect-eq
::      !>  init-state
::    ::
::      !>
::      =^  f  state  (n state %bat q:random-tx)
::      ~&  ['tx-type' j]
::      state
::    ::
::    ++  do-spawn  ^-  octs
::      =/  from  [ship=~losrut proxy=%own]
::      =/  sptx=skim-tx:naive  [%spawn ~mishus-loplus (addr %nowhere)]
::      =/  tx=tx:naive  [from sptx]
::      (gen-tx-octs tx)
::    ++  do-transfer-point  ^-  octs
::      =/  from  [ship=~losrut proxy=%own]
::      =/  xrtx=skim-tx:naive  [%transfer-point (addr %somewhere) &]
::      =/  tx=tx:naive  [from xrtx]
::      (gen-tx-octs tx)
::    ++  do-configure-keys  ^-  octs
::      =/  from  [ship=~losrut proxy=%own]
::      =/  cftx=skim-tx:naive  [%configure-keys (shax 'uno') (shax 'dos') (shax 'tres') |]
::      =/  tx=tx:naive  [from cftx]
::      (gen-tx-octs tx)
::    ++  do-escape  ^-  octs
::      =/  from  [ship=~losrut proxy=%own]
::      =/  estx=skim-tx:naive  [%escape ~red]
::      =/  tx=tx:naive  [from estx]
::      (gen-tx-octs tx)
::    ++  do-cancel-escape  ^-  octs
::      =/  from  [ship=~rabsum-ravtyd proxy=%own]
::      =/  cetx=skim-tx:naive  [%cancel-escape ~rigred]
::      =/  tx=tx:naive  [from cetx]
::      (gen-tx-octs tx)
::    ++  do-adopt  ^-  octs
::      =/  from  [ship=~rigred proxy=%own]
::      =/  adtx=skim-tx:naive  [%adopt ~rabsum-ravtyd]
::      =/  tx=tx:naive  [from adtx]
::      (gen-tx-octs tx)
::    ++  do-reject  ^-  octs
::      =/  from  [ship=~rigred proxy=%own]
::      =/  rjtx=skim-tx:naive  [%adopt ~rabsum-ravtyd]
::      =/  tx=tx:naive  [from rjtx]
::      (gen-tx-octs tx)
::    ++  do-detach
::      =/  from  [ship=~losrut proxy=%own]
::      =/  dttx=skim-tx:naive  [%detach ~rabsum-ravtyd]
::      =/  tx=tx:naive  [from dttx]
::      (gen-tx-octs tx)
::    ++  do-set-management-proxy
::      =/  from  [ship=~losrut proxy=%own]
::      =/  mgtx=skim-tx:naive  [%set-management-proxy (addr %new-mgmt)]
::      =/  tx=tx:naive  [from mgtx]
::      (gen-tx-octs tx)
::    ++  do-set-spawn-proxy
::      =/  from  [ship=~losrut proxy=%own]
::      =/  sptx=skim-tx:naive  [%set-spawn-proxy (addr %new-spawn)]
::      =/  tx=tx:naive  [from sptx]
::      (gen-tx-octs tx)
::    ++  do-set-transfer-proxy
::      =/  from  [ship=~losrut proxy=%own]
::      =/  tftx=skim-tx:naive  [%set-transfer-proxy (addr %new-xfer)]
::      =/  tx=tx:naive  [from tftx]
::      (gen-tx-octs tx)
::    --
::
:: the following tests are to ensure that padding of zeroes creates
:: no issues
::
++  test-zod-spawn-to-zero
  =/  bz-spawn        [[~zod %spawn] %spawn ~binzod 0x0]
  ::
  %+  expect-eq
    !>  [0x0 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-zod state)
    =^  f  state  (n state %bat q:(gen-tx 0 bz-spawn %zod-skey-0))
    transfer-proxy.own:(got:orm points.state ~binzod)
::
++  test-zod-spawn-proxy
  =/  bz-spawn        [[~zod %spawn] %spawn ~binzod (addr %binzod-key-0)]
  ::
  %+  expect-eq
    !>  [`@ux`(addr %binzod-key-0) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-zod state)
    =^  f  state  (n state %bat q:(gen-tx 0 bz-spawn %zod-skey-0))
    transfer-proxy.own:(got:orm points.state ~binzod)
::
++  test-dopzod-spawn
  =/  tm-spawn        [[~dopzod %own] %spawn ~tasben-monbur (addr %tm)]
  ::
  %+  expect-eq
    !>  [`@ux`(addr %tm) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-zod state)
    =^  f  state  (n state %bat q:(gen-tx 0 tm-spawn %dopzod-key-0))
    transfer-proxy.own:(got:orm points.state ~tasben-monbur)
::
++  test-address-padding
  ::  tells ~dopzod to spawn ~tasben-monbur at 0x00000000001111111111
  =/  spawn-octs=octs
  %:  cad:naive  3
      1^(can 0 3^%0 5^0 ~)                       :: %own proxy
      4^~dopzod
      1^%1                                       :: %spawn
      4^~tasben-monbur
      20^(can 3 10^0 1^1 9^0 ~)
      ~
    ==
  =/  signed-tx=octs
  %^  sign-tx  %dopzod-key-0  0  spawn-octs
  ::
  %+  expect-eq
    !>  [`@ux`(can 3 10^0 1^1 9^0 ~) 0]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-zod state)
    =^  f  state  (n state %bat q:signed-tx)
    transfer-proxy.own:(got:orm points.state ~tasben-monbur)
::
::  TODO: L1 tests with leading zeroes. in particular, changing
::  keys uses data.log, so keys with leading zeroes might run into
::  issues
::
++  test-batch-generation
  =,  l2-event-gen
  =/  marbud-transfer    [marbud-own %transfer-point (addr %marbud-key-0) |]
  =/  marbud-transfer-2  [marbud-own %transfer-point (addr %marbud-key-1) |]
  ::
  =/  tx-1=full-tx  [0 marbud-transfer %marbud-key-0]
  =/  tx-2=full-tx  [1 marbud-transfer-2 %marbud-key-0]
  =/  txs=tx-list  (limo ~[tx-1 tx-2])
  %+  expect-eq
    !>  [(addr %marbud-key-1) 2]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state %bat (tx-list-to-batch txs))
    owner.own:(got:orm points.state ~marbud)
::
++  test-changed-dns
  ::  uses actual data from ETH transaction
  ::  0x51a26c3b100ad1c7aa8d593068df60465046d437edc3e939fadee4056791fd13
  ::
  =/  data  %-  crip
            ;:  weld
            "0x000000000000000000000000000000"
            "00000000000000000000000000000000"
            "60000000000000000000000000000000"
            "00000000000000000000000000000000"
            "a0000000000000000000000000000000"
            "00000000000000000000000000000000"
            "e0000000000000000000000000000000"
            "00000000000000000000000000000000"
            "0975726269742e6f7267000000000000"
            "00000000000000000000000000000000"
            "00000000000000000000000000000000"
            "00000000000000000000000000000000"
            "0975726269742e6f7267000000000000"
            "00000000000000000000000000000000"
            "00000000000000000000000000000000"
            "00000000000000000000000000000000"
            "0975726269742e6f7267000000000000"
            "00000000000000000000000000000000"
            "00"
            ==
  ::
  %+  expect-eq
    !>  `(list @t)`['urbit.org' 'urbit.org' 'urbit.org' ~]
  ::
    !>
    =|  =^state:naive
    =^  f  state  (init-marbud state)
    =^  f  state  (n state (changed-dns:l1 (hex-to-num:ethereum data)))
    dns.state
::
++  test-approval-for-all
  =|  operators=(jug address address)
  =/  op1   (~(put ju operators) (addr %test1) (addr %test2))
  =/  del1  (~(del ju op1) (addr %test1) (addr %test2))
  =/  op2   (~(put ju op1) (addr %test1) (addr %test3))
  =/  del2  (~(del ju op2) (addr %test1) (addr %test2))
  ::
  ;:  weld
    %+  expect-eq
      !>  op1
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state (approval-for-all:l1 (addr %test1) (addr %test2) 1))
      operators.state
    ::
    %+  expect-eq
      !>  del1
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state (approval-for-all:l1 (addr %test1) (addr %test2) 1))
      =^  f  state  (n state (approval-for-all:l1 (addr %test1) (addr %test2) 0))
      operators.state
    ::
    %+  expect-eq
      !>  op2
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state (approval-for-all:l1 (addr %test1) (addr %test2) 1))
      =^  f  state  (n state (approval-for-all:l1 (addr %test1) (addr %test3) 1))
      operators.state
    ::
    %+  expect-eq
      !>  del2
    ::
      !>
      =|  =^state:naive
      =^  f  state  (init-marbud state)
      =^  f  state  (n state (approval-for-all:l1 (addr %test1) (addr %test2) 1))
      =^  f  state  (n state (approval-for-all:l1 (addr %test1) (addr %test3) 1))
      =^  f  state  (n state (approval-for-all:l1 (addr %test1) (addr %test2) 0))
      operators.state
  ==
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
::      transfer-proxy.own:(got:orm points.state ~marbud)
--
