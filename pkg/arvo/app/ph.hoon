::  Test the pH of your aquarium.  See if it's safe to put in real fish.
::
::  usage:
::  :aqua [%run-test %test-add]
::
::  TODO:
::  - Restore a fleet
::  - Compose tests
::
/-  aquarium, ph
/+  ph, ph-tests, ph-azimuth, ph-philter
=,  ph-sur=^ph
=,  aquarium
=,  ph
=,  ph-philter
=>  $~  |%
    +$  move  (pair bone card)
    +$  card
      $%  [%poke wire dock poke-type]
          [%peer wire dock path]
          [%pull wire dock ~]
          [%diff diff-type]
      ==
    ::
    +$  poke-type
      $%  [%aqua-events (list aqua-event)]
          [%drum-start term term]
          [%aqua-vane-control ?(%subscribe %unsubscribe)]
      ==
    ::
    +$  diff-type
      $%  [%aqua-effects aqua-effects]
      ==
    ::
    +$  state
      $:  %0
          test-core=(unit test-core-state)
          tests=(map term [(list ship) _*form:(ph ,~)])
          other-state
      ==
    ::
    +$  test-core-state
      $:  lab=term
          hers=(list ship)
          test=_*form:(ph ,~)
      ==
    ::
    +$  other-state
      $:  test-qeu=(qeu term)
          results=(list (pair term ?))
          effect-log=(list [who=ship uf=unix-effect])
      ==
    --
=,  gall
=/  vane-apps=(list term)
  ~[%aqua-ames %aqua-behn %aqua-dill %aqua-eyre]
|_  $:  hid=bowl
        state
    ==
++  this  .
++  manual-tests
  ^-  (list (pair term [(list ship) _*form:(ph ,~)]))
  =+  (ph-tests our.hid)
  =+  ph-azimuth=(ph-azimuth our.hid)
  =/  eth-node  (spawn:ph-azimuth ~bud)
  =/  m  (ph ,~)
  :~  :+  %boot-bud
        ~[~bud]
      (raw-ship ~bud ~)
    ::
      :+  %add
        ~[~bud]
      ;<  ~  bind:m  (raw-ship ~bud ~)
      |=  pin=ph-input
      ?:  =(%init -.q.uf.pin)
        [& (dojo ~bud "[%test-result (add 2 3)]") %wait ~]
      ?:  (is-dojo-output ~bud who.pin uf.pin "[%test-result 5]")
        [& ~ %done ~]
      [& ~ %wait ~]
    ::
      :+  %hi
        ~[~bud ~dev]
      ;<  ~  bind:m  (raw-ship ~bud ~)
      ;<  ~  bind:m  (raw-ship ~dev ~)
      (send-hi ~bud ~dev)
    ::
      :+  %boot-planet
        ~[~bud ~marbud ~linnup-torsyx]
      (planet ~linnup-torsyx)
    ::
      :+  %second-cousin-hi
        ~[~bud ~marbud ~linnup-torsyx ~dev ~mardev ~mitnep-todsut]
      ;<  ~  bind:m  (planet ~linnup-torsyx)
      ;<  ~  bind:m  (planet ~mitnep-todsut)
      (send-hi ~linnup-torsyx ~mitnep-todsut)
    ::
      :+  %change-file
        ~[~bud]
      ;<  ~        bind:m  (raw-ship ~bud ~)
      ;<  file=@t  bind:m  (touch-file ~bud %home)
      (check-file-touched ~bud %home file)
    ::
      :+  %child-sync
        ~[~bud ~marbud]
      ;<  ~        bind:m  (star ~marbud)
      ;<  file=@t  bind:m  (touch-file ~bud %base)
      (check-file-touched ~marbud %home file)
    ::
      :+  %boot-az
        ~[~bud]
      ;<  [eth-node=_eth-node ~]  bind:m
        %+  (wrap-philter ,_eth-node ,~)
          router:eth-node
        (raw-real-ship:eth-node ~bud)
      (pure:m ~)
    ::
      :+  %hi-az
        ~[~bud ~dev]
      =.  eth-node  (spawn:eth-node ~dev)
      ;<  [eth-node=_eth-node ~]  bind:m
        %+  (wrap-philter ,_eth-node ,~)
          router:eth-node
        ;<  ~  bind:m  (raw-real-ship:eth-node ~dev)
        ~&  >  %dev-done
        ;<  ~  bind:m  (raw-real-ship:eth-node ~bud)
        ~&  >  %bud-done
        (send-hi ~bud ~dev)
      (pure:m ~)
    ::
      :+  %moon-az
        ~[~bud ~marbud ~linnup-torsyx ~linnup-torsyx-linnup-torsyx ~dev]
      =.  eth-node  (spawn:eth-node ~marbud)
      =.  eth-node  (spawn:eth-node ~linnup-torsyx)
      ;<  [eth-node=_eth-node ~]  bind:m
        %+  (wrap-philter ,_eth-node ,~)
          router:eth-node
        ;<  ~  bind:m  (raw-real-ship:eth-node ~bud)
        ~&  >  'BUD DONE'
        ;<  ~  bind:m  (raw-real-ship:eth-node ~marbud)
        ~&  >  'MARBUD DONE'
        ;<  ~  bind:m  (raw-real-ship:eth-node ~linnup-torsyx)
        ~&  >  'LINNUP DONE'
        ;<  ~  bind:m  (raw-real-ship:eth-node ~linnup-torsyx-linnup-torsyx)
        ~&  >  'MOON LINNUP DONE'
        ;<  ~  bind:m  (send-hi ~bud ~linnup-torsyx-linnup-torsyx)
        ~&  >  'HI DOWN DONE'
        ;<  ~  bind:m  (send-hi ~linnup-torsyx-linnup-torsyx ~marbud)
        ~&  >  'HI UP DONE'
        ;<  ~  bind:m  (raw-real-ship:eth-node ~bud)
        ~&  >  'HI OVER UP DONE'
        ;<  ~  bind:m  (send-hi ~linnup-torsyx-linnup-torsyx ~dev)
        ::  ~&  >  'HI OVER DOWN DONE'
        ::  ;<  ~  bind:m  (send-hi ~dev ~linnup-torsyx-linnup-torsyx)
        (pure:m ~)
      (pure:m ~)
    ::
      :+  %breach-hi
        ~[~bud ~dev]
      =.  eth-node  (spawn:eth-node ~dev)
      ;<  [eth-node=_eth-node ~]  bind:m
        %+  (wrap-philter ,_eth-node ,~)
          router:eth-node
        ;<  ~  bind:m  (raw-real-ship:eth-node ~bud)
        ~&  >  'BUD DONE'
        ;<  ~  bind:m  (raw-real-ship:eth-node ~dev)
        ~&  >  'DEV DONE'
        (send-hi ~bud ~dev)
      ~&  >  'HI DONE'
      ;<  eth-node=_eth-node  bind:m
        (breach-and-hear:eth-node our.hid ~dev ~bud)
      ~&  >  'BREACH DONE'
      ;<  [eth-node=_eth-node ~]  bind:m
        %+  (wrap-philter ,_eth-node ,~)
          router:eth-node
        ;<  ~  bind:m  (send-hi-not-responding ~bud ~dev)
        ~&  >  'HI NOT RESPONDING DONE'
        ;<  ~  bind:m  (raw-real-ship:eth-node ~dev)
        ~&  >  'REBOOT DEV DONE'
        (wait-for-dojo ~bud "hi ~dev successful")
      ~&  >  'DONE'
      (pure:m ~)
    ::
      :+  %breach-hi-cousin
        ~[~bud ~dev ~marbud ~mardev]
      =.  eth-node  (spawn:eth-node ~dev)
      =.  eth-node  (spawn:eth-node ~marbud)
      =.  eth-node  (spawn:eth-node ~mardev)
      ;<  [eth-node=_eth-node ~]  bind:m
        %+  (wrap-philter ,_eth-node ,~)
          router:eth-node
        ;<  ~  bind:m  (raw-real-ship:eth-node ~bud)
        ;<  ~  bind:m  (raw-real-ship:eth-node ~dev)
        ;<  ~  bind:m  (raw-real-ship:eth-node ~marbud)
        ;<  ~  bind:m  (raw-real-ship:eth-node ~mardev)
        (send-hi ~marbud ~mardev)
      ;<  eth-node=_eth-node  bind:m
        (breach-and-hear:eth-node our.hid ~mardev ~marbud)
      ;<  [eth-node=_eth-node ~]  bind:m
        %+  (wrap-philter ,_eth-node ,~)
          router:eth-node
        ;<  ~  bind:m  (send-hi-not-responding ~marbud ~mardev)
        ;<  ~  bind:m  (raw-real-ship:eth-node ~mardev)
        (wait-for-dojo ~marbud "hi ~mardev successful")
      (pure:m ~)
    ::
      :+  %breach-sync
        ~[~bud ~marbud]
      =.  eth-node  (spawn:eth-node ~marbud)
      =.  eth-node  (spawn:eth-node ~fipfes)
      ;<  [eth-node=_eth-node ~]  bind:m
        %+  (wrap-philter ,_eth-node ,~)
          router:eth-node
        ;<  ~        bind:m  (raw-real-ship:eth-node ~bud)
        ~&  >  'BUD DONE'
        ;<  ~        bind:m  (raw-real-ship:eth-node ~marbud)
        ~&  >  'MARBUD DONE'
        ;<  file=@t  bind:m  (touch-file ~bud %base)
        ~&  >  'TOUCH FILE DONE'
        (check-file-touched ~marbud %home file)
      ~&  >  'TOUCH FILE CHECK DONE'
      ;<  eth-node=_eth-node  bind:m
        (breach-and-hear:eth-node our.hid ~bud ~marbud)
      ~&  >  'BREACH DONE'
      ;<  [eth-node=_eth-node ~]  bind:m
        %+  (wrap-philter ,_eth-node ,~)
          router:eth-node
        ;<  ~        bind:m  (raw-real-ship:eth-node ~bud)
        ~&  >  'BUD RE DONE'
        ;<  ~        bind:m  (just-events (dojo ~bud "|merge %base ~marbud %kids, =gem %this"))
        ~&  >  'THIS MERGE STARTED DONE'
        ;<  file=@t  bind:m  (touch-file ~bud %base)
        ~&  >  'TOUCH-1 DONE'
        ;<  file=@t  bind:m  (touch-file ~bud %base)
        ~&  >  'TOUCH-2 DONE'
        (check-file-touched ~marbud %home file)
      ~&  >  'DONE DONE'
      (pure:m ~)
    ::
      :+  %breach-multiple
        ~[~bud ~marbud]
      =.  eth-node  (spawn:eth-node ~marbud)
      =.  eth-node  (spawn:eth-node ~fipfes)
      ;<  [eth-node=_eth-node ~]  bind:m
        %+  (wrap-philter ,_eth-node ,~)
          router:eth-node
        ;<  ~        bind:m  (raw-real-ship:eth-node ~bud)
        ~&  >  'BUD DONE'
        ;<  ~        bind:m  (raw-real-ship:eth-node ~marbud)
        ~&  >  'MARBUD DONE'
        ;<  file=@t  bind:m  (touch-file ~bud %base)
        ~&  >  'TOUCH DONE'
        (check-file-touched ~marbud %home file)
      ;<  eth-node=_eth-node  bind:m
        (breach-and-hear:eth-node our.hid ~bud ~marbud)
      ~&  >  'BREACH-1 DONE'
      ;<  [eth-node=_eth-node ~]  bind:m
        %+  (wrap-philter ,_eth-node ,~)
          router:eth-node
        (raw-real-ship:eth-node ~bud)
      ~&  >  'BUD RE DONE'
      ;<  eth-node=_eth-node  bind:m
        (breach-and-hear:eth-node our.hid ~marbud ~bud)
      ~&  >  'BREACH-2 DONE'
      ;<  [eth-node=_eth-node ~]  bind:m
        %+  (wrap-philter ,_eth-node ,~)
          router:eth-node
        ;<  ~        bind:m  (raw-real-ship:eth-node ~marbud)
        ~&  >  'MARBUD RE DONE'
        ;<  file=@t  bind:m  (touch-file ~bud %base)
        ~&  >  'TOUCH-1 DONE'
        ;<  file=@t  bind:m  (touch-file ~bud %base)
        ~&  >  'TOUCH-2 DONE'
        (check-file-touched ~marbud %home file)
      ~&  >  'DONE DONE'
      (pure:m ~)
    ::
      :+  %breach-sudden
        ~[~bud ~marbud]
      =.  eth-node  (spawn:eth-node ~marbud)
      =.  eth-node  (spawn:eth-node ~fipfes)
      ;<  [eth-node=_eth-node ~]  bind:m
        %+  (wrap-philter ,_eth-node ,~)
          router:eth-node
        ;<  ~        bind:m  (raw-real-ship:eth-node ~bud)
        ~&  >  'BUD DONE'
        ;<  ~        bind:m  (raw-real-ship:eth-node ~marbud)
        ~&  >  'MARBUD DONE'
        ;<  file=@t  bind:m  (touch-file ~bud %base)
        ~&  >  'TOUCH FILE DONE'
        (check-file-touched ~marbud %home file)
      ~&  >  'TOUCH FILE CHECK DONE'
      =.  eth-node  (breach:eth-node ~bud)
      ~&  >  'BREACH EXECUTED'
      ;<  [eth-node=_eth-node ~]  bind:m
        %+  (wrap-philter ,_eth-node ,~)
          router:eth-node
        ;<  ~        bind:m  (raw-real-ship:eth-node ~bud)
        ~&  >  'BUD RE DONE'
        ;<  ~        bind:m  (just-events (dojo ~bud "|merge %base ~marbud %kids, =gem %this"))
        ~&  >  'THIS MERGE STARTED DONE'
        ;<  file=@t  bind:m  (touch-file ~bud %base)
        ~&  >  'TOUCH-1 DONE'
        ;<  file=@t  bind:m  (touch-file ~bud %base)
        ~&  >  'TOUCH-2 DONE'
        (check-file-touched ~marbud %home file)
      ~&  >  'DONE DONE'
      (pure:m ~)
  ==
::
++  install-tests
  ^+  this
  =.  tests  (malt manual-tests)
  this
::
++  prep
  |=  old=(unit [@ tests=* rest=*])
  ^-  (quip move _this)
  ~&  prep=%ph
  =.  this  install-tests
  `this
  ::  ?~  old
  ::    `this
  ::  =/  new  ((soft other-state) rest.u.old)
  ::  ?~  new
  ::    `this
  ::  `this(+<+>+> u.new)
::
++  publish-aqua-effects
  |=  afs=aqua-effects
  ^-  (list move)
  %+  murn  ~(tap by sup.hid)
  |=  [b=bone her=ship pax=path]
  ^-  (unit move)
  ?.  ?=([%effects ~] pax)
    ~
  `[b %diff %aqua-effects afs]
::
++  run-events
  |=  [lab=term what=(list ph-event)]
  ^-  (quip move _this)
  ?:  =(~ what)
    `this
  =/  res
    |-  ^-  (each (list aqua-event) ?)
    ?~  what
      [%& ~]
    ?:  ?=(%test-done -.i.what)
      [%| p.i.what]
    =/  nex  $(what t.what)
    ?:  ?=(%| -.nex)
      nex
    [%& `aqua-event`i.what p.nex]
  ?:  ?=(%| -.res)
    =^  moves-1  this  (finish-test lab p.res)
    =^  moves-2  this  run-test
    [(weld moves-1 moves-2) this]
  [[ost.hid %poke /running [our.hid %aqua] %aqua-events p.res]~ this]
::
::  Cancel subscriptions to ships
::
++  finish-test
  |=  [lab=term success=?]
  ^-  (quip move _this)
  ?~  test-core
    `this
  ~&  ?:  success
        "TEST {(trip lab)} SUCCESSFUL"
      "TEST {(trip lab)} FAILED"
  :_  this(test-core ~, results [[lab success] results])
  %-  zing
  %+  turn  hers.u.test-core
  |=  her=ship
  ^-  (list move)
  :~  [ost.hid %pull /[lab]/(scot %p her) [our.hid %aqua] ~]
      :*  ost.hid
          %poke
          /cancelling
          [our.hid %aqua]
          %aqua-events
          [%pause-events her]~
      ==
  ==
::
::  Start another test if one is in the queue
::
++  run-test
  ^-  (quip move _this)
  ?^  test-core
    `this
  ?:  =(~ test-qeu)
    ?~  results
      `this
    =/  throw-away  print-results
    `this(results ~)
  =^  lab  test-qeu  ~(get to test-qeu)
  ~&  [running-test=lab test-qeu]
  =.  effect-log  ~
  =+  ^-  [ships=(list ship) test=_*form:(ph ,~)]
      (~(got by tests) lab)
  =>  .(test-core `(unit test-core-state)`test-core)
  =.  test-core  `[lab ships test]
  =^  moves-1  this  (subscribe-to-effects lab ships)
  =^  moves-2  this
    (diff-aqua-effects /[lab]/(scot %p -.ships) -.ships [/ %init ~]~)
  [:(weld init-vanes pause-fleet subscribe-vanes moves-1 moves-2) this]
::
::  Print results with ~&
::
++  print-results
  ~&  "TEST REPORT:"
  =/  throw-away
    %+  turn
      results
    |=  [lab=term success=?]
    ~&  "{?:(success "SUCCESS" "FAILURE")}: {(trip lab)}"
    ~
  ~&  ?:  (levy results |=([term s=?] s))
        "ALL TESTS SUCCEEDED"
      "FAILURES"
  ~
::
::  Should check whether we're already subscribed
::
++  subscribe-to-effects
  |=  [lab=@tas hers=(list ship)]
  :_  this
  %+  turn  hers
  |=  her=ship
  ^-  move
  :*  ost.hid
      %peer
      /[lab]/(scot %p her)
      [our.hid %aqua]
      /effects/(scot %p her)
  ==
::
::  Start the vane drivers
::
++  init-vanes
  ^-  (list move)
  %+  murn
    `(list term)`[%aqua vane-apps]
  |=  vane-app=term
  ^-  (unit move)
  =/  app-started
    .^(? %gu /(scot %p our.hid)/[vane-app]/(scot %da now.hid))
  ?:  app-started
    ~
  `[ost.hid %poke /start [our.hid %hood] %drum-start %home vane-app]
::
::  Restart the vane drivers' subscriptions
::
++  subscribe-vanes
  ^-  (list move)
  %+  turn
    vane-apps
  |=  vane-app=term
  [ost.hid %poke /init [our.hid vane-app] %aqua-vane-control %subscribe]
::
::  Pause all existing ships
::
++  pause-fleet
  ^-  (list move)
  :_  ~
  :*  ost.hid  %poke  /pause-fleet  [our.hid %aqua]  %aqua-events
      %+  turn
        .^((list ship) %gx /(scot %p our.hid)/aqua/(scot %da now.hid)/ships/noun)
      |=  who=ship
      [%pause-events who]
  ==
::
::  User interface
::
++  poke-ph-command
  |=  com=cli:ph-sur
  ^-  (quip move _this)
  ?-  -.com
      %init  [init-vanes this]
      %run
    ?.  (~(has by tests) lab.com)
      ~&  [%no-test lab.com]
      `this
    =.  test-qeu  (~(put to test-qeu) lab.com)
    run-test
  ::
      %cancel
    =^  moves-1  this  (finish-test %last |)
    =.  test-qeu  ~
    =^  moves-2  this  run-test
    [:(weld moves-1 moves-2) this]
  ::
      %run-all
    =.  test-qeu
      %-  ~(gas to test-qeu)
      (turn manual-tests head)
    run-test
  ::
      %print
    ~&  lent=(lent effect-log)
    ~&  %+  roll  effect-log
        |=  [[who=ship uf=unix-effect] ~]
        ?:  ?=(?(%blit %doze) -.q.uf)
          ~
        ?:  ?=(%ergo -.q.uf)
          ~&  [who [- +<]:uf %omitted-by-ph]
          ~
        ~&  [who uf]
        ~
    `this
  ==
::
::  Receive effects back from aqua
::
++  diff-aqua-effects
  |=  [way=wire afs=aqua-effects]
  ^-  (quip move _this)
  ::  ~&  [%diff-aqua-effect way who.afs]
  ?>  ?=([@tas @ ~] way)
  =/  lab  i.way
  ?~  test-core
    ~&  [%ph-dropping-done lab]
    [[ost.hid %pull way [our.hid %aqua] ~]~ this]
  ?.  =(lab lab.u.test-core)
    ~&  [%ph-dropping-strange lab]
    [[ost.hid %pull way [our.hid %aqua] ~]~ this]
  =+  |-  ^-  $:  thru-effects=(list unix-effect)
                  events=(list ph-event)
                  log=_effect-log
                  done=(unit ?)
                  test=_test.u.test-core
              ==
      ?~  ufs.afs
        [~ ~ ~ ~ test.u.test-core]
      =/  m-res=_*output:(ph ,~)
        (test.u.test-core now.hid who.afs i.ufs.afs)
      =?  ufs.afs  =(%cont -.next.m-res)
        [i.ufs.afs [/ %init ~] t.ufs.afs]
      =^  done=(unit ?)  test.u.test-core
        ?-    -.next.m-res
          %wait  [~ test.u.test-core]
          %cont  [~ self.next.m-res]
          %fail  [`| test.u.test-core]
          %done  [`& test.u.test-core]
        ==
      =+  ^-  _$
          ?~  done
            $(ufs.afs t.ufs.afs)
          [~ ~ ~ done test.u.test-core]
      :^    ?:  thru.m-res
              [i.ufs.afs thru-effects]
            thru-effects
          (weld events.m-res events)
        [[who i.ufs]:afs log]
      [done test]
    =.  test.u.test-core  test
    =.  effect-log  (weld log effect-log)
    =>  .(test-core `(unit test-core-state)`test-core)
    ?^  done
      =^  moves-1  this  (finish-test lab u.done)
      =^  moves-2  this  run-test
      [(weld moves-1 moves-2) this]
    =/  moves-1  (publish-aqua-effects who.afs thru-effects)
    =^  moves-2  this  (run-events lab events)
    [(weld moves-1 moves-2) this]
::
::  Subscribe to effects
::
++  peer-effects
  |=  pax=path
  ^-  (quip move _this)
  ?.  ?=(~ pax)
    ~&  [%ph-bad-peer-effects pax]
    `this
  `this
::
::  Subscription cancelled
::
++  pull
  |=  pax=path
  `+>.$
--
