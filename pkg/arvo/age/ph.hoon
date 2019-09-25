::  Test the pH of your aquarium.  See if it's safe to put in real fish.
::
::  usage:
::  :aqua [%run-test %test-add]
::
/-  aquarium, ph
/+  ph, ph-tests, ph-azimuth, ph-philter, default-agent
=,  ph-sur=^ph
=,  aquarium
=,  ph
=,  ph-philter
=>  $~  |%
    +$  card  card:agent:mall
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
::
=;  ph-core
  =|  =all=state
  ^-  agent:mall
  |_  =bowl:mall
  +*  this  .
      def  ~(. default-agent bowl this)
  ++  handle-init           `this
  ++  handle-extract-state  !>(all-state)
  ++  handle-upgrade-state
    |=  old-state=vase
    ^-  step:agent:mall
    ~&  prep=%ph
    =.  tests.all-state  (malt ~(manual-tests ph-core bowl all-state))
    `this
  ::
  ++  handle-poke
    |=  [=mark =vase]
    ^-  step:agent:mall
    ?.  ?=(%ph-command mark)
      (handle-poke:def mark vase)
    =^  cards  ph-core
      (~(poke-ph-command ph-core bowl all-state) !<(cli:ph-sur vase))
    [cards this(all-state +<+.ph-core)]
  ::
  ++  handle-subscribe
    |=  =path
    ^-  step:agent:mall
    ?.  ?=([%effects ~] path)
      ~|  [%ph-bad-subscribe-path path]
      !!
    `this
  ::
  ++  handle-unsubscribe     handle-unsubscribe:def
  ++  handle-peek            handle-peek:def
  ++  handle-agent-response
    |=  [=wire =gift:agent:mall]
    ^-  step:agent:mall
    ?.  ?=([%subscription-update * %aqua-effects *] gift)
      (handle-agent-response:def wire gift)
    =^  cards  ph-core
      %+  ~(diff-aqua-effects ph-core bowl all-state)
        wire
      !<(aqua-effects q.cage.gift)
    [cards this(all-state +<+.ph-core)]
  ::
  ++  handle-arvo-response   handle-arvo-response:def
  ++  handle-error           handle-error:def
  --
::
=/  vane-apps=(list term)
  ~[%aqua-ames %aqua-behn %aqua-dill %aqua-eyre]
|_  $:  hid=bowl:mall
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
      ~&  >  "BUD DONE"
      ;<  ~  bind:m  (raw-ship ~dev ~)
      ~&  >  "DEV DONE"
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
      =.  eth-node  (spawn:eth-node ~dev)
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
        ;<  ~  bind:m  (raw-real-ship:eth-node ~dev)
        ~&  >  'DEV DONE'
        ;<  ~  bind:m  (send-hi ~linnup-torsyx-linnup-torsyx ~dev)
        ~&  >  'HI OVER UP DONE'
        ;<  ~  bind:m  (send-hi ~dev ~linnup-torsyx-linnup-torsyx)
        ~&  >  'HI OVER DOWN DONE'
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
++  publish-aqua-effects
  |=  afs=aqua-effects
  ^-  (list card)
  [%give %subscription-update `/effects %aqua-effects !>(afs)]~
::
++  run-events
  |=  [lab=term what=(list ph-event)]
  ^-  (quip card _this)
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
    =^  cards-1  this  (finish-test lab p.res)
    =^  cards-2  this  run-test
    [(weld cards-1 cards-2) this]
  [[%pass /running %agent [our.hid %aqua] %poke %aqua-events !>(p.res)]~ this]
::
::  Cancel subscriptions to ships
::
++  finish-test
  |=  [lab=term success=?]
  ^-  (quip card _this)
  ?~  test-core
    `this
  ~&  ?:  success
        "TEST {(trip lab)} SUCCESSFUL"
      "TEST {(trip lab)} FAILED"
  :_  this(test-core ~, results [[lab success] results])
  %-  zing
  %+  turn  hers.u.test-core
  |=  her=ship
  ^-  (list card)
  :~  [%pass /[lab]/(scot %p her) %agent [our.hid %aqua] %unsubscribe ~]
      :*  %pass
          /cancelling
          %agent
          [our.hid %aqua]
          %poke
          %aqua-events
          !>([%pause-events her]~)
      ==
  ==
::
::  Start another test if one is in the queue
::
++  run-test
  ^-  (quip card _this)
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
  =^  cards-1  this  (subscribe-to-effects lab ships)
  =^  cards-2  this
    (diff-aqua-effects /[lab]/(scot %p -.ships) -.ships [/ %init ~]~)
  [:(weld init-vanes pause-fleet subscribe-vanes cards-1 cards-2) this]
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
  ^-  card
  :*  %pass
      /[lab]/(scot %p her)
      %agent
      [our.hid %aqua]
      %subscribe
      /effects/(scot %p her)
  ==
::
::  Start the vane drivers
::
++  init-vanes
  ^-  (list card)
  %+  murn
    `(list term)`[%aqua vane-apps]
  |=  vane-app=term
  ^-  (unit card)
  =/  app-started
    .^(? %mu /(scot %p our.hid)/[vane-app]/(scot %da now.hid))
  ?:  app-started
    ~
  `[%pass /start %agent [our.hid %hood] %poke %drum-start !>([%home vane-app])]
::
::  Restart the vane drivers' subscriptions
::
++  subscribe-vanes
  ^-  (list card)
  %+  turn
    vane-apps
  |=  vane-app=term
  :*  %pass  /init
      %agent  [our.hid vane-app]
      %poke  %aqua-vane-control
      !>(%subscribe)
  ==
::
::  Pause all existing ships
::
++  pause-fleet
  ^-  (list card)
  :_  ~
  :*  %pass  /pause-fleet
      %agent  [our.hid %aqua]
      %poke  %aqua-events  !>
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
  ^-  (quip card _this)
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
    =^  cards-1  this  (finish-test %last |)
    =.  test-qeu  ~
    =^  cards-2  this  run-test
    [:(weld cards-1 cards-2) this]
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
  ^-  (quip card _this)
  ::  ~&  [%diff-aqua-effect way who.afs]
  ?>  ?=([@tas @ ~] way)
  =/  lab  i.way
  ?~  test-core
    ~&  [%ph-dropping-done lab]
    [[%pass way %agent [our.hid %aqua] %unsubscribe ~]~ this]
  ?.  =(lab lab.u.test-core)
    ~&  [%ph-dropping-strange lab]
    [[%pass way %agent [our.hid %aqua] %unsubscribe ~]~ this]
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
      =^  cards-1  this  (finish-test lab u.done)
      =^  cards-2  this  run-test
      [(weld cards-1 cards-2) this]
    =/  cards-1  (publish-aqua-effects who.afs thru-effects)
    =^  cards-2  this  (run-events lab events)
    [(weld cards-1 cards-2) this]
--
