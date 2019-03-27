::  Test the pH of your aquarium.  See if it's safe to put in real fish.
::
::  usage:
::  :aqua [%run-test %test-add]
::
::  TODO:
::  - Restore a fleet
::  - Compose tests
::
/-  aquarium
/+  ph
=,  aquarium
=,  ph
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
          raw-test-cores=(map term raw-test-core)
          test-core=(unit test-core-state)
          other-state
      ==
    ::
    +$  test-core-state
      $:  hers=(list ship)
          cor=raw-test-core
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
++  test-lib  ~(. ^test-lib our.hid)
::
::  Tests that will be run automatically with :ph %run-all-tests
::
++  auto-tests
  =,  test-lib
  ^-  (list (pair term raw-test-core))
  :~
    :-  %boot-bud
    (galaxy ~bud)
  ::
    :-  %add
    ^-  raw-test-core
    %+  compose-tests  (galaxy ~bud)
    %+  stateless-test
      %add
    |_  now=@da
    ++  start
      (dojo ~bud "[%test-result (add 2 3)]")
    ::
    ++  route
      |=  [who=ship uf=unix-effect]
      (expect-dojo-output ~bud who uf "[%test-result 5]")
    --
  ::
    :-  %hi
    %+  compose-tests
      %+  compose-tests
        (galaxy ~bud)
      (galaxy ~dev)
    %+  stateless-test
      %hi
    |_  now=@da
    ++  start
      (dojo ~bud "|hi ~dev")
    ::
    ++  route
      |=  [who=ship uf=unix-effect]
      (expect-dojo-output ~bud who uf "hi ~dev successful")
    --
  ::
    :-  %boot-planet
    (planet ~linnup-torsyx)
  ::
    :-  %hi-grandparent
    %+  compose-tests  (planet ~linnup-torsyx)
    %+  stateless-test
      %hi-grandparent
    |_  now=@da
    ++  start
      (dojo ~linnup-torsyx "|hi ~bud")
    ::
    ++  route
      |=  [who=ship uf=unix-effect]
      (expect-dojo-output ~linnup-torsyx who uf "hi ~bud successful")
    --
  ::
    :-  %second-cousin-hi
    %+  compose-tests
      %+  compose-tests  (planet ~mitnep-todsut)
        (planet ~haplun-todtus)
    %+  stateless-test
      %second-cousin-hi
    |_  now=@da
    ++  start
      (dojo ~haplun-todtus "|hi ~mitnep-todsut")
    ::
    ++  route
      |=  [who=ship uf=unix-effect]
      (expect-dojo-output ~haplun-todtus who uf "hi ~mitnep-todsut successful")
    --
  ::
    :-  %change-file
    %+  compose-tests  (galaxy ~bud)
    (touch-file ~bud %home)
  ::
    :-  %child-sync
    %+  compose-tests
      %+  compose-tests
        (star ~marbud)
      (touch-file ~bud %base)
    (check-file-touched ~marbud %home)
  ==
::
::  Tests that will not be run automatically.
::
::    Some valid reasons for not running a test automatically:
::    - Nondeterministic
::    - Depends on external services
::    - Is very slow
::
++  manual-tests
  =,  test-lib
  ^-  (list (pair term raw-test-core))
  :~  :-  %boot-from-azimuth
      %-  compose-tests
      :_  *raw-test-core
      %^    wrap-test-http
          'http://localhost:8545'
        %-  malt
        ^-  (list [@t @t])
        :~  :-  '{"params":[],"id":"block number","jsonrpc":"2.0","method":"eth_blockNumber"}'
            'response-1'
            :-  '{"params":[{"fromBlock":"0x0","address":"0x863d9c2e5c4c133596cfac29d55255f0d0f86381","toBlock":"0x7"}],"id":"catch up","jsonrpc":"2.0","method":"eth_getLogs"}'
            'response-1'
            :-  '{"params":[{"fromBlock":"0x0","address":"0x863d9c2e5c4c133596cfac29d55255f0d0f86381"}],"id":"new filter","jsonrpc":"2.0","method":"eth_newFilter"}'
            'response-1'
            :-  '{"params":["0x07"],"id":"filter logs","jsonrpc":"2.0","method":"eth_getFilterLogs"}'
            'response-1'
        ==
      %+  compose-tests
        (raw-ship ~bud `(dawn:azimuth ~bud))
      (touch-file ~bud %home)
    ::
      :-  %simple-add
      %+  compose-tests  (galaxy ~bud)
      %+  stateless-test
        %add
      ^-  stateless-test-core
      |_  now=@da
      ++  start
        =/  command  "[%test-result (add 2 3)]"
        :~  [%event ~bud //term/1 %belt %txt ((list @c) command)]
            [%event ~bud //term/1 %belt %ret ~]
        ==
      ::
      ++  route
        |=  [who=ship uf=unix-effect]
        ?.  (is-dojo-output ~bud who uf "[%test-result 5]")
          ~
        [%test-done &]~
      --
    ::
      :-  %count
      %+  compose-tests  (galaxy ~bud)
      %+  porcelain-test
        %state
      =|  count=@
      |_  now=@da
      ++  start
        ^-  (quip ph-event _..start)
        [(dojo ~bud "\"count: {<count>}\"") ..start]
      ::
      ++  route
        |=  [who=ship uf=unix-effect]
        ^-  (quip ph-event _..start)
        ?.  (is-dojo-output ~bud who uf "\"count: {<count>}\"")
          [~ ..start]
        ?:  (gte count 10)
          [[%test-done &]~ ..start]
        =.  count  +(count)
        start
      --
    ::
      :-  %break-behn
      %+  compose-tests
        %+  compose-tests
          (galaxy ~bud)
        (galaxy ~dev)
      ^-  raw-test-core
      |_  now=@da
      ++  label  %break-behn
      ++  ships  ~
      ++  start
        [(dojo ~bud "|hi ~dev") ..start]
      ::
      ++  route
        |=  [who=ship uf=unix-effect]
        ^-  [? (quip ph-event _..start)]
        ?:  ?=(%doze -.q.uf)
          [| ~ ..start]
        :-  &  :_  ..start
        (expect-dojo-output ~bud who uf "hi ~dev successful")
      --
  ==
::
++  install-tests
  ^+  this
  =.  raw-test-cores
    (~(uni by (malt auto-tests)) (malt manual-tests))
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
      ~&  ?~  p.i.what
            "TEST {(trip lab)} SUCCESSFUL"
          "TEST {(trip lab)} FAILED"
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
  =/  res=[events=(list ph-event) new-state=raw-test-core]
    ~(start (~(got by raw-test-cores) lab) now.hid)
  =>  .(test-core `(unit test-core-state)`test-core)
  =.  test-core  `[ships .]:new-state.res
  =^  moves-1  this  (subscribe-to-effects lab ships.new-state.res)
  =^  moves-2  this  (run-events lab events.res)
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
++  poke-noun
  |=  arg=*
  ^-  (quip move _this)
  ?+  arg  ~|(%bad-noun-arg !!)
      %init
    [init-vanes this]
  ::
      %run-all-tests
    =.  test-qeu
      %-  ~(gas to test-qeu)
      (turn auto-tests head)
    run-test
  ::
      [%run-test lab=@tas]
    ?.  (~(has by raw-test-cores) lab.arg)
      ~&  [%no-test lab.arg]
      `this
    =.  test-qeu  (~(put to test-qeu) lab.arg)
    run-test
  ::
      %cancel
    =^  moves-1  this  (finish-test %last |)
    =.  test-qeu  ~
    =^  moves-2  this  run-test
    [:(weld moves-1 moves-2) this]
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
    ~&  [%ph-dropping lab]
    `this
  =+  |-  ^-  $:  thru-effects=(list unix-effect)
                  events=(list ph-event)
                  cor=_u.test-core
                  log=_effect-log
              ==
      ?~  ufs.afs
        [~ ~ u.test-core ~]
      =+  ^-  [thru=? events-1=(list ph-event) cor=_cor.u.test-core]
          (~(route cor.u.test-core now.hid) who.afs i.ufs.afs)
      =.  cor.u.test-core  cor
      =+  $(ufs.afs t.ufs.afs)
      :^    ?:  thru
              [i.ufs.afs thru-effects]
            thru-effects
          (weld events-1 events)
        cor
      [[who i.ufs]:afs log]
  =.  test-core  `cor
  =.  effect-log  (weld log effect-log)
  =>  .(test-core `(unit test-core-state)`test-core)
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
