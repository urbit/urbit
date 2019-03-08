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
          test-cores=(map term test-core-state)
          other-state
      ==
    ::
    +$  test-core-state
      $:  hers=(list ship)
          cor=raw-test-core
          effect-log=(list [who=ship uf=unix-effect])
      ==
    ::
    +$  other-state
      $~
    --
=,  gall
|_  $:  hid=bowl
        state
    ==
++  this  .
++  test-lib  ~(. ^test-lib our.hid)
++  install-tests
  ^+  this
  =.  raw-test-cores
    ~&  jael=.^(noun %j /(scot %p our.hid)/code/(scot %da now.hid)/(scot %p our.hid))
    =,  test-lib
    %-  malt
    ^-  (list (pair term raw-test-core))
    :~
      :-  %add
      ^-  raw-test-core
      =+  num=5
      |%
      ++  label  %add
      ++  ships  ~[~bud]
      ++  start
        |=  now=@da
        ^-  (pair (list ph-event) _..start)
        =.  num  +(num)
        :_  ..start
        %-  zing
        :~  (init ~bud ~)
            (dojo ~bud "[%test-result (add 2 3)]")
        ==
      ::
      ++  route
        |=  [now=@da who=ship uf=unix-effect]
        ^-  [? (quip ph-event _..start)]
        ~&  [%num num]
        :-  &
        :_  ..start
        (expect-dojo-output ~bud who uf "[%test-result 5]")
      --
    ::
      :-  %hi
      ^-  raw-test-core
      |%
      ++  label  %hi
      ++  ships  ~[~bud ~dev]
      ++  start
        |=  now=@da
        ^-  (pair (list ph-event) _..start)
        :_  ..start
        %-  zing
        :~  (init ~bud ~)
            (init ~dev ~)
            (dojo ~bud "|hi ~dev")
        ==
      ::
      ++  route
        |=  [now=@da who=ship uf=unix-effect]
        ^-  [? (quip ph-event _..start)]
        :-  &
        :_  ..start
        (expect-dojo-output ~bud who uf "hi ~dev successful")
      --
    ::
      [%headstart-bud (galaxy ~bud)]
    ::
      :-  %composed-child-boot
      %+  compose-tests  (planet ~linnup-torsyx)
      %+  porcelain-test  %composed-child-boot
      |%
      ++  start
        |=  now=@da
        [(dojo ~linnup-torsyx "|hi ~bud") ..start]
      ::
      ++  route
        |=  [now=@da who=ship uf=unix-effect]
        ^-  (quip ph-event _..start)
        :_  ..start
        %-  on-dojo-output
        :^  ~linnup-torsyx  who  uf
        :-  "hi ~bud successful"
        |=  ~
        [%test-done &]~
      --
    ::
      :-  %composed-child-boot-2
      %+  compose-tests
        %+  compose-tests  (planet ~mitnep-todsut)
          (planet ~haplun-todtus)
      %+  porcelain-test
        %composed-child-boot-2
      |%
      ++  start
        |=  now=@da
        [(dojo ~haplun-todtus "|hi ~bud") ..start]
      ::
      ++  route
        |=  [now=@da who=ship uf=unix-effect]
        ^-  (quip ph-event _..start)
        :_  ..start
        %-  on-dojo-output
        :^  ~haplun-todtus  who  uf
        :-  "hi ~bud successful"
        |=  ~
        [%test-done &]~
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
    ::
      :-  %boot-azimuth
      %+  compose-tests
        %+  compose-tests
          (raw-ship ~bud `(dawn:azimuth ~bud))
        (touch-file ~bud %home)
      ::  %-  assert-happens
      ::  :~
      ::  ==
      *raw-test-core
    ::
      :-  %individual-breach
      *raw-test-core
      ::
      ::  (init ~zod)
      ::  (init ~marzod)
      ::  wait for sync to finish
      ::  cycle ~zod keys
      ::  verify it sunk
      ::  kill ~zod
      ::  (init ~zod) w/new keys
      ::  change file on ~zod
      ::  wait for sync to finish
      ::  verify file has changed on ~marzod
      ::
    ==
  this
::
++  prep
  |=  old=(unit [@ tests=* rest=*])
  ^-  (quip move _this)
  =.  this  install-tests
  ?~  old
    `this
  =/  new  ((soft other-state) rest.u.old)
  ?~  new
    `this
  `this(+<+>+> u.new)
::
++  publish-aqua-effects
  |=  afs=aqua-effects
  ^-  (list move)
  %+  murn  ~(tap by sup.hid)
  |=  [b=bone her=ship pax=path]
  ^-  (unit move)
  ?.  ?=([%effects ~] pax)
    ~
  `[ost.hid %diff %aqua-effects afs]
::
++  run-events
  |=  [lab=term what=(list ph-event)]
  ^-  (quip move _this)
  ?:  =(~ what)
    `this
  =/  res
    |-  ^-  (each (list aqua-event) $~)
    ?~  what
      [%& ~]
    ?:  ?=(%test-done -.i.what)
      ~&  ?~(p.i.what "test successful" "test failed")
      [%| ~]
    =/  nex  $(what t.what)
    ?:  ?=(%| -.nex)
      nex
    [%& `aqua-event`i.what p.nex]
  ?:  ?=(%| -.res)
    (cancel-test lab)
  [[ost.hid %poke /running [our.hid %aqua] %aqua-events p.res]~ this]
::
::  Cancel subscriptions to ships
::
++  cancel-test
  |=  lab=term
  ^-  (quip move _this)
  =/  test  (~(get by test-cores) lab)
  ?~  test
    `this
  =.  test-cores  (~(del by test-cores) lab)
  :_  this
  %-  zing
  %+  turn  hers.u.test
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
++  poke-noun
  |=  arg=*
  ~&  %herm
  ^-  (quip move _this)
  ?+  arg  ~|(%bad-noun-arg !!)
      %init
    :_  this
    %-  zing  ^-  (list (list move))
    %+  turn
      ^-  (list term)
      ~[%aqua-ames %aqua-behn %aqua-dill %aqua-eyre]
    |=  vane-app=term
    :~  [ost.hid %poke /start [our.hid %hood] %drum-start %home vane-app]
        [ost.hid %poke /init [our.hid vane-app] %aqua-vane-control %subscribe]
    ==
  ::
      [%run-test lab=@tas]
    =/  res=[events=(list ph-event) new-state=raw-test-core]
      (start:(~(got by raw-test-cores) lab.arg) now.hid)
    =.  test-cores  (~(put by test-cores) lab.arg [ships . ~]:new-state.res)
    =^  moves-1  this  (subscribe-to-effects lab.arg ships.new-state.res)
    =^  moves-2  this  (run-events lab.arg events.res)
    [(weld moves-1 moves-2) this]
  ::
      [%print lab=@tas]
    =/  log  effect-log:(~(got by test-cores) lab.arg)
    ~&  lent=(lent log)
    ~&  %+  roll  log
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
++  diff-aqua-effects
  |=  [way=wire afs=aqua-effects]
  ^-  (quip move _this)
  ::  ~&  [%diff-aqua-effect way who.afs]
  ?>  ?=([@tas @ ~] way)
  =/  lab  i.way
  =/  test-cor  (~(get by test-cores) lab)
  ?~  test-cor
    ~&  [%ph-dropping lab]
    `this
  =+  |-  ^-  $:  thru-effects=(list unix-effect)
                  events=(list ph-event)
                  cor=_u.test-cor
              ==
      ?~  ufs.afs
        [~ ~ u.test-cor]
      =.  effect-log.u.test-cor
        [[who i.ufs]:afs effect-log.u.test-cor]
      =+  ^-  [thru=? events-1=(list ph-event) cor=_cor.u.test-cor]
          (route:cor.u.test-cor now.hid who.afs i.ufs.afs)
      =.  cor.u.test-cor  cor
      =+  $(ufs.afs t.ufs.afs)
      :+  ?:  thru
            [i.ufs.afs thru-effects]
          thru-effects
        (weld events-1 events)
      cor
  =.  u.test-cor  cor
  =.  test-cores  (~(put by test-cores) lab u.test-cor)
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
--
