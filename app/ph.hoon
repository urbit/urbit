::  Test the pH of your aquarium.  See if it's safe to put real fish in.
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
    ++  move  (pair bone card)
    ++  card
      $%  [%poke wire dock %aqua-events (list aqua-event)]
          [%peer wire dock path]
          [%pull wire dock ~]
      ==
    ::
    ++  state
      $:  %0
          raw-test-cores=(map term test-core)
          test-cores=(map term [hers=(list ship) cor=test-core])
          other-state
      ==
    ++  other-state
      $~
    --
=,  gall
|_  $:  hid=bowl
        state
    ==
++  this  .
++  install-tests
  ^+  this
  =.  raw-test-cores
    %-  malt
    ^-  (list (pair term test-core))
    :~
      :-  %add
      =+  num=5
      |%
      ++  start
        ^-  (trel (list ship) (list ph-event) _..start)
        =.  num  +(num)
        :+  ~[~bud]
          %-  zing
          :~  (init ~bud)
              (dojo ~bud "[%test-result (add 2 3)]")
          ==
        ..start
      ::
      ++  route
        |=  [who=ship ovo=unix-effect]
        ^-  (list ph-event)
        ~&  [%num num]
        (expect-dojo-output ~bud who ovo "[%test-result 5]")
        ::  XX  if it's been five minutes, we failed
      --
    ::
      :-  %hi
      |%
      ++  start
        ^-  (trel (list ship) (list ph-event) _..start)
        :+  ~[~bud ~dev]
          %-  zing
          :~  (init ~bud)
              (init ~dev)
              (dojo ~bud "|hi ~dev")
          ==
        ..start
      ::
      ++  route
        |=  [who=ship ovo=unix-effect]
        ^-  (list ph-event)
        (expect-dojo-output ~bud who ovo "hi ~dev successful")
      --
    ::
      :-  %child-sync
      |%
      ++  start
        ^-  (trel (list ship) (list ph-event) _..start)
        :+  ~[~bud ~marbud]
          %-  zing
          :~  (init ~bud)
              ::  (dojo ~bud "\"magic-go\":[.^(")
              ::  (dojo ~bud "|mount %")
              ::  %+  insert-file  ~bud
              ::  /(scot %p our.hid)/home/(scot %da now.hid)/sys/vane/clay/hoon
              ::  (init ~marbud)
              ::  (dojo ~marbud "|mount %")
              ::  %+  insert-file  ~marbud
              ::  /(scot %p our.hid)/home/(scot %da now.hid)/sys/vane/clay/hoon
          ==
        ..start
      ++  route
        |=  [who=ship ovo=unix-effect]
        ^-  (list ph-event)
        ::
        ::  This is actually super fragile.  If we start ~marbud any
        ::  earlier in the process, we get a crash.  The crash may be
        ::  harmless, not sure.
        ::
        %-  on-dojo-output
        :^  ~bud  who  ovo
        :-  "~zod not responding still trying"
        ^-  $-($~ (list ph-event))
        |=  ~
        (init ~marbud)
      --
      ::  (init ~zod)
      ::  (init ~marzod)
      ::  wait for initial sync
      ::  change file on zod
      ::  check on ~marzod
    ::
      :-  %individual-breach
      *test-core
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
      [%run-test lab=@tas]
    =/  res=[hers=(list ship) events=(list ph-event) new-state=test-core]
      start:(~(got by raw-test-cores) lab.arg)
    =.  test-cores  (~(put by test-cores) lab.arg hers.res new-state.res)
    =^  moves-1  this  (subscribe-to-effects lab.arg hers.res)
    =^  moves-2  this  (run-events lab.arg events.res)
    [(weld moves-1 moves-2) this]
  ==
::
++  diff-aqua-effects
  |=  [way=wire ova=aqua-effects]
  ^-  (quip move _this)
  ::  ~&  [%diff-aqua-effect way who.ova]
  ?>  ?=([@tas @ ~] way)
  =/  lab  i.way
  %+  run-events  lab
  |-  ^-  (list ph-event)
  ?~  ovo.ova
    ~
  ::  ~&  [%diff-aqua-effect-i way -.q.i.ovo.ova]
  %+  weld
    (route:cor:(~(got by test-cores) lab) who.ova i.ovo.ova)
  $(ovo.ova t.ovo.ova)
--
