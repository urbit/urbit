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
      $%  [%peer wire dock path]
          [%poke wire dock %aqua-events (list aqua-event)]
      ==
    ::
    ++  test-map  (map term test-core)
    ::
    ++  state
      $:  %0
          test-cores=test-map
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
  =.  test-cores
    %-  malt
    ^-  (list (pair term test-core))
    :~
      :-  %test-add
      |%
      ++  start
        ^-  (pair (list ship) (list ph-event))
        :-  ~[~bud]
        %-  zing
        :~  (init ~bud)
            (dojo ~bud "[%test-result (add 2 3)]")
        ==
      ::
      ++  route
        |=  [who=ship ovo=unix-effect]
        ^-  (list ph-event)
        (expect-dojo-output ~bud who ovo "[%test-result 5]")
        ::  XX  if it's been five minutes, we failed
      --
    ::
      :-  %test-hi
      |%
      ++  start
        ^-  (pair (list ship) (list ph-event))
        :-  ~[~bud ~dev]
        %-  zing
        :~  (init ~bud)
            (init ~dev)
            (dojo ~bud "|hi ~dev")
        ==
      ::
      ++  route
        |=  [who=ship ovo=unix-effect]
        ^-  (list ph-event)
        ::
        ::  doesn't work because for some reason we lose the
        ::  subscription immediately after opening it.  maybe
        ::  because we receive so many events without immediate
        ::  reap it triggers the backpressure mechanism in gall?
        ::
        (expect-dojo-output ~bud who ovo "hi ~dev successful")
      --
    ==
  this
::
++  prep
  |=  old=(unit [@ tests=* rest=*])
  ^-  [(list move) _this]
  =.  this  install-tests
  ?~  old
    `this
  =/  new  ((soft other-state) rest.u.old)
  ?~  new
    `this
  `this(+<+>+ u.new)
::
++  run-events
  |=  what=(list ph-event)
  ^-  [(list move) _this]
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
    `this
  [[ost.hid %poke /running [our.hid %aqua] %aqua-events p.res]~ this]
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
    =/  res=[hers=(list ship) events=(list ph-event)]
      start:(~(got by test-cores) lab.arg)
    =^  moves-1  this  (subscribe-to-effects lab.arg hers.res)
    =^  moves-2  this  (run-events events.res)
    [(weld moves-1 moves-2) this]
  ==
::
++  diff-aqua-effects
  |=  [way=wire ova=aqua-effects]
  ^-  (quip move _this)
  ::  ~&  [%diff-aqua-effect way who.ova]
  ?>  ?=([@ @ ~] way)
  =/  lab  i.way
  %-  run-events
  |-  ^-  (list ph-event)
  ?~  ovo.ova
    ~
  ~&  [%diff-aqua-effect-i way -.q.i.ovo.ova]
  %+  weld
    (route:(~(got by test-cores) lab) who.ova i.ovo.ova)
  $(ovo.ova t.ovo.ova)
--
