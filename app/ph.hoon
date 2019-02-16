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
++  test-lib  ~(. ^test-lib our.hid)
++  install-tests
  ^+  this
  =.  raw-test-cores
    ~&  jael=.^(noun %j /(scot %p our.hid)/code/(scot %da now.hid)/(scot %p our.hid))
    =,  test-lib
    %-  malt
    ^-  (list (pair term test-core))
    :~
      :-  %add
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
        :~  (init ~bud)
            (dojo ~bud "[%test-result (add 2 3)]")
        ==
      ::
      ++  route
        |=  [now=@da who=ship ovo=unix-effect]
        ^-  (quip ph-event _..start)
        ~&  [%num num]
        :_  ..start
        (expect-dojo-output ~bud who ovo "[%test-result 5]")
        ::  XX  if it's been five minutes, we failed
      --
    ::
      :-  %hi
      |%
      ++  label  %hi
      ++  ships  ~[~bud ~dev]
      ++  start
        |=  now=@da
        ^-  (pair (list ph-event) _..start)
        :_  ..start
        %-  zing
        :~  (init ~bud)
            (init ~dev)
            (dojo ~bud "|hi ~dev")
        ==
      ::
      ++  route
        |=  [now=@da who=ship ovo=unix-effect]
        ^-  (quip ph-event _..start)
        :_  ..start
        (expect-dojo-output ~bud who ovo "hi ~dev successful")
      --
    ::
      [%headstart-bud (galaxy ~bud)]
    ::
      :-  %composed-child-boot
      %+  compose-tests  (planet ~linnup-torsyx)
      ^-  test-core
      |%
      ++  label  %composed-child-boot
      ++  ships  ~
      ++  start
        |=  now=@da
        [(dojo ~linnup-torsyx "|hi ~bud") ..start]
      ::
      ++  route
        |=  [now=@da who=ship ovo=unix-effect]
        ^-  (quip ph-event _..start)
        :_  ..start
        %-  on-dojo-output
        :^  ~linnup-torsyx  who  ovo
        :-  "hi ~bud successful"
        |=  ~
        [%test-done &]~
      --
    ::
      :-  %composed-child-boot-2
      %+  compose-tests
        %+  compose-tests  (planet ~mitnep-todsut)
          (planet ~haplun-todtus)
      ^-  test-core
      |%
      ++  label  %composed-child-boot-2
      ++  ships  ~
      ++  start
        |=  now=@da
        [(dojo ~haplun-todtus "|hi ~bud") ..start]
      ::
      ++  route
        |=  [now=@da who=ship ovo=unix-effect]
        ^-  (quip ph-event _..start)
        :_  ..start
        %-  on-dojo-output
        :^  ~haplun-todtus  who  ovo
        :-  "hi ~bud successful"
        |=  ~
        [%test-done &]~
      --
    ::
      :-  %change-file
      %+  compose-tests  (galaxy ~bud)
      (touch-file ~bud)
    ::
      :-  %child-sync
      %+  compose-tests
        %+  compose-tests
          (star ~marbud)
        (touch-file ~bud)
      (check-file-touched ~marbud)
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
    =/  res=[events=(list ph-event) new-state=test-core]
      (start:(~(got by raw-test-cores) lab.arg) now.hid)
    =.  test-cores  (~(put by test-cores) lab.arg [ships .]:new-state.res)
    =^  moves-1  this  (subscribe-to-effects lab.arg ships.new-state.res)
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
  =/  test-cor  (~(get by test-cores) lab)
  ?~  test-cor
    ~&  [%ph-dropping lab]
    `this
  =^  events  u.test-cor
    |-  ^-  (quip ph-event _u.test-cor)
    ?~  ovo.ova
      [~ u.test-cor]
    =^  events-1  cor.u.test-cor
      (route:cor.u.test-cor now.hid who.ova i.ovo.ova)
    =^  events-2  u.test-cor
      $(ovo.ova t.ovo.ova)
    [(weld events-1 events-2) u.test-cor]
  =.  test-cores  (~(put by test-cores) lab u.test-cor)
  (run-events lab events)
--
