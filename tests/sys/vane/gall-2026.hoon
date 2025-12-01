/+  *test
/=  gall-raw   /gall  ::  not the real gall!
=/  gall       (gall-raw ~dev)  ::  intentionally shadow for new types
::
|%
++  mock-agent
  ^-  agent:gall
  |_  =bowl:gall
  +*  this  .
  ++  on-init   [~ this]
  ++  on-save   !>(~)
  ++  on-load   |=(* [~ this])
  ++  on-watch  |=(* [~ this])
  ++  on-leave  |=(* [~ this])
  ++  on-agent  |=(* [~ this])
  ++  on-arvo   |=(* [~ this])  ::TODO  echo the call outward?
  ++  on-fail   |=(* [~ this])  ::TODO  echo the call outward?
  ++  on-peek   |=(* ~)
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card:agent:gall _this)
    ?>  ?=(%test-card mark)
    [[!<(card:agent:gall vase)]~ this]
  --
::
++  mock-roof
  ^-  roof
  |=  [=gang prov=path =view [=ship =desk =case] =path]
  *(unit (unit cage))
::
++  make-gall
  =/  core  (gall ~2026.1.1 *@uvJ mock-roof)
  =+  [out g]=(call:core ~[/sysduct] ~ %init ~)
  g
::
++  ga
  =/  =duct     [/test]~
  =/  now=@da   ~2026.1.1
  =/  eny=@uvJ  *@uvJ
  |_  g=_make-gall
  +|  %reads
  ++  inner
    (g now eny mock-roof)
  ::
  ++  scry
    |*  [=mold care=term =desk =path]
    !<(mold q:(need (need (scry:inner `~ / care [our.g desk da+now.g] path))))
  ::
  ++  egg
    |=  =dude:gall
    ^-  egg:gall
    =+  e=(scry egg-any:gall %v dude /$)
    ?>(?=(%16 -.e) +.e)
  ::
  ++  live-egg
    |=  =dude:gall
    ^-  _+:*$>(%live egg:gall)
    =+  e=(egg dude)
    ?>(?=(%live -.e) +.e)
  ::
  +|  %writes
  ::
  ++  call
    |=  =task:gall
    (call:inner duct ~ task)
  ::
  ++  load
    |=  [=dude:gall =agent:gall]
    ^-  (quip move:gall _g)
    (call %load [dude [our.g %desk da+now.g] agent]~)
  ::
  ++  deal
    |=  [=dude:gall =deal:gall]
    ^-  (quip move:gall _g)
    (call %deal [our.g our.g /] dude deal)
  ::
  ++  take
    |=  [=wire sign=sign-arvo]
    ^-  (quip move:gall _g)
    (take:inner wire duct ~ sign)
  --
::
::  setup
::
++  setup-agent
  =/  g  make-gall
  =^  moz=(list move:gall)  g
    (~(load ga g) %mock mock-agent)
  ?>  =(& (~(scry ga g) ? %u %mock /$))
  g
::
++  make-poke
  |=  =card:agent:gall
  ^-  task:agent:gall
  [%poke %test-card !>(card)]
::
++  set-timer
  |=  g=_setup-agent
  ^-  [tang _g]
  =/  old-e  (~(live-egg ga g) %mock)
  =^  moz=(list move:gall)  g
    =/  =card:agent:gall
      [%pass /agent/wire %arvo %behn %wait ~2026.2.2]
    (~(deal ga g) %mock (make-poke card))
  :_  g
  ::
  =/  ex-resource=arvo-resource:gall
    [/agent/wire %behn %wait ~2026.2.2]
  =/  e  (~(live-egg ga g) %mock)
  =/  gall-wire=wire
    /use/mock/[run-nonce.e]/~dev/[(crip ~(rend co %blob ~2026.2.2))]/agent/wire
  ;:  weld
    %+  expect-eq
      !>  ^-  (list move:gall)
      :~  [duct:ga %give %unto %poke-ack ~]
          [~[/sysduct] %pass gall-wire `note-arvo`[%b %wait ~2026.2.2]]
      ==
    !>(moz)
  ::
    %+  expect-eq
      !>((~(gas by *(set arvo-resource:gall)) ex-resource ~))  ::TODO  bad
    !>(resources.e)
  ==
::
::  tests
::
++  test-timer-tracking
  =/  g  setup-agent
  =^  fail  g  (set-timer g)
  ?^  fail  fail
  ::
  =/  gall-wire=wire  ::TODO  from +set-timer
    /use/mock/[run-nonce:(~(live-egg ga g) %mock)]/~dev/[(crip ~(rend co %blob ~2026.2.2))]/agent/wire
  =^  moz=(list move:gall)  g
    (~(take ga g) gall-wire %behn %wake ~)
  =/  e  (~(live-egg ga g) %mock)
  ;:  weld
    (expect-eq !>(~) !>(moz))
    (expect-eq !>(~) !>(resources.e))
  ==
--
