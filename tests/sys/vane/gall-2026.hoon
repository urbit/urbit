/+  test
/=  gall-raw   /gall  ::  not the real gall!
=/  gall       (gall-raw ~dev)  ::  intentionally shadow for new types
::
|%
+$  state         [g=_make-gall now=@da eny=@uvJ =roof]
++  default-duct  ~[//test]
::
++  form-raw    |$  [a]  $-(state (output-raw a))
++  output-raw  |$  [a]  (each [out=a =state] tang)
++  mare
  |*  a=mold
  |%
  ++  form    (form-raw a)
  ++  output  (output-raw a)
  ++  pure    |=(arg=a `form`|=(=state [%& arg state]))
  ++  bind
    |*  b=mold
    |=  [m-b=(form-raw b) fun=$-(b form)]
    |=  =state
    =+  r=(m-b state)
    ?-  -.r
      %&  ((fun out.p.r) state.p.r)
      %|  [%| p.r]
    ==
  --
++  m  (mare ,~)
++  eval-mare
  |=  f=form:m
  ^-  tang
  =/  res  (f make-gall ~2026.1.1 *@uvJ mock-roof)
  ?:  ?=(%& -.res)  ~
  ?^  p.res  p.res
  ~['+eval-mare failure with empty trace']
::
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
::
++  do
  |=  f=$-(_*g:*state (quip move:gall _g:*state))
  |=  state
  =*  state  +<
  =/  m  (mare (list move:gall))
  ^-  output:m
  =^  moz=(list move:gall)  g  (f (g now eny roof))
  [%& moz state]
::
++  do-call
  |=  [duct=?(~ duct) =task:gall]
  =?  duct  =(~ duct)  default-duct
  %-  do
  |=  g=_(g:*state)
  (call:g duct ~ task)
  :: |=  state
  :: =*  state  +<
  :: =/  m  (mare (list move:gall))
  :: ^-  output:m
  :: =^  moz=(list move:gall)  g
  ::   (call:(g now eny roof) duct ~ task)
  :: [%& moz state]
::
++  do-load
  |=  [=dude:gall =agent:gall]
  %-  do
  |=  g=_(g:*state)
  (call:g default-duct ~ %load [dude [our.g %desk da+now.g] agent]~)
::
++  do-deal
  |=  [=dude:gall =deal:gall]
  %-  do
  |=  g=_(g:*state)
  (call:g default-duct ~ %deal [our.g our.g /] dude deal)
::
++  do-take
  |=  [[=wire =duct] =sign-arvo:gall]  ::REVIEW  default duct awkard?
  %-  do
  |=  g=_(g:*state)
  (take:g wire duct ~ sign-arvo)
::
++  get-scry
  |*  [=mold care=term =desk =path]
  =/  m  (mare mold)
  ^-  form:m
  |=  =state
  =-  [%& - state]  ::  %-  pure:m
  !<  mold
  =<  q  %-  need  %-  need
  %-  scry:(g.state +.state)
  [`~ / care [our.g.state desk da+now.g.state] path]
::
++  get-our
  |=  =state
  [%& our.g.state state]
::
++  get-egg
  |=  =dude:gall
  =/  m  (mare egg:gall)
  ^-  form:m
  ;<  e=egg-any:gall  bind:m  (get-scry egg-any:gall %v dude /$)
  (pure:m ?>(?=(%16 -.e) +.e))
::
++  get-live-egg
  |=  =dude:gall
  =/  m  (mare _+:*$>(%live egg:gall))
  ^-  form:m
  ;<  e=egg:gall  bind:m  (get-egg dude)
  (pure:m ?>(?=(%live -.e) +.e))
::
::  expectations
::
++  ex-equal
  |=  [actual=vase expected=vase]  ::NOTE  reverse order from /lib/test
  =/  m  (mare ,~)
  ^-  form:m
  |=  s=state
  =/  =tang  (expect-eq:test expected actual)
  ?~(tang &+[~ s] |+tang)
::
++  ex-resources
  |=  [=dude:gall rez=(list arvo-resource:gall)]
  =/  m  (mare ,~)
  ::NOTE  if only ;<  let us specify just a face...
  ;<  e=_+:*$>(%live egg:gall)  bind:m  (get-live-egg dude)
  (ex-equal !>(resources.e) !>((~(gas in *(set arvo-resource:gall)) rez)))
::
++  ex-moves
  |=  [moz=(list move:gall) exes=(list $-(move:gall tang))]
  =/  m  (mare ,~)
  ^-  form:m
  |=  s=state
  =;  =tang
    ?~(tang &+[~ s] |+tang)
  |-  ^-  tang
  ?~  exes
    ?~  moz
      ~
    ['got more moves than expected' >moz< ~]
  ?~  moz
    ['expected more moves than got' ~]
  %+  weld
    (i.exes i.moz)
  $(exes t.exes, moz t.moz)
::
++  ex-move
  |=  mow=move:gall
  |=  mov=move:gall
  (expect-eq:test !>(mow) !>(mov))
::
::  setup
::
++  mock-card
  |=  =card:agent:gall
  (do-deal %mock %poke %test-card !>(card))
::
++  use-wire
  |=  [=dude:gall =wire]
  =/  m  (mare ,^wire)
  ;<  o=ship  bind:m  get-our
  ;<  e=_+:*$>(%live egg:gall)  bind:m  (get-live-egg dude)
  (pure:m %use [dude] [run-nonce:e] (scot %p o) wire)
::
::  tests
::
++  test-timer-tracking
  %-  eval-mare
  ;<  *  bind:m  (do-load %mock mock-agent)
  ;<  moz=(list move:gall)  bind:m
    (mock-card %pass /agent/wire %arvo %behn %wait ~2026.2.2)
  ;<  gall-wire=wire        bind:m
    (use-wire %mock /[(crip ~(rend co %blob ~2026.2.2))]/agent/wire)
  ;<  ~  bind:m
    %+  ex-moves  moz
    :~  (ex-move default-duct %give %unto %poke-ack ~)
        (ex-move ~[/sysduct] %pass gall-wire [%b %wait ~2026.2.2])
    ==
  ;<  ~  bind:m
    %+  ex-resources  %mock
    :~  [/agent/wire %behn %wait ~2026.2.2]
    ==
  ::
  ;<  moz=(list move:gall)  bind:m
    (do-take [gall-wire default-duct] %behn %wake ~)
  ::TODO  check that +on-arvo got called?
  ;<  e=_+:*$>(%live egg:gall)  bind:m  (get-live-egg %mock)
  (ex-resources %mock ~)
::
++  test-timer-cancellation
  %-  eval-mare
  ;<  *  bind:m  (do-load %mock mock-agent)
  ;<  *  bind:m
    (mock-card %pass /agent/wire %arvo %behn %wait ~2026.2.2)
  ;<  moz=(list move:gall)  bind:m
    (mock-card %pass /agent/wire %arvo %behn %rest ~2026.2.2)
  ;<  gall-wire=wire        bind:m
    (use-wire %mock /[(crip ~(rend co %blob ~2026.2.2))]/agent/wire)
  ;<  ~  bind:m
    %+  ex-moves  moz
    :~  (ex-move default-duct %give %unto %poke-ack ~)
        (ex-move ~[/sysduct] %pass gall-wire [%b %rest ~2026.2.2])
    ==
  (ex-resources %mock ~)
::
++  test-lick-socket
  %-  eval-mare
  ;<  *  bind:m  (do-load %mock mock-agent)
  ;<  moz=(list move:gall)  bind:m
    (mock-card %pass /agent/wire %arvo %lick %spin /mysocket)
  ;<  gall-wire=wire        bind:m
    (use-wire %mock //agent/wire)
  ;<  ~  bind:m
    %+  ex-moves  moz
    :~  (ex-move default-duct %give %unto %poke-ack ~)
        (ex-move ~[/sysduct] %pass gall-wire [%l %spin [%mock /mysocket]])
    ==
  %+  ex-resources  %mock
  :~  [/agent/wire %lick %spin /mysocket]
  ==
--
