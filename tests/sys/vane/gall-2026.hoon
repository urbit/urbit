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
++  branch
  =/  m  (mare ,~)
  |=  l=(list [t=@t f=form:m])  ::NOTE  can't seem to use $^ here
  ^-  form:m
  =/  e=tang  ~
  |=  s=state
  |-  ^-  output:m
  ?~  l
    ?.  =(~ e)  [%| e]
    [%& ~ s]
  =/  o  (f.i.l s)
  =?  e  ?=(%| -.o)
    =-  (weld e `tang`-)
    [(rap 3 'failed in branch \'' t.i.l '\':' ~) p.o]
  $(l t.l)
::
++  merge  ::  branch with shared, cached continuation
  |*  a=mold  ::  arg for constructing continuation, comes out of branches
  =/  w  (mare a)
  =/  m  (mare ,~)
  |=  [l=(list [t=@t f=form:w]) n=$-(a form:m)]
  ^-  form:m
  =|  err=tang
  =|  per=(map tang @t)
  =|  cac=(map @ output:m)
  |=  sat=state
  |-  ^-  output:m
  ?~  l
    ?.  =(~ err)  [%| err]
    [%& ~ sat]
  =^  res=output:m  cac
    ::  the below is essentially (((bind:m a) f.i.l n) sat)
    ::  but with the n invocation cached
    ::
    =/  wes=output:w  (f.i.l sat)
    ?:  ?=(%| -.wes)  [wes cac]
    ?^  hit=(~(get by cac) (mug p.wes))
      [u.hit cac]
    =/  res=output:m  ((n out.p.wes) state.p.wes)
    [res (~(put by cac) (mug p.wes) res)]
  ::  when printing fail traces, if a previous branch had an identical failure,
  ::  just print a reference to that for brevity
  ::
  =?  err  ?=(%| -.res)
    =-  (weld err `tang`-)
    :-  (rap 3 'failed in merge branch \'' t.i.l '\':' ~)
    ?~  pev=(~(get by per) p.res)  p.res
    [(rap 3 '[same as in merge branch \'' u.pev '\']' ~)]~
  =?  per  &(?=(%| -.res) !(~(has by per) p.res))
    (~(put by per) p.res t.i.l)
  $(l t.l)
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
  ++  on-fail   |=(* ~&(%mock-on-fail [~ this]))  ::TODO  echo the call outward?
  ++  on-peek   |=(* ~)
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card:agent:gall _this)
    ?>  ?=(%test-card mark)
    [[!<(card:agent:gall vase)]~ this]
  ::
  ++  on-arvo
    |=  [=wire =gift-user-v1:gall]
    [[%pass //echo %arvo %syscall `note-arvo`[%b %drip !>(+<)]]~ this]
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
  ~|  [%get-scry care desk path]
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
++  get-yoke
  |=  =dude:gall
  |=  =state
  [%& (~(got by yokes.state.g.state) dude) state]
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
++  ex-echo
  |=  =vase
  |=  move:gall
  ~|  -.move
  ~!  -.move
  ?>  ?=(%pass -.move)
  ~|  p.move
  ?>  ?=([%use @ @ @ %$ %$ %echo ~] p.move)
  ~|  q.move
  ?>  ?=([%b %drip *] q.move)
  (expect-eq:test p.q.move vase)
::
++  ex-on-arvo
  |=  [=wire gift=gift-user-v1:gall]
  (ex-echo !>(+<))
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
::
++  test-keen-request
  %-  eval-mare
  ;<  *  bind:m  (do-load %mock mock-agent)
  ::  agent issues a plain %keen
  ::
  =/  =spar:ames  [~fun /g/x/~2222.2.2/dude/some/thing]
  ;<  gall-wire=wire  bind:m
    (use-wire %mock //agent/wire)  ::TODO  different from +test-lick-socket
  %+  (merge (list move:gall))
    :~  :-  'unencrypted keen'
        ;<  moz=(list move:gall)  bind:m
          (mock-card %pass /agent/wire %arvo %ames %keen secret=| spar)
        ;<  ~  bind:m
          %+  ex-moves  moz  ::TODO  different from +test-lick-socket
          :~  (ex-move ~[/sysduct] %pass gall-wire [%a %keen sec=~ spar])
              (ex-move default-duct %give %unto %poke-ack ~)
          ==
        (pure:m ~)  ::TODO
      ::
        :-  'encrypted keen'
        ;<  moz=(list move:gall)  bind:m
          (mock-card %pass /agent/wire %arvo %ames %keen secret=& spar)
        =/  plea-wire=wire
          [%key %mock '0w3.lBw1H' %bod (scot %p ship.spar) path.spar]  ::TODO  construct from helper
        ;<  ~  bind:m
          %+  ex-moves  moz  ::TODO  different from +test-lick-socket
          :~  (ex-move default-duct %pass plea-wire [%a %plea ship.spar [%g /gk/dude %0 /some/thing]])
              (ex-move default-duct %give %unto %poke-ack ~)
          ==
        =/  =brood:gall  [path.spar 1 2 3]
        ;<  moz=(list move:gall)  bind:m
          (do-take [plea-wire ~[/sysduct]] %ames %boon %0 `brood)
        ;<  ~  bind:m
          %+  ex-moves  moz
          :~  (ex-move ~[/sysduct] %pass gall-wire [%a %keen sec=`+.hutch.brood spar])
          ==
        (pure:m ~)
    ==
  |=  moz=(list move:gall)
  ;<  e=_+:*$>(%live egg:gall)  bind:m  (get-live-egg %mock)
  ;<  ~  bind:m  (ex-equal !>(ken.e) !>((~(put ju *(jug spar:ames wire)) spar /agent/wire)))
  ::TODO  emit a second request?
  ::  response comes back from ames
  ::
  %+  (merge ,~)
    :~  :-  'page result'
        ;<  moz=(list move:gall)  bind:m
          (do-take [gall-wire ~[/sysduct]] %ames %sage `sage:mess:ames`[spar *page])
        (ex-moves moz (ex-on-arvo /agent/wire %ames %sage spar *page) ~)
      ::
        :-  'empty result'
        ;<  moz=(list move:gall)  bind:m
          (do-take [gall-wire ~[/sysduct]] %ames %sage `sage:mess:ames`[spar ~])
        (ex-moves moz (ex-on-arvo /agent/wire %ames %sage spar ~) ~)
    ==
  |=  ~
  ;<  e=_+:*$>(%live egg:gall)  bind:m  (get-live-egg %mock)
  ;<  ~  bind:m  (ex-equal !>(ken.e) !>(*(jug spar:ames wire)))
  (pure:m ~)
::
++  test-nuke-closes-resources
  %-  eval-mare
  ;<  *  bind:m  (do-load %mock mock-agent)
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %behn %wait ~2345.6.7)
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %iris %request *request:http *outbound-config:iris)
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %lick %spin /mysocket)
  ::  nuking the agent should delete/clear/close/cancel all its resources.
  ::  make sure we grab the wire while the nonce is still known!
  ::
  ;<  gall-wire=wire        bind:m
    (use-wire %mock //agent/wire)
  ;<  gall-wire-b=wire        bind:m
    (use-wire %mock /[(crip ~(rend co %blob ~2345.6.7))]/agent/wire)
  ;<  moz=(list move:gall)  bind:m
    (do-call ~ %nuke %mock)
  ;<  ~  bind:m
    ::NOTE  moves sorted because otherwise dependent on set order
    %+  ex-moves  (sort moz aor)
    :~  (ex-move ~[/sysduct] %pass gall-wire [%i %cancel-request ~])
        (ex-move ~[/sysduct] %pass gall-wire [%l %shut [%mock /mysocket]])
        (ex-move ~[/sysduct] %pass gall-wire-b [%b %rest ~2345.6.7])
    ==
  ;<  y=yoke:gall  bind:m  (get-yoke %mock)
  (ex-equal !>(-.y) !>(%nuke))
::
::TODO  test keen wire consistent between %keen, %keen w/ secret, reinstall
::TODO  test namespace revision nrs across nukes
--
