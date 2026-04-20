/+  test
/=  gall-raw   /sys/vane/gall
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
  ++  fail    |=(arg=tang `form`|=(=state [%| arg]))
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
++  mock
  |%
  ++  easy  (make ~)
  +$  arm   ?(%on-init %on-load %on-poke %on-watch %on-leave %on-agent %on-arvo %on-fail)
  +$  fec   $-(vase (unit card:agent:gall))
  ++  echo  |=(vase `[%pass //echo %arvo %syscall `note-arvo`[%b %drip +<]])
  ::
  ++  make  ::  from stock
    |=  a=(list (pair arm fec))
    %-  full
    :*  ::  always echo callback arms
        ::
        :-  %on-agent  echo:mock
        :-  %on-arvo   echo:mock
        :-  %on-fail   |=(* ~&(%mock-on-fail ~))  ::TODO  echo?
        ::  always handle %test-card pokes
        ::
        :-  %on-poke
        |=  a=vase
        =+  !<([=mark =vase] a)
        ?.  ?=(%test-card mark)  ~
        `!<(card:agent:gall vase)
        ::
        a
    ==
  ::
  ++  full  ::  from scratch
    |=  a=(list (pair arm fec))
    =/  j=(jar arm fec)
      %+  roll  a
      |=  [[a=arm f=fec] j=(jar arm fec)]
      (~(add ja j) a f)
    =/  c
      |=  [a=arm b=vase]
      ^-  (list card:agent:gall)
      (murn (~(get ja j) a) |=(f=fec (f b)))
    ^-  agent:gall
    |_  =bowl:gall
    +*  this  .
    ++  on-init   [(c %on-init !>(~)) this]
    ++  on-load   |=(vase [(c %on-load !>(+<)) this])
    ::
    ++  on-poke   |=([mark vase] [(c %on-poke !>(+<)) this])
    ++  on-watch  |=(path [(c %on-watch !>(+<)) this])
    ++  on-leave  |=(path [(c %on-leave !>(+<)) this])
    ::
    ++  on-agent  |=([wire sign:agent:gall] [(c %on-agent !>(+<)) this])
    ++  on-arvo   |=([wire gift-user-v1:gall] [(c %on-arvo !>(+<)) this])
    ++  on-fail   |=([term tang] [(c %on-fail !>(+<)) this])
    ::
    ++  on-save   !>(~)
    ++  on-peek   |=(path ~)
    --
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
  |=  [[=wire =duct] =sign-arvo]  ::REVIEW  default duct awkard?
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
  (pure:m ?>(?=(%20 -.e) +.e))
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
  |=  [=dude:gall rez=(list [arvo-resource:gall (unit resource-deet:gall)])]
  =/  m  (mare ,~)
  ::NOTE  if only ;<  let us specify just a face...
  ;<  e=_+:*$>(%live egg:gall)  bind:m  (get-live-egg dude)
  ;<  ~  bind:m
    %+  ex-equal  !>(resources.e)
    !>((~(gas in *(set arvo-resource:gall)) (turn rez head)))
  %+  ex-equal  !>(resource-deets.e)
  !>  %-  ~(gas by *(map arvo-resource:gall resource-deet:gall))
  (murn rez |*([k=* v=*] ?:(?=(~ v) ~ (some [k +.v]))))
::
++  ex-resource-deet
  |=  [=dude:gall res=arvo-resource:gall det=resource-deet:gall]
  =/  m  (mare ,~)
  ;<  e=_+:*$>(%live egg:gall)  bind:m  (get-live-egg dude)
  ?:  !(~(has in resources.e) res)
    (fail:m leaf+"%{(trip dude)} has no such resource {<res>}" ~)
  ?:  !(~(has by resource-deets.e) res)
    (fail:m leaf+"%{(trip dude)} has no such resource-deets {<res>}" ~)
  (ex-equal !>((~(got by resource-deets.e) res)) !>(det))
::
++  ex-boat
  |=  [=dude:gall =boat:gall]
  =/  m  (mare ,~)
  ::NOTE  if only ;<  let us specify just a face...
  ;<  e=_+:*$>(%live egg:gall)  bind:m  (get-live-egg dude)
  (ex-equal !>(boat.e) !>(boat))
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
  ?.  ?=(%pass -.move)
    (expect-eq:test !>(-.move) !>(%pass))
  ?.  ?=([%use @ @ @ %$ %$ %echo ~] p.move)
    (expect-eq:test !>(p.move) !>(`wire`/use/some-dude/some-nonce/some-ship/$/$/echo))
  ?.  ?=([%b %drip *] q.move)
    (expect-eq:test !>(q.move) !>([%b %drip *^vase]))
  (expect-eq:test p.q.move vase)
::
++  ex-on-arvo
  |=  [=wire gift=gift-user-v1:gall]
  (ex-echo !>(+<))
::
++  ex-on-agent
  |=  [=wire =sign:agent:gall]
  (ex-echo !>(+<))
::
::  setup
::
++  mock-card
  |=  =card:agent:gall
  (do-deal %mock %poke %test-card !>(card))
::
++  a2k-wire
  |=  [=dude:gall =wire deet=(unit *)]
  =/  m  (mare ,^wire)
  ;<  src=ship  bind:m  get-our  ::NOTE  !
  (use-wire dude %hug (scot %p src) ?~(deet %$ (crip ~(rend co %blob u.deet))) wire)
::
++  use-wire
  |=  [=dude:gall =wire]
  =/  m  (mare ,^wire)
  ;<  e=_+:*$>(%live egg:gall)  bind:m  (get-live-egg dude)
  (pure:m %use dude run-nonce:e wire)
::
++  a2a-wire
  |=  [=dude:gall =dock:gall =wire]
  (use-wire dude %out (scot %p p.dock) q.dock wire)
::
::  tests
::
++  test-timer-tracking
  %-  eval-mare
  ;<  *  bind:m  (do-load %mock easy:mock)
  ;<  moz=(list move:gall)  bind:m
    (mock-card %pass /agent/wire %arvo %behn %wait ~2026.2.2)
  ;<  gall-wire=wire        bind:m
    (a2k-wire %mock /agent/wire `~2026.2.2)
  ;<  ~  bind:m
    %+  ex-moves  moz
    :~  (ex-move default-duct %give %unto %poke-ack ~)
        (ex-move ~[/sysduct] %pass gall-wire [%b %wait ~2026.2.2])
    ==
  ;<  ~  bind:m
    %+  ex-resources  %mock
    :~  [/agent/wire %behn %wait ~2026.2.2]^~
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
  ;<  *  bind:m  (do-load %mock easy:mock)
  ;<  *  bind:m
    (mock-card %pass /agent/wire %arvo %behn %wait ~2026.2.2)
  ;<  moz=(list move:gall)  bind:m
    (mock-card %pass /agent/wire %arvo %behn %rest ~2026.2.2)
  ;<  gall-wire=wire        bind:m
    (a2k-wire %mock /agent/wire `~2026.2.2)
  ;<  ~  bind:m
    %+  ex-moves  moz
    :~  (ex-move default-duct %give %unto %poke-ack ~)
        (ex-move ~[/sysduct] %pass gall-wire [%b %rest ~2026.2.2])
    ==
  (ex-resources %mock ~)
::
++  test-lick-socket
  %-  eval-mare
  ;<  *  bind:m  (do-load %mock easy:mock)
  ;<  moz=(list move:gall)  bind:m
    (mock-card %pass /agent/wire %arvo %lick %spin /mysocket)
  ;<  gall-wire=wire        bind:m
    (a2k-wire %mock /agent/wire ~)
  ;<  ~  bind:m
    %+  ex-moves  moz
    :~  (ex-move default-duct %give %unto %poke-ack ~)
        (ex-move ~[/sysduct] %pass gall-wire [%l %spin [%mock /mysocket]])
    ==
  %+  ex-resources  %mock
  :~  [/agent/wire %lick %spin /mysocket]^~
  ==
::
++  test-keen-request
  %-  eval-mare
  ;<  *  bind:m  (do-load %mock easy:mock)
  ::  agent issues a plain %keen
  ::
  =/  =spar:ames  [~fun /g/x/~2222.2.2/dude/some/thing]
  ;<  gall-wire=wire  bind:m
    (a2k-wire %mock /agent/wire ~)  ::TODO  different from +test-lick-socket
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
  ;<  *  bind:m  (do-load %mock easy:mock)
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %behn %wait ~2345.6.7)
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %iris %request *request:http *outbound-config:iris)
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %lick %spin /mysocket)
  ::TODO  clay request
  ::  nuking the agent should delete/clear/close/cancel all its resources.
  ::  make sure we grab the wire while the nonce is still known!
  ::
  ;<  gall-wire=wire        bind:m
    (a2k-wire %mock /agent/wire ~)
  ;<  gall-wire-b=wire        bind:m
    (a2k-wire %mock /agent/wire `~2345.6.7)
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
++  test-suspend-and-revive
  %-  eval-mare
  ;<  *  bind:m  (do-load %mock easy:mock)
  =/  =rave:clay  [%sing %x ud+1 /some/txt]
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %behn %wait ~2345.6.7)
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %clay %read 123 ~zod %desk rave)
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %eyre %connect [~ /x] %dude)
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %iris %request *request:http *outbound-config:iris)
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %lick %spin /mysocket)
  ;<  *  bind:m  (mock-card %pass /agent/wire %agent [~fun %bar] %watch /blah)
  ::  suspending the agent should "pause" all its resources.
  ::  we delete the resources, but remember them for revival.
  ::
  ;<  gall-wire=wire        bind:m
    (a2k-wire %mock /agent/wire ~)
  ;<  gall-wire-e=wire        bind:m
    (a2k-wire %mock /agent/wire `%dude)
  ;<  gall-wire-c=wire        bind:m
    (a2k-wire %mock /agent/wire `123)
  ;<  gall-wire-b=wire        bind:m
    (a2k-wire %mock /agent/wire `~2345.6.7)
  ;<  gall-wire-a=wire      bind:m
    (a2a-wire %mock [~fun %bar] /agent/wire)
  ;<  moz=(list move:gall)  bind:m
    (do-call ~ %idle %mock)
  ;<  ~  bind:m
    ::NOTE  moves sorted because otherwise dependent on set order
    %+  ex-moves  (sort moz aor)
    :~  (ex-move ~[/sysduct] %pass gall-wire [%i %cancel-request ~])
        (ex-move ~[/sysduct] %pass gall-wire [%l %shut [%mock /mysocket]])
        (ex-move ~[/sysduct] %pass gall-wire-e [%e %disconnect ~ /x])
        (ex-move ~[/sysduct] %pass gall-wire-c [%c %warp ~zod %desk ~])
        (ex-move ~[/sysduct] %pass gall-wire-b [%b %rest ~2345.6.7])
        (ex-move ~[/sysduct] %pass gall-wire-a [%g %deal [~dev ~fun /gall/mock] %bar %leave ~])  ::TODO  deal constructor
    ==
  ::TODO  test that resources still tracked
  ;<  y=yoke:gall  bind:m  (get-yoke %mock)
  ?.  &(?=(%live -.y) ?=(%| -.agent.y))
    (fail:m 'agent not suspended' ~)
  ::  reviving the agent should reiflate its resources
  ::
  ;<  moz=(list move:gall)  bind:m
    (do-load %mock easy:mock)
  ;<  ~  bind:m
    ::NOTE  moves sorted because otherwise dependent on set order
    %+  ex-moves  (sort moz aor)
    :~  (ex-move default-duct %pass /sys/say [%d [%text "gall: bumped %mock"]])
        (ex-move ~[/sysduct] %pass gall-wire [%l %spin [%mock /mysocket]])
        (ex-move ~[/sysduct] %pass gall-wire-e [%e %connect [~ /x] %dude])
        (ex-move ~[/sysduct] %pass gall-wire-c [%c %warp ~zod %desk ~ rave])
        (ex-move ~[/sysduct] %pass gall-wire-b [%b %wait ~2345.6.7])
        (ex-on-agent /agent/wire %kick ~)
        (ex-on-arvo /agent/wire [%iris %http-response %cancel ~])
        (ex-on-arvo /agent/wire [%lick %soak /mysocket %disconnect ~])
    ==
  (pure:m ~)
::
++  test-redundant-arvo-deflate-onload
  ::  agent unsets timer during +on-load (during reinstall):
  ::  gall must not emit the %rest, it already emitted that during suspend
  ::
  %-  eval-mare
  ;<  *  bind:m  (do-load %mock easy:mock)
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %behn %wait ~2345.6.7)
  ;<  *  bind:m  (do-call ~ %idle %mock)
  ::  reinstall agent that clears its timer on-load
  ::
  ;<  moz=(list move:gall)  bind:m
    %+  do-load  %mock
    %-  make:mock
    [%on-load |=(* `[%pass /agent/wire %arvo %behn %rest ~2345.6.7])]~
  ::  expect timer to not be reinflated,
  ::  and not to see a redundant %rest,
  ::  and the resource to have been untracked.
  ::
  ;<  ~  bind:m
    %+  ex-moves  moz
    :~  (ex-move default-duct %pass /sys/say [%d [%text "gall: bumped %mock"]])
    ==
  (ex-resources %mock ~)
::
++  test-redundant-arvo-deflate-onagent
  ::  agent unsets timer during +on-agent (during reinstall):
  ::  gall must not emit the %rest, it already emitted that during suspend
  ::
  %-  eval-mare
  ;<  *  bind:m  (do-load %mock easy:mock)
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %behn %wait ~2345.6.7)
  ;<  *  bind:m  (mock-card %pass /agent/wire %agent [~fun %bar] %watch /blah)
  ;<  *  bind:m  (do-call ~ %idle %mock)
  ::  reinstall agent that clears its timer on-load
  ::
  ;<  moz=(list move:gall)  bind:m
    %+  do-load  %mock
    %-  make:mock
    [%on-agent |=(* `[%pass /agent/wire %arvo %behn %rest ~2345.6.7])]~
  ::  expect timer to not be reinflated,
  ::  and not to see a redundant %rest,
  ::  and the resource to have been untracked.
  ::
  ;<  ~  bind:m
    %+  ex-moves  moz
    :~  (ex-move default-duct %pass /sys/say [%d [%text "gall: bumped %mock"]])
        (ex-on-agent /agent/wire %kick ~)
    ==
  (ex-resources %mock ~)
::
++  test-redundant-watch-deflate-onload
  ::  agent leaves a subscription during +on-load (during reinstall):
  ::  gall must not emit %leave, it already emitted that during suspend
  ::
  %-  eval-mare
  ;<  *  bind:m  (do-load %mock easy:mock)
  ;<  *  bind:m  (mock-card %pass /agent/wire %agent [~fun %bar] %watch /blah)
  ;<  *  bind:m  (do-call ~ %idle %mock)
  ::  reinstall agent that clears its timer on-load
  ::
  ;<  moz=(list move:gall)  bind:m
    %+  do-load  %mock
    %-  make:mock
    [%on-load |=(* `[%pass /agent/wire %agent [~fun %bar] %leave ~])]~
  ::  expect +on-agent not to be called,
  ::  and not to see a redundant %leave,
  ::  and the resource to have been untracked.
  ::
  ;<  ~  bind:m
    %+  ex-moves  moz
    :~  (ex-move default-duct %pass /sys/say [%d [%text "gall: bumped %mock"]])
    ==
  (ex-boat %mock ~)
::
++  test-redundant-watch-deflate-onagent
  ::  agent leaves a subscription during +on-agent (during reinstall):
  ::  gall must not inflate that sub nor emit the %leave,
  ::  it already emitted that during suspend
  ::
  %-  eval-mare
  ;<  *  bind:m  (do-load %mock easy:mock)
  ;<  *  bind:m  (mock-card %pass /agent/wire %agent [~fun %bar] %watch /blah)
  ;<  *  bind:m  (mock-card %pass /agent/wire2 %agent [~fun %baz] %watch /blah)
  ;<  *  bind:m  (do-call ~ %idle %mock)
  ::  reinstall agent that leaves the sub on-agent
  ::
  ;<  moz=(list move:gall)  bind:m
    %+  do-load  %mock
    %-  make:mock
    [%on-agent |=(* `[%pass /agent/wire2 %agent [~fun %baz] %leave ~])]~
  ::  expect +on-agent to only be called for the first-kicked sub,
  ::  and not to see a redundant %leave,
  ::  and the resource to have been untracked.
  ::
  ;<  ~  bind:m
    %+  ex-moves  moz
    :~  (ex-move default-duct %pass /sys/say [%d [%text "gall: bumped %mock"]])
        (ex-on-agent /agent/wire %kick ~)  ::NOTE  assumes order!
    ==
  (ex-boat %mock ~)
::
++  test-reload
  %-  eval-mare
  ;<  *  bind:m  (do-load %mock easy:mock)
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %behn %wait ~2345.6.7)
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %iris %request *request:http *outbound-config:iris)
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %lick %spin /mysocket)
  ;<  *  bind:m  (mock-card %pass /agent/wire %agent [~fun %bar] %watch /blah)
  ::  simply reloading the agent with a new core
  ::  should leave its resources untouched
  ::
  ;<  moz=(list move:gall)  bind:m
    %+  do-load  %mock
    ::  we care about this being a different core than +easy:mock,
    ::  so that we trigger the "gall: bumped"
    ::
    %-  make:mock
    [%on-poke |=(* ~&(%hi ~))]~
  %+  ex-moves  moz
  :~  (ex-move default-duct %pass /sys/say [%d [%text "gall: bumped %mock"]])
  ==
::
++  test-lick-close-on-disconnect
  %-  eval-mare
  ;<  *  bind:m  (do-load %mock easy:mock)
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %lick %spin /mysocket)
  ::  inflating a lick socket simulates a %disconnect,
  ::  but the agent may close the socket in response to that.
  ::  gall should not re-open the socket in that case.
  ::
  ;<  gall-wire=wire        bind:m
    (a2k-wire %mock /agent/wire ~)
  ;<  moz=(list move:gall)  bind:m
    (do-call ~ %idle %mock)
  ;<  ~  bind:m
    %+  ex-moves  moz
    :~  (ex-move ~[/sysduct] %pass gall-wire [%l %shut [%mock /mysocket]])
    ==
  ;<  moz=(list move:gall)  bind:m
    %+  do-load  %mock
    %-  make:mock
    [%on-arvo |=(* `[%pass /agent/wire %arvo [%lick %shut /mysocket]])]~
  %+  ex-moves  moz
  :~  (ex-move default-duct %pass /sys/say [%d [%text "gall: bumped %mock"]])
      (ex-on-arvo /agent/wire [%lick %soak /mysocket %disconnect ~])
  ==
::
++  test-clay-read-single
  %-  eval-mare
  ;<  *  bind:m  (do-load %mock easy:mock)
  ;<  gall-wire=wire  bind:m  (a2k-wire %mock /agent/wire `123)
  =/  =rave:clay  [%sing %x ud+1 /some/txt]
  ::
  ;<  moz=(list move:gall)  bind:m
    (mock-card %pass /agent/wire %arvo %clay %read 123 ~zod %desk rave)
  ;<  ~  bind:m
    %+  ex-moves  moz
    :~  (ex-move default-duct %give %unto %poke-ack ~)
        (ex-move ~[/sysduct] %pass gall-wire [%c %warp ~zod %desk `rave])
    ==
  ;<  ~  bind:m
    %+  ex-resources  %mock
    :~  [/agent/wire %clay %warp 123]^`[%clay %warp ~zod %desk rave]
    ==
  ::
  %-  branch
  :~  :-  'cancelled by agent'
      ;<  moz=(list move:gall)  bind:m
        (mock-card %pass /agent/wire %arvo %clay %drop 123)
      ;<  ~  bind:m
        %+  ex-moves  moz
        :~  (ex-move default-duct %give %unto %poke-ack ~)
            (ex-move ~[/sysduct] %pass gall-wire [%c %warp ~zod %desk ~])
        ==
      (ex-resources %mock ~)
    ::
      :-  'response from clay'
      =/  =riot:clay  `[[%x ud+1 %desk] /some/txt *cage]
      ;<  moz=(list move:gall)  bind:m
        ::NOTE  %sing and %mult always give a single %writ response
        (do-take [gall-wire default-duct] %clay %writ riot)
      ;<  ~  bind:m
        %+  ex-moves  moz
        :~  (ex-on-arvo /agent/wire %clay %read 123 riot)
        ==
      (ex-resources %mock ~)
  ==
::
++  test-clay-read-many
  ::  %many subscriptions make clay notify on every change in a case range.
  ::  gall should track the sub and progressively shrink its range
  ::  until clay sends an explicit close signal.
  ::
  %-  eval-mare
  ;<  *  bind:m  (do-load %mock easy:mock)
  ;<  gall-wire=wire  bind:m  (a2k-wire %mock /agent/wire `123)
  =/  =rave:clay  [%many | ud+1 ud+3 /some/txt]
  ::
  ;<  moz=(list move:gall)  bind:m
    (mock-card %pass /agent/wire %arvo %clay %read 123 ~zod %desk rave)
  ;<  ~  bind:m
    %+  ex-moves  moz
    :~  (ex-move default-duct %give %unto %poke-ack ~)
        (ex-move ~[/sysduct] %pass gall-wire [%c %warp ~zod %desk `rave])
    ==
  ;<  ~  bind:m
    %+  ex-resources  %mock
    :~  [/agent/wire %clay %warp 123]^`[%clay %warp ~zod %desk rave]
    ==
  ::  clay gives an initial response that doesn't fully fill the request
  ::
  =/  =riot:clay  `[[%w ud+1 %desk] /some/txt %null !>(~)]
  ;<  moz=(list move:gall)  bind:m
    (do-take [gall-wire default-duct] %clay %writ riot)
  ;<  ~  bind:m
    %+  ex-moves  moz
    :~  (ex-on-arvo /agent/wire %clay %read 123 riot)
    ==
  =.  rave  [%many | ud+2 ud+3 /some/txt]
  ;<  ~  bind:m
    %+  ex-resources  %mock
    :~  [/agent/wire %clay %warp 123]^`[%clay %warp ~zod %desk rave]
    ==
  ::  clay indicates the request will not get further responses
  ::
  ;<  moz=(list move:gall)  bind:m
    (do-take [gall-wire default-duct] %clay %writ ~)
  ;<  ~  bind:m
    %+  ex-moves  moz
    :~  (ex-on-arvo /agent/wire %clay %read 123 ~)
    ==
  (ex-resources %mock ~)
::
++  test-clay-read-many-partial-inflate
  ::  when %many has received _some_ responses but isn't done yet,
  ::  should pick up where we left off
  ::
  %-  eval-mare
  ;<  *  bind:m  (do-load %mock easy:mock)
  ;<  gall-wire=wire  bind:m
    (a2k-wire %mock /agent/wire `123)
  =/  =rave:clay  [%many | ud+1 ud+3 /some/txt]
  ;<  *  bind:m
    (mock-card %pass /agent/wire %arvo %clay %read 123 ~zod %desk rave)
  =/  =riot:clay  `[[%w ud+1 %desk] /some/txt %null !>(~)]
  ;<  *  bind:m
    (do-take [gall-wire default-duct] %clay %writ riot)
  =.  rave  [%many | ud+2 ud+3 /some/txt]
  ::
  ;<  *                     bind:m  (do-call ~ %idle %mock)
  ;<  moz=(list move:gall)  bind:m  (do-load %mock easy:mock)
  ::
  %+  ex-moves  moz
  :~  (ex-move default-duct %pass /sys/say [%d [%text "gall: bumped %mock"]])
      (ex-move ~[/sysduct] %pass gall-wire [%c %warp ~zod %desk ~ rave])
  ==
::
::TODO  same style for simple +test-simply-tracked-tasks
++  test-misc-untracked-tasks
  %-  zing
  %+  turn
    ^-  (list [task-user-v1:gall note-arvo])
    :~  :-  [%ames %snub %deny ~fed ~]  [%a %snub %deny ~fed ~]
        :-  [%ames %prod ~]             [%a %prod ~]
        :-  [%ames %sift ~]             [%a %sift ~]
        :-  [%ames %spew ~]             [%a %spew ~]
        :-  [%ames %cong 1 2]           [%a %cong 1 2]
        :-  [%ames %stir 'a']           [%a %stir 'a']
        :-  [%ames %trim 1]             [%a %trim 1]
      ::
        :-  [%behn %trim 1]             [%b %trim 1]
        :-  [%clay %trim 1]             [%c %trim 1]
        :-  [%dill %trim 1]             [%d %trim 1]
        :-  [%eyre %trim 1]             [%e %trim 1]
        :-  [%gall %trim 1]             [%g %trim 1]
        :-  [%iris %trim 1]             [%i %trim 1]
        :-  [%jael %trim 1]             [%j %trim 1]
        :-  [%khan %trim 1]             [%k %trim 1]
        :-  [%lick %trim 1]             [%l %trim 1]
    ==
  ::
  |=  [task=task-user-v1:gall note=note-arvo]
  %-  eval-mare
  ;<  *                     bind:m
    (do-load %mock easy:mock)
  ;<  moz=(list move:gall)  bind:m
    (mock-card %pass /agent/wire %arvo task)
  ::
  ;<  gall-wire=wire        bind:m
    (a2k-wire %mock /agent/wire ~)
  ;<  ~  bind:m  (ex-resources %mock ~)
  %+  ex-moves  moz
  :~  (ex-move default-duct %give %unto %poke-ack ~)
      (ex-move ~[/sysduct] %pass gall-wire note)
  ==
--
::
::TODO  test duplicate resource creation for failure
::TODO  same during reinstall/reload
::
::TODO  test keen wire consistent between %keen, %keen w/ secret, reinstall
::TODO  test namespace revision nrs across nukes
