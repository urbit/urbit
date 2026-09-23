::  gall nonce subscription tests
::
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
        :-  %on-load   echo:mock
        :-  %on-agent
        |=  a=vase
        (echo:mock a)
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
    ++  on-fail   |=([[term tang] call:agent:gall] [(c %on-fail !>(+<)) this])
    ::
    ++  on-save   !>(~)
    ++  on-peek   |=(path ~)
    --
  ::
  ++  give-fact
    ^-  (list (pair arm fec))
    :~  :-  %on-poke
        |=(* `[%give [%fact [/agent/path ~] %noun !>(~)]])
    ==
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
  |=  agents=(list [dude:gall agent:gall])
  =/  m  (mare (list move:gall))
  ^-  form:m
  |=  s=state
  =.  s  s(eny +(eny.s))
  %-  %-  do
            |=  g=_(g:*state)
            =/  =load:gall
              %+  turn  agents
              |=  [=dude:gall =agent:gall]
              [dude [our.g %desk da+now.g] [[[%super ~] ~ ~] ~] agent]
            (call:g default-duct ~ %load load)
        s
::
++  do-nuke
  |=  =dude:gall
  %-  do
  |=  g=_(g:*state)
  ::      originating from our agent.
  (call:g default-duct ~ %nuke dude)
::
++  do-deal
  |=  [=dude:gall =deal:gall]
  %-  do
  |=  g=_(g:*state)
  (call:g default-duct ~ %deal [our.g our.g /] dude deal)
::
++  do-take
  |=  [[=wire =duct] =sign-arvo]  ::REVIEW  default duct awkward?
  %-  do
  |=  g=_(g:*state)
  (take:g wire duct ~ sign-arvo)
::
++  do-watch
  |=  [wir=wire =path watch-task=task:agent:gall]
  =*  deal-watch  (gall-deal %agent-a watch-task)
  ;<  moz-watch=(list move:gall)  bind:m
    (mock-card %pass path %agent [~dev %agent-a] watch-task)
  ;<  ~  bind:m
    %+  ex-moves  moz-watch
    :~  (ex-move default-duct %give %unto %poke-ack ~)
        (ex-move ~[/sysduct] %pass wir [%g deal-watch])
    ==
  ::
  ;<  moz-deal-watch=(list move:gall)  bind:m
    (do-call ~[/sysduct] deal-watch)
  %+  ex-moves  moz-deal-watch
  :~  (ex-move ~[/sysduct] %give %unto %watch-ack ~)
  ==
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
  (pure:m ?>(?=(%21 -.e) +.e))
::
++  get-live-egg
  |=  =dude:gall
  =/  m  (mare _+:*$>(%live egg:gall))
  ^-  form:m
  ;<  e=egg:gall  bind:m  (get-egg dude)
  (pure:m ?>(?=(%live -.e) +.e))
::
++  get-nonce
  |=  =dude:gall
  =/  m  (mare ,[@t @ud])
  ^-  form:m
  ;<  yok=yoke:gall  bind:m  (get-yoke dude)
  (pure:m ?>(?=(%live -.yok) [run-nonce.yok sub-nonce.yok]))
::
::  expectations
::
++  ex
  |=  expected=vase
  =/  m  (mare ,~)
  ^-  form:m
  |=  s=state
  =/  =tang  (expect:test expected)
  ?~(tang &+[~ s] |+tang)
++  ex-equal
  |=  [actual=vase expected=vase]  ::NOTE  reverse order from /lib/test
  =/  m  (mare ,~)
  ^-  form:m
  |=  s=state
  =/  =tang  (expect-eq:test expected actual)
  ?~(tang &+[~ s] |+tang)
::
++  ex-boat
  |=  [=dude:gall =boat:gall]
  =/  m  (mare ,~)
  ::NOTE  if only ;<  let us specify just a face...
  ;<  e=_+:*$>(%live egg:gall)  bind:m  (get-live-egg dude)
  (ex-equal !>(boat.e) !>(boat))
::
++  ex-boar
  |=  [=dude:gall =boar:gall]
  =/  m  (mare ,~)
  ::NOTE  if only ;<  let us specify just a face...
  ;<  e=_+:*$>(%live egg:gall)  bind:m  (get-live-egg dude)
  (ex-equal !>(boar.e) !>(boar))
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
++  use-wire
  |=  [=dude:gall =wire]
  =/  m  (mare ,^wire)
  ;<  e=_+:*$>(%live egg:gall)  bind:m  (get-live-egg dude)
  (pure:m %use dude run-nonce:e wire)
::
++  a2a-wire
  |=  [=dude:gall =dock:gall =wire]
  =/  m  (mare ,^wire)
  ;<  y=yoke:gall  bind:m  (get-yoke dude)
  ?.  ?=(%live -.y)  (pure:m ~)
  (use-wire dude %out (scot %p p.dock) q.dock [(scot %ud sub-nonce.y) wire])
::
++  gall-deal
  |=  [agent=dude:gall task-agent=task:agent:gall]
  ^-  task:gall
  [%deal [~dev ~dev /gall/desk] agent task-agent]
::
::  tests
::
::
++  test-subscribe
  %-  eval-mare
  ;<  *  bind:m  (do-load [[%agent-a easy:mock] [%mock easy:mock] ~])
  ::
  ::  start subscription from %mock to %agent-a
  ;<  nonce-old=[@t @ud]  bind:m  (get-nonce %mock)
  ;<  wir=wire        bind:m
    (a2a-wire %mock [~dev %agent-a] /agent/wire)
  ;<  ~  bind:m  (do-watch wir /agent/wire [%watch /sub/path])
  ::
  ;<  nonce=[@t @ud]  bind:m  (get-nonce %mock)
  ;<  ~  bind:m  (ex-equal !>(-.nonce) !>(-.nonce-old))
  ;<  ~  bind:m  (ex !>(=(+.nonce +(+.nonce-old))))
  ;<  ~  bind:m  (ex-boat %mock (malt [[/agent/wire ~dev %agent-a]^[| /sub/path] ~]))
  ;<  ~  bind:m  (ex-boar %mock (malt [[/agent/wire ~dev %agent-a]^+:nonce-old ~]))
  ::
  ::  pass %watch-ack from %agent-a back to %mock
  ;<  take-watch-ack=(list move:gall)  bind:m
    (do-take [wir ~[/sysduct]] [%gall %unto %watch-ack ~])
  ;<  ~  bind:m
    %+  ex-moves  take-watch-ack
    :~  (ex-on-agent /agent/wire %watch-ack ~)
    ==
  ;<  ~  bind:m  (ex-boat %mock (malt [[/agent/wire ~dev %agent-a]^[& /sub/path] ~]))
  ;<  ~  bind:m  (ex-boar %mock (malt [[/agent/wire ~dev %agent-a]^+:nonce-old ~]))
  ;<  ~  bind:m
    %+  ex-moves  take-watch-ack
    :~  (ex-on-agent /agent/wire %watch-ack ~)
    ==
  ::
  ::  start new subscription on a different path from %mock to %agent-a
  ;<  foo-wir=wire        bind:m
    (a2a-wire %mock [~dev %agent-a] /agent/wire/foo)
  ;<  ~  bind:m  (do-watch foo-wir /agent/wire/foo [%watch /sub/path-new])
  ::
  ;<  nonce-new=[@t @ud]  bind:m  (get-nonce %mock)
  ;<  ~  bind:m  (ex-equal !>(-.nonce-new) !>(-.nonce))
  ;<  ~  bind:m  (ex !>(=(+.nonce-new +(+.nonce))))
  ;<  ~  bind:m
    %+  ex-boat  %mock
    %-  malt  %-  limo
    :~  [[/agent/wire/foo ~dev %agent-a] [| /sub/path-new]]
        [[/agent/wire ~dev %agent-a] [& /sub/path]]
    ==
  =/  boar-ex
    %-  malt  %-  limo
    :~  [[/agent/wire/foo ~dev %agent-a] +:nonce]
        [[/agent/wire ~dev %agent-a] +:nonce-old]
    ==
  ;<  ~  bind:m  (ex-boar %mock boar-ex)
  ::
  ;<  *  bind:m  (do-take [foo-wir ~[/sysduct]] [%gall %unto %watch-ack ~])
  ;<  ~  bind:m
    %+  ex-boat  %mock
    %-  malt  %-  limo
    :~  [[/agent/wire/foo ~dev %agent-a] [& /sub/path-new]]
        [[/agent/wire ~dev %agent-a] [& /sub/path]]
    ==
  (ex-boar %mock boar-ex)
::
::  agent to agent subscription, reload subscriber agent
++  test-subscribe-and-reload
  %-  eval-mare
  ;<  *  bind:m  (do-load [[%agent-a easy:mock] [%mock easy:mock] ~])
  ::
  ;<  old-nonce=[@t @ud]  bind:m  (get-nonce %mock)
  ;<  wir=wire      bind:m
    (a2a-wire %mock [~dev %agent-a] /agent/wire)
  ::
  ;<  ~  bind:m  (do-watch wir /agent/wire [%watch /sub/path])
  ;<  ~  bind:m  (ex-boat %mock (malt [[/agent/wire ~dev %agent-a]^[| /sub/path] ~]))
  ;<  ~  bind:m  (ex-boar %mock (malt [[/agent/wire ~dev %agent-a]^1 ~]))
  ::
  ::  pass %watch-ack from %agent-a back to %mock
  ;<  take-watch-ack=(list move:gall)  bind:m
    (do-take [wir ~[/sysduct]] [%gall %unto %watch-ack ~])
  ;<  ~  bind:m  (ex-boat %mock (malt [[/agent/wire ~dev %agent-a]^[& /sub/path] ~]))
  ;<  ~  bind:m  (ex-boar %mock (malt [[/agent/wire ~dev %agent-a]^1 ~]))
  ;<  ~  bind:m
    %+  ex-moves  take-watch-ack
    :~  (ex-on-agent /agent/wire %watch-ack ~)
    ==
  ::
  ::  reload %mock with changes
  =/  =agent:gall
    %-  make:mock
    [%on-fail |=(* ~)]~
  ;<  moz-load=(list move:gall)  bind:m  (do-load [[%agent-a easy:mock] [%mock agent] ~])
  ;<  nonce=[@t @ud]  bind:m  (get-nonce %mock)
  ;<  ~  bind:m  (ex-equal !>(-.nonce) !>(-.old-nonce))
  ;<  ~  bind:m  (ex !>(=(+.nonce +(+.old-nonce))))
::   ;<  ~  bind:m
    %+  ex-moves  moz-load
    :~  (ex-move default-duct %pass /sys/say [%d [%text "gall: bumped %mock"]])
        (ex-echo !>(!>(~)))
    ==
::   ;<  give-fact=(list move:gall)  bind:m
::     %+  do-deal  %agent-a
::     :+  %poke  %test-card
::     !>([%give %fact ~[/sub/path] %test-fact !>(~)])
:: ::   ~&  give-fact/give-fact
::   ;<  take-fact=(list move:gall)  bind:m
::   (do-take [wir ~[/sysduct]] [%gall %unto %fact %test-fact !>(~)])
::   ~&  take-fact/take-fact
::   %+  ex-moves  ~  ~
::
::  agent to agent subscription, nuke and revive subscriber
++  test-nonce-nuke-and-revive
  %-  eval-mare
  ;<  *  bind:m  (do-load [[%mock easy:mock] [%agent-a (make:mock give-fact:mock)] ~])
  ::
  ::  subscribe from %mock to %agent-a
  ;<  old-nonce=[@t @ud]  bind:m  (get-nonce %mock)
  ;<  wir=wire      bind:m
    (a2a-wire %mock [~dev %agent-a] /agent/wire)
  ;<  *  bind:m  (do-watch wir /agent/wire [%watch /sub/path])
  ::
  ;<  nonce=[@t @ud]  bind:m  (get-nonce %mock)
  ;<  ~  bind:m  (ex-equal !>(-.nonce) !>(-.old-nonce))
  ;<  ~  bind:m  (ex !>(=(+.nonce +(+.old-nonce))))
  ;<  ~  bind:m  (ex-boat %mock (malt [[/agent/wire ~dev %agent-a]^[| /sub/path] ~]))
  ;<  ~  bind:m  (ex-boar %mock (malt [[/agent/wire ~dev %agent-a]^+:old-nonce ~]))
  ::
  ;<  take-watch-ack=(list move:gall)  bind:m
    (do-take [wir ~[/sysduct]] [%gall %unto %watch-ack ~])
  ;<  ~  bind:m  (ex-boat %mock (malt [[/agent/wire ~dev %agent-a]^[& /sub/path] ~]))
  ;<  ~  bind:m  (ex-boar %mock (malt [[/agent/wire ~dev %agent-a]^+:old-nonce ~]))
  ;<  ~  bind:m
    %+  ex-moves  take-watch-ack
    :~  (ex-on-agent /agent/wire %watch-ack ~)
    ==
  ::
  ::  nuke and restart subscriber
  ;<  moz-nuke=(list move:gall)  bind:m  (do-nuke %mock)
  ;<  ~  bind:m
    %+  ex-moves   moz-nuke
    :~  (ex-move ~[/sysduct] %pass wir [%g (gall-deal %agent-a [%leave ~])])
    ==
  ;<  moz=(list move:gall)  bind:m  (do-call ~[/sysduct] (gall-deal %agent-a [%leave ~]))
  ::
  ;<  moz-revive=(list move:gall)  bind:m
    (do-load [[%mock easy:mock] [%agent-a (make:mock give-fact:mock)] ~])
  ;<  ~  bind:m
    %+  ex-moves  moz-revive
    :~  (ex-move default-duct %pass /sys/say [%d [%text "gall: booted %mock"]])
    ==
  ::
  ;<  new-nonce=[@t @ud]  bind:m  (get-nonce %mock)
  (ex !>(!=(-.nonce -.new-nonce)))
::
++  test-subscribe-and-suspend
  %-  eval-mare
  ;<  *  bind:m  (do-load [[%agent-a easy:mock] [%mock easy:mock] ~])
  ::
  ;<  old-nonce=[@t @ud]  bind:m  (get-nonce %mock)
  ;<  wir=wire      bind:m
    (a2a-wire %mock [~dev %agent-a] /agent/wire)
  ::
  ;<  ~  bind:m  (do-watch wir /agent/wire [%watch /sub/path])
  ;<  ~  bind:m  (ex-boat %mock (malt [[/agent/wire ~dev %agent-a]^[| /sub/path] ~]))
  ;<  ~  bind:m  (ex-boar %mock (malt [[/agent/wire ~dev %agent-a]^1 ~]))
  ::
  ::  pass %watch-ack from %agent-a back to %mock
  ;<  take-watch-ack=(list move:gall)  bind:m
    (do-take [wir ~[/sysduct]] [%gall %unto %watch-ack ~])
  ;<  ~  bind:m  (ex-boat %mock (malt [[/agent/wire ~dev %agent-a]^[& /sub/path] ~]))
  ;<  ~  bind:m  (ex-boar %mock (malt [[/agent/wire ~dev %agent-a]^1 ~]))
  ;<  ~  bind:m
    %+  ex-moves  take-watch-ack
    :~  (ex-on-agent /agent/wire %watch-ack ~)
    ==
  ::
  ::  suspend %mock with changes
  ;<  moz-suspend=(list move:gall)  bind:m
    (do-call ~ %idle %mock)
  ;<  nonce=[@t @ud]  bind:m  (get-nonce %mock)
  ;<  ~  bind:m  (ex-equal !>(-.nonce) !>(-.old-nonce))
  ;<  ~  bind:m  (ex !>(=(+.nonce +(+.old-nonce))))
  %+  ex-moves  moz-suspend
  :~  (ex-move ~[/sysduct] %pass wir [%g (gall-deal %agent-a [%leave ~])])
  ==
::
::  agent subscribed to a nuked agent, agent gets revived
++  test-subscribe-to-nuked-agent
  %-  eval-mare
  %+  (merge ,~)
    :~  :-  'load and nuke %agent-a before watch'
        ;<  *  bind:m  (do-load [%agent-a easy:mock]~)
        ;<  *  bind:m  (do-nuke %agent-a)
        (pure:m ~)
        :-  'do nothing before watch'
        (pure:m ~)
    ==
  |=  ~
  ;<  *  bind:m  (do-load [%mock easy:mock]~)
  ::
  =/  watch-task=task:agent:gall  [%watch /sub/path]
  =/  deal-watch  (gall-deal %agent-a watch-task)
  =/  deal-leave  (gall-deal %agent-a [%leave ~])
  ;<  old-nonce=[@t @ud]  bind:m  (get-nonce %mock)
  ;<  wir=wire      bind:m
    (a2a-wire %mock [~dev %agent-a] /agent/wire)
  ::
  ::  start subscription to nuked %agent-a
  ;<  moz-watch=(list move:gall)  bind:m
    (mock-card %pass /agent/wire %agent [~dev %agent-a] watch-task)
  ;<  ~  bind:m
    %+  ex-moves  moz-watch
    :~  (ex-move default-duct %give %unto %poke-ack ~)
        (ex-move ~[/sysduct] %pass wir [%g deal-watch])
    ==
  ::
  ;<  moz-deal-watch=(list move:gall)  bind:m  (do-call [/sysduct]~ deal-watch)
  ;<  nonce=[@t @ud]  bind:m  (get-nonce %mock)
  ;<  ~  bind:m  (ex-equal !>(-.nonce) !>(-.old-nonce))
  ;<  ~  bind:m  (ex !>(=(+.nonce +(+.old-nonce))))
  ;<  ~  bind:m  (ex-moves moz-deal-watch ~)
  ;<  ~  bind:m  (ex-boat %mock (malt [[/agent/wire ~dev %agent-a]^[| /sub/path] ~]))
  ;<  ~  bind:m  (ex-boar %mock (malt [[/agent/wire ~dev %agent-a]^1 ~]))
  ::
  %-  branch
  :~  :-  'wait for %watch-ack'
      ;<  moz-load=(list move:gall)  bind:m
        (do-load [[%agent-a easy:mock] [%mock easy:mock] ~])
      ;<  ~  bind:m
        %+  ex-moves  moz-load
        :~  (ex-move ~[/sysduct] %slip %g deal-watch)
            (ex-move default-duct %pass /sys/say [%d [%text "gall: booted %agent-a"]])
        ==
      ::
      ::  pass blocked queued cards to %agent-a
      ;<  moz-deal-watch=(list move:gall)  bind:m
        (do-call ~[/sysduct] deal-watch)
      ;<  ~  bind:m
        %+  ex-moves  moz-deal-watch
        :~  (ex-move ~[/sysduct] %give %unto %watch-ack ~)
        ==
      ::
      ::  pass %watch-ack from %agent-a back to %mock
      ;<  take-watch-ack=(list move:gall)  bind:m
        (do-take [wir ~[/sysduct]] [%gall %unto %watch-ack ~])
      ;<  ~  bind:m
        %+  ex-moves  take-watch-ack
        :~  (ex-on-agent /agent/wire %watch-ack ~)
        ==
      ::
      ;<  ~  bind:m  (ex-boat %mock (malt [[/agent/wire ~dev %agent-a]^[& /sub/path] ~]))
      (ex-boar %mock (malt [[/agent/wire ~dev %agent-a]^1 ~]))
    ::
      :-  'leave subscription'
      ;<  moz-leave=(list move:gall)  bind:m  (mock-card %pass /agent/wire %agent [~dev %agent-a] [%leave ~])
      ;<  ~  bind:m
        %+  ex-moves  moz-leave
        :~  (ex-move default-duct %give %unto %poke-ack ~)
            (ex-move ~[/sysduct] %pass wir [%g deal-leave])
        ==
      ::
      ;<  *  bind:m  (do-call [/sysduct]~ deal-leave)
      ;<  ~  bind:m  (ex-boat %mock ~)
      ;<  ~  bind:m  (ex-boar %mock ~)
      ::
      ::  start %agent-a
      ;<  moz-load=(list move:gall)  bind:m
        (do-load [[%agent-a easy:mock] [%mock easy:mock] ~])
      ;<  ~  bind:m
        %+  ex-moves  moz-load
        :~  (ex-move ~[/sysduct] %slip %g deal-watch)
            (ex-move ~[/sysduct] %slip %g deal-leave)
            (ex-move default-duct %pass /sys/say [%d [%text "gall: booted %agent-a"]])
        ==
      ::
      ::  pass blocked queued cards to %agent-a
      ;<  moz-deal-watch=(list move:gall)  bind:m
        (do-call ~[/sysduct] deal-watch)
      ;<  ~  bind:m
        %+  ex-moves  moz-deal-watch
        :~  (ex-move ~[/sysduct] %give %unto %watch-ack ~)
        ==
      ::
      ::  pass %watch-ack from %agent-a to %mock
      ;<  take-watch-ack=(list move:gall)  bind:m
        (do-take [wir ~[/sysduct]] [%gall %unto %watch-ack ~])
      ;<  ~  bind:m  (ex-moves take-watch-ack ~)
      ::  pass %leave
      ;<  moz-deal-leave=(list move:gall)  bind:m
        (do-call ~[/sysduct] deal-leave)
      (ex-moves moz-deal-leave ~)
  ==
::
--