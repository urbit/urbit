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
  ++  echo  |=(v=vase `[%pass /echo %agent [~dev %mock] %poke %noun v])
  ::
  ++  make  ::  from stock
    |=  a=(list (pair arm fec))
    %-  full
    :*  ::  always echo callback arms
        ::
        :-  %on-agent  echo:mock
        :-  %on-arvo   echo:mock
        :-  %on-fail   |=(=vase ~&([%mock-on-fail !<([frag:agent:gall term] vase)] ~))  ::TODO  echo?
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
    ++  on-fail   |=([=frag:agent:gall =call:agent:gall] [(c %on-fail !>([frag -.call])) this])
    ::
    ++  on-save   !>(~)
    ++  on-peek
      |=  =path
      ^-  (unit (unit cage))
      ?.  ?=([%c %foo %bar @ ~] path)
        ~
      =/  =ship  (slav %p i.t.t.t.path)
      ?:  =(~dev ship)
        ``[%noun !>(%.y)]
      ``[%noun !>(%.n)]
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
::
++  do-load
  |=  [=dude:gall =agent:gall]
  %-  do
  |=  g=_(g:*state)
  ::NOTE  %desk is a desk name, shows up provenance path of a2a %deals
  ::      originating from our agent.
  =/  perm=[(set perm:gall) (set perm:gall)]
    [[[%super ~] ~ ~] ~]
  (call:g default-duct ~ %load [dude [our.g %desk da+now.g] perm agent]~)
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
++  get-sky
  |=  =dude:gall
  =/  m  (mare farm:gall)
  ^-  form:m
  ;<  e=_+:*$>(%live egg:gall)  bind:m  (get-live-egg dude)
  (pure:m sky.e)
::
++  tap-plot
  |=  [wer=path =farm:gall]
  |-  ^-  (list [path plot:gall])
  =*  tap-plot  $
  ?:  ?=(%coop -.farm)
    =/  fal  ~(tap by q.farm)
    ?~  fal  [wer *plot:gall]~
    %+  turn  fal
    |=  [=path plot=plot:gall]
    [(welp wer path) plot]
  %+  welp  ?~(p.farm ~ [wer u.p.farm]~)
  %-  zing
  %+  turn  ~(tap by q.farm)
  |=  [seg=@ta f=farm:gall]
  ^-  (list [path plot:gall])
  tap-plot(wer (snoc wer seg), farm f)
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
  ?.  ?=([%use @ @ %out @ @ %echo ~] p.move)
    (expect-eq:test !>(p.move) !>(`wire`/use/some-dude/some-nonce/out/some-ship/some-name/echo))
  ?.  ?=([%g %deal * * %poke *] q.move)
    %+  expect-eq:test  !>(q.move)
    !>([%g %deal *sack:gall %mock %poke `cage`[%noun vase]])
  ?>  ?=([%poke *] r.q.move)
  (expect-eq:test q.cage.r.q.move vase)
::
++  ex-on-arvo
  |=  [=wire gift=gift-user-v1:gall]
  (ex-echo !>(+<))
::
++  ex-on-agent
  |=  [=wire =sign:agent:gall]
  (ex-echo !>(+<))
::
++  ex-sky
  |=  [=dude:gall rez=(map path [@ud (unit ~)])]
  =/  m  (mare ,~)
  ;<  =farm:gall  bind:m  (get-sky dude)
  =/  plots=(map path plot:gall)
    %-  malt  %-  limo
    (tap-plot / farm)
  ;<  ~  bind:m
    (ex-equal !>(~(key by plots)) !>(~(key by rez)))
  =/  l-rez=(list [path @ud (unit ~)])
    (turn ~(tap by rez) |=([=path [rev=@ud data=(unit ~)]] [path rev data]))
  =-  (ex-equal !>(-) !>(l-rez))
  ^-  (list [path @ud (unit ~)])
  %+  turn  l-rez
  |=  [=path *]
  =/  =plot:gall  (~(got by plots) path)
  ?~  fan.plot
    ?~  bob.plot  [path 0 ~]
    [path u.bob.plot ~]
  =/  on-fans  ((on @ud (pair @da (each page @uvI))) lte)
  ?~  latest=(ram:on-fans fan.plot)  !!
  ::  checking if has data
  ?^  p.q.val.+.u.latest  [path -.u.latest `~]
  [path -.u.latest ~]
::
++  ex-file-at
  |=  [=dude:gall case=@ud =path rez=?(%null %hash %data)]
  =/  m  (mare ,~)
  ;<  =farm:gall  bind:m  (get-sky dude)
  =/  plots
    (malt ~(tap-plot of-farm:gall farm))
  ?.  (~(has by plots) path)  (fail:m leaf+"{<dude>} farm missing {<path>}")
  =/  =plot:gall  (~(got by plots) path)
  =/  on-fans  ((on @ud (pair @da (each page @uvI))) lte)
  =-  (ex-equal !>(-) !>(rez))
  ?~  fan.plot  %null
  =/  data  (get:on-fans fan.plot case)
  ?~  data  %null
  ?^  p.q.u.data  %data
  %hash
::
++  ex-gem
  |=  [=dude:gall rez=(list coop:gall)]
  =/  m  (mare ,~)
  ^-  form:m
  ;<  e=_+:*$>(%live egg:gall)  bind:m  (get-live-egg dude)
  =/  gem=(list coop:gall)  ~(tap in ~(key by gem.e))
  (ex-equal !>(gem) !>(rez))
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
:: tests
::
++  test-germ-and-tend
  %-  eval-mare
  =/  coop  /foo
  ;<  *  bind:m  (do-load %mock easy:mock)
  ;<  moz-germ=(list move:gall)  bind:m
    (mock-card %pass /agent/wire %arvo %ames %germ coop)
  ::
  =/  plug-wire  [%key %mock '0w3.lBw1H' %pug coop]
  ;<  ~  bind:m
    %+  ex-moves  moz-germ
    :~  (ex-move default-duct %pass plug-wire [%a %plug [%g %x %mock %$ '1' coop]])
        (ex-move default-duct %give %unto %poke-ack ~)
    ==
  ;<  ~  bind:m  (ex-gem %mock coop ~)
  ::  sends %pass to %ames %plug to handle key reservation
  ::
  =/  kid=@ud  1
  =/  key=@   (shaz 1)
  ;<  moz-plug=(list move:gall)  bind:m
    (do-take [plug-wire ~[/sysduct]] %ames %stub kid key)
  ;<  ~  bind:m  (ex-moves moz-plug ~)
  ::
  ;<  ~  bind:m  (ex-sky %mock (malt (limo [/foo [0 ~]]~)))
  ;<  ~  bind:m  (ex-gem %mock coop ~)
  ::
  ;<  moz-tend=(list move:gall)  bind:m
    (mock-card %pass /agent/wire %arvo %ames %tend coop /bar [%txt 'foo'])
  ;<  ~  bind:m
    %+  ex-moves  moz-tend
    :~  (ex-move default-duct %give %unto %poke-ack ~)
    ==
  ;<  ~  bind:m  (ex-sky %mock (malt (limo [/foo/bar [1 `~]]~)))
  ;<  ~  bind:m  (ex-gem %mock coop ~)
  ::  nuking and reviving agent
  ::
  ;<  *  bind:m  (do-call ~ %nuke %mock)
  ;<  *  bind:m  (do-load %mock easy:mock)
  ::  rebind on the same path
  ::
  ;<  moz-germ=(list move:gall)  bind:m
    (mock-card %pass /agent/wire %arvo %ames %germ coop)
  ::
  ;<  ~  bind:m
    %+  ex-moves  moz-germ
    :~  (ex-move default-duct %pass plug-wire [%a %plug [%g %x %mock %$ '1' coop]])
        (ex-move default-duct %give %unto %poke-ack ~)
    ==
  ;<  moz-plug=(list move:gall)  bind:m
    (do-take [plug-wire ~[/sysduct]] %ames %stub kid key)
  ::
  ;<  ~  bind:m  (ex-moves moz-plug ~)
  ;<  ~  bind:m  (ex-sky %mock (malt (limo [/foo/bar [1 ~]]~)))
  ::
  ;<  moz-tend=(list move:gall)  bind:m
    (mock-card %pass /agent/wire %arvo %ames %tend coop /bar [%txt 'foo new'])
  ;<  ~  bind:m
    %+  ex-moves  moz-tend
    :~  (ex-move default-duct %give %unto %poke-ack ~)
    ==
  (ex-sky %mock (malt (limo [/foo/bar [2 `~]]~)))
::
++  test-make-brood
  %-  eval-mare
  =/  coop  /foo/bar
  ;<  *  bind:m  (do-load %mock easy:mock)
  ;<  moz-germ=(list move:gall)  bind:m
    (mock-card %pass /agent/wire %arvo %ames %germ coop)
  ::
  =/  plug-wire  [%key %mock '0w3.lBw1H' %pug coop]
  ;<  ~  bind:m
    %+  ex-moves  moz-germ
    :~  (ex-move default-duct %pass plug-wire [%a %plug [%g %x %mock %$ '1' coop]])
        (ex-move default-duct %give %unto %poke-ack ~)
    ==
  ::  sends %pass to %ames %plug to handle key reservation
  ::
  =/  kid=@ud  1
  =/  key=@   (shaz 1)
  ;<  moz-plug=(list move:gall)  bind:m
    (do-take [plug-wire ~[/sysduct]] %ames %stub kid key)
  ;<  ~  bind:m  (ex-moves moz-plug ~)
  ::
  ;<  moz-tend=(list move:gall)  bind:m
    (mock-card %pass /agent/wire %arvo %ames %tend coop /some/path [%txt 'foo'])
  ;<  ~  bind:m
    %+  ex-moves  moz-tend
    :~  (ex-move default-duct %give %unto %poke-ack ~)
    ==
  ::
  ;<  moz=(list move:gall)  bind:m
    (do-call ~ %plea ~dev [%g /gk/mock [%0 %$ '1' coop]])
  =/  brood  [coop [1 kid key]]
  %+  ex-moves  moz
  :~  (ex-move default-duct %give %boon %0 `brood)
      (ex-move default-duct %give %done ~)
  ==
::
::
++  test-grow-re-bind-after-nuke
  %-  eval-mare
  =/  gall-wire  (a2k-wire %mock /agent/wire `~2026.2.2)
  ;<  *  bind:m  (do-load %mock easy:mock)
  ;<  *  bind:m
    (mock-card %pass /agent/wire %arvo %ames %grow /foo/bus [%txt 'foo'])
  ;<  *  bind:m
    (mock-card %pass /agent/wire %arvo %ames %grow /bar [%txt 'bar'])
  ;<  ~  bind:m  (ex-sky %mock (malt (limo [[/foo/bus 1 `~] [/bar 1 `~]~])))
  ::
  ;<  moz=(list move:gall)  bind:m
    (mock-card %pass /agent/wire %arvo %ames %grow /foo/bus [%txt 'new-foo-bus'])
  ;<  ~  bind:m  (ex-sky %mock (malt (limo [[/foo/bus 2 `~] [/bar 1 `~]~])))
  ::  nuke and revive the agent,
  ::  expect sky.yoke to persist with namespace case
  ::
  ;<  *  bind:m  (do-call ~ %nuke %mock)
  ;<  *  bind:m  (do-load %mock easy:mock)
  ;<  ~  bind:m  (ex-sky %mock (malt (limo [[/foo/bus 2 ~] [/bar 1 ~]~])))
  ::  bind data on already existent prior nuke path
  ::  expect case to increase
  ::
  ;<  *  bind:m
    (mock-card %pass /agent/wire %arvo %ames %grow /foo/bus [%txt 'new-foo-bus'])
  (ex-sky %mock (malt (limo [[/foo/bus 3 `~] [/bar 1 ~]~])))
::
++  test-cull
  %-  eval-mare
  =/  gall-wire  (a2k-wire %mock /agent/wire `~2026.2.2)
  ;<  *  bind:m  (do-load %mock easy:mock)
  ;<  *  bind:m
    (mock-card %pass /agent/wire %arvo %ames %grow /foo/bus [%txt 'foo'])
  ;<  *  bind:m
    (mock-card %pass /agent/wire %arvo %ames %grow /foo/bus [%txt 'new foo'])
  ;<  ~  bind:m
    %+  ex-sky  %mock
    (malt (limo [/foo/bus [2 `~]]~))
  %-  branch
  :~  :-  '%cull at latest'
      ;<  *  bind:m
        (mock-card %pass /agent/wire %arvo %ames %cull [%ud 2] /foo/bus)
      ;<  ~  bind:m  (ex-file-at %mock 1 /foo/bus %null)
      ;<  ~  bind:m  (ex-sky %mock (malt (limo [/foo/bus [2 ~]]~)))
      ::  nuke and revive the agent,
      ::  expect to persist with namespace case
      ::
      ;<  *  bind:m  (do-call ~ %nuke %mock)
      ;<  *  bind:m  (do-load %mock easy:mock)
      ;<  ~  bind:m  (ex-sky %mock (malt (limo [/foo/bus [2 ~]]~)))
      ;<  *  bind:m
        (mock-card %pass /agent/wire %arvo %ames %grow /foo/bus [%txt 'new foo'])
      (ex-sky %mock (malt (limo [/foo/bus [3 `~]]~)))
    ::
      :-  '%cull at 1'
      ;<  *  bind:m
        (mock-card %pass /agent/wire %arvo %ames %cull [%ud 1] /foo/bus)
      ;<  ~  bind:m  (ex-file-at %mock 1 /foo/bus %null)
      (ex-sky %mock (malt (limo [/foo/bus [2 `~]]~)))
  ==
::
  ++  test-tomb
  %-  eval-mare
  =/  gall-wire  (a2k-wire %mock /agent/wire `~2026.2.2)
  ;<  *  bind:m  (do-load %mock easy:mock)
  ;<  *  bind:m
    (mock-card %pass /agent/wire %arvo %ames %grow /foo/bus [%txt 'foo'])
  ;<  *  bind:m
    (mock-card %pass /agent/wire %arvo %ames %grow /foo/bus [%txt 'new foo'])
  ;<  ~  bind:m  (ex-sky %mock (malt (limo [/foo/bus [2 `~]]~)))
  %-  branch
  :~  :-  '%tomb at latest'
      ;<  *  bind:m
        (mock-card %pass /agent/wire %arvo %ames %tomb [%ud 2] /foo/bus)
      ;<  ~  bind:m  (ex-sky %mock (malt (limo [/foo/bus [2 ~]]~)))
      ::  nuke and revive the agent,
      ::  expect to persist with namespace case
      ::
      ;<  *  bind:m  (do-call ~ %nuke %mock)
      ;<  *  bind:m  (do-load %mock easy:mock)
      ;<  ~  bind:m  (ex-sky %mock (malt (limo [/foo/bus [2 ~]]~)))
      ;<  *  bind:m
        (mock-card %pass /agent/wire %arvo %ames %grow /foo/bus [%txt 'new foo'])
      (ex-sky %mock (malt (limo [/foo/bus [3 `~]]~)))
    ::
      :-  '%tomb at 1'
      ;<  *  bind:m
        (mock-card %pass /agent/wire %arvo %ames %tomb [%ud 1] /foo/bus)
      ;<  ~  bind:m  (ex-file-at %mock 1 /foo/bus %hash)
    ::
      (ex-sky %mock (malt (limo [/foo/bus [2 `~]]~)))
  ==
--