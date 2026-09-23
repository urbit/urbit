::  gall 2026 resource tracking tests
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
::
++  do-load
  |=  [=dude:gall =agent:gall super=(each ~ (set perm:gall))]
  %-  do
  |=  g=_(g:*state)
  ::NOTE  %desk is a desk name, shows up provenance path of a2a %deals
  ::      originating from our agent.
  =/  perm=[(set perm:gall) (set perm:gall)]
    ?:  ?=(%.n -.super)  [(~(put in p.super) [%write [~ %mock]]) ~]
    [[[%super ~] ~ ~] ~]
  (call:g default-duct ~ %load [dude [our.g %desk da+now.g] perm agent]~)
::
++  do-load-mock-super
  ::NOTE  grant super permissions so that perm checks always pass
  (do-load %mock easy:mock [%.y ~])
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
  (pure:m ?>(?=(%21 -.e) +.e))
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
  =/  m  (mare ,^wire)
  ;<  y=yoke:gall  bind:m  (get-yoke dude)
  ?.  ?=(%live -.y)  (pure:m ~)
  (use-wire dude %out (scot %p p.dock) q.dock [(scot %ud sub-nonce.y) wire])
::
::  tests
::
+$  tracked-task
  $:  task=task-user-v1:gall
      note=note-arvo
      [res=_+:*arvo-resource:gall det=(unit resource-deet:gall)]
      wire-deets=(unit *)
    ::
      kill=note-arvo     ::  resource deleted by gall
    ::
      signal=(unit gift-user-v1:gall)
      revive=(unit note-arvo)
      perm-req=?
  ==
::
++  tracked-tasks
  ^-  (list tracked-task)
  :~  ^-  tracked-task
      :*  [%behn %wait ~2026.1.2]
          [%b %wait ~2026.1.2]
          [%behn %wait ~2026.1.2]^~
          `~2026.1.2
        ::
          [%b %rest ~2026.1.2]
        ::
          ~
          `[%b %wait ~2026.1.2]
          %.n
      ==
    ::
      ^-  tracked-task
      :*  [%clay %read 'someid' ~fun %desk %sing *mood:clay]
          [%c %warp ~fun %desk ~ %sing *mood:clay]
          [%clay %warp 'someid']^`[%clay %warp ~fun %desk %sing *mood:clay]
          `'someid'
        ::
          [%c %warp ~fun %desk ~]
        ::
          ~
          `[%c %warp ~fun %desk ~ %sing *mood:clay]
          %.y
      ==
    ::
      ^-  tracked-task
      :*  [%clay %read 'someid' ~fun %desk %many | ud+1 ud+3 /foo/hoon]
          [%c %warp ~fun %desk ~ %many | ud+1 ud+3 /foo/hoon]
          [%clay %warp 'someid']^`[%clay %warp ~fun %desk %many | ud+1 ud+3 /foo/hoon]
          `'someid'
        ::
          [%c %warp ~fun %desk ~]
        ::
          ~
          `[%c %warp ~fun %desk ~ %many | ud+1 ud+3 /foo/hoon]
          %.y
      ==
    ::
      ^-  tracked-task
      :*  [%clay %tire `~]
          [%c %tire `~]
          [%clay %tire]^~
          ~
        ::
          [%c %tire ~]
        ::
          ~
          `[%c %tire `~]
          %.y
      ==
      ^-  tracked-task
      :*  [%clay %ward `~]
          [%c %ward ~]
          [%clay %ward]^~
          ~
        ::
          [%c %wink ~]
        ::
          ~
          `[%c %ward ~]
          %.y
      ==
    ::
      ^-  tracked-task
      :*  [%dill %logs `~]
          [%d %logs `~]
          [%dill %logs]^~
          ~
        ::
          [%d %logs ~]
        ::
          ~
          `[%d %logs `~]
          %.y
      ==
    ::
      ^-  tracked-task
      :*  [%dill %shot %sesh %view ~]
          [%d %shot %sesh %view ~]
          [%dill %view %sesh]^~
          `%sesh
        ::
          [%d %shot %sesh %flee ~]
        ::
          ~
          `[%d %shot %sesh %view ~]
          %.y
      ==
    ::
      ^-  tracked-task
      :*  [%eyre %connect ['foo']~ %foo]
          [%e %connect ['foo']~ %foo]
          [%eyre %binding 'foo' ~]^`[%eyre %binding %foo]
          `%foo
        ::
          [%e %disconnect 'foo' ~]
        ::
          ~
          `[%e %connect ['foo']~ %foo]
          %.y
      ==
    ::
      ^-  tracked-task
      =/  entry=cache-entry:eyre
        [auth=& %payload [200 ~] `[4 'body']]
      :*  [%eyre %set-response '/some/url' `entry]
          [%e %set-response %desk '/some/url' `entry]
          [%eyre %cache '/some/url']^`[%eyre %cache entry]
          ~
        ::
          [%e %set-response %desk '/some/url' ~]
        ::
          ~
          `[%e %set-response %desk '/some/url' `entry]
          %.y
      ==
    ::
      ^-  tracked-task
      :*  [%jael %private-keys `~]
          [%j %private-keys ~]
          [%jael %keys]^`[%jael %keys %private]
          ~
        ::
          [%j %nuke ~]
        ::
          ~
          `[%j %private-keys ~]
          %.y
      ==
    ::
      ^-  tracked-task
      :*  [%jael %public-keys `~]
          [%j %public-keys ~]
          [%jael %keys]^`[%jael %keys %public]
          ~
        ::
          [%j %nuke ~]
        ::
          ~
          `[%j %public-keys ~]
          %.n
      ==
    ::
      ^-  tracked-task
      =/  sis=(set ship)  [~met ~ ~]
      :*  [%jael %public-keys `sis]
          [%j %public-keys sis]
          [%jael %keys]^`[%jael %keys sis]
          ~
        ::
          [%j %nuke sis]
        ::
          ~
          `[%j %public-keys sis]
          %.n
      ==
    ::
      ^-  tracked-task
      :*  [%lick %spin /mysocket]
          [%l %spin [%mock /mysocket]]
          [%lick %spin /mysocket]^~
          ~
        ::
          [%l %shut [%mock /mysocket]]
        ::
          `[%lick %soak /mysocket %disconnect ~]
          `[%l %spin [%mock /mysocket]]
          %.y
      ==
  ==
++  test-normal-tracking-behavior
  %-  zing
  %+  turn  tracked-tasks
  |=  tracked-task
  %-  eval-mare
  ::TODO  put >task< into trace if below results in failure
  ;<  *  bind:m  do-load-mock-super
  ::  create the resource,
  ::  see the task go out to the kernel,
  ::  and check that gall remembers it.
  ::
  ;<  moz=(list move:gall)  bind:m
    (mock-card %pass /agent/wire %arvo task)
  ;<  gall-wire=wire        bind:m
    (a2k-wire %mock /agent/wire wire-deets)
  ;<  ~  bind:m
    %+  ex-moves  moz
    :~  (ex-move default-duct %give %unto %poke-ack ~)
        (ex-move ~[/sysduct] %pass gall-wire note)
    ==
  ;<  ~  bind:m
    %+  ex-resources  %mock
    :~  [/agent/wire res]^det
    ==
  ::
  %-  branch
  :~  :-  'gall suspends, revoke perms & revive'
      ::  suspending the agent should delete its resource
      ::
      ;<  moz=(list move:gall)  bind:m
        (do-call ~ %idle %mock)
      ;<  ~  bind:m
        %+  ex-moves  moz
        :~  (ex-move ~[/sysduct] %pass gall-wire kill)
        ==
      ;<  ~  bind:m
        %+  ex-resources  %mock
        :~  [/agent/wire res]^det
        ==
      ;<  y=yoke:gall  bind:m  (get-yoke %mock)
      ?.  &(?=(%live -.y) ?=(%| -.agent.y))
        (fail:m 'agent not suspended' ~)
      ::  reviving the agent with revoked permissions to resource
      ::  should drop resource and send gift notifying agent of resource revokation
      ::
      ;<  moz=(list move:gall)  bind:m
        (do-load %mock easy:mock [%.n ~])
      ?:  perm-req
        ;<  ~  bind:m
          %+  ex-moves  moz
          :-  (ex-move default-duct %pass /sys/say [%d [%text "gall: bumped %mock"]])
              [(ex-on-arvo /agent/wire [%revoked [/agent/wire res]]) ~]
        (ex-resources %mock ~)
      ;<  ~  bind:m
        %+  ex-moves  moz
        :-  (ex-move default-duct %pass /sys/say [%d [%text "gall: bumped %mock"]])
        =;  mos=(list (unit $-(move:gall tang)))
          (murn mos same)
        :~  ?~(signal ~ `(ex-on-arvo /agent/wire u.signal))
            ?~(revive ~ `(ex-move ~[/sysduct] %pass gall-wire u.revive))
        ==
      (ex-resources %mock [/agent/wire res]^det ~)
    ::
      :-  'revoke perm'
      ;<  moz=(list move:gall)  bind:m
          (do-load %mock easy:mock [%.n ~])
      ?:  perm-req
        ;<  ~  bind:m
          %+  ex-moves  moz
          :-  (ex-move default-duct %pass /sys/say [%d [%text "gall: bumped %mock"]])
          :-  (ex-move ~[/sysduct] %pass gall-wire kill)
              [(ex-on-arvo /agent/wire [%revoked [/agent/wire res]]) ~]
        (ex-resources %mock ~)
      ;<  ~  bind:m
        %+  ex-moves  moz
        [(ex-move default-duct %pass /sys/say [%d [%text "gall: bumped %mock"]]) ~]
      (ex-resources %mock [/agent/wire res]^det ~)
  ==
::
++  test-suspend-and-revive
  %-  eval-mare
  ;<  *  bind:m  do-load-mock-super
  =/  =rave:clay  [%sing %x ud+1 /some/txt]
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
  ::
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %behn %wait ~2345.6.7)
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %clay %read 123 ~zod %desk rave)
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %eyre %connect /x %dude)
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %iris %request *request:http *outbound-config:iris)
  ;<  *  bind:m  (mock-card %pass /agent/wire %arvo %lick %spin /mysocket)
  ;<  *  bind:m  (mock-card %pass /agent/wire %agent [~fun %bar] %watch /blah)
  ;<  moz=(list move:gall)  bind:m
    (do-call ~ %idle %mock)
  ::
  =/  resources=(lest [res=_+:*arvo-resource:gall det=(unit resource-deet:gall)])
    :~  [%behn %wait ~2345.6.7]^~
        [%clay %warp 123]^`[%clay %warp ~zod %desk rave]
        [%eyre %binding /x]^`[%eyre %binding %dude]
        [%iris %request]^~
        [%lick %spin /mysocket]^~
    ==
  ::  suspending the agent should "pause" all its resources.
  ::  we delete the resources, but remember them for revival.
  ::
  ;<  ~  bind:m
    ::NOTE  moves sorted because otherwise dependent on set order
    %+  ex-moves  (sort moz aor)
    :~  (ex-move ~[/sysduct] %pass gall-wire [%i %cancel-request ~])
        (ex-move ~[/sysduct] %pass gall-wire [%l %shut [%mock /mysocket]])
        (ex-move ~[/sysduct] %pass gall-wire-e [%e %disconnect /x])
        (ex-move ~[/sysduct] %pass gall-wire-c [%c %warp ~zod %desk ~])
        (ex-move ~[/sysduct] %pass gall-wire-b [%b %rest ~2345.6.7])
        (ex-move ~[/sysduct] %pass gall-wire-a [%g %deal [~dev ~fun /gall/desk] %bar %leave ~])  ::TODO  deal constructor
    ==
  ;<  ~  bind:m
    %+  ex-resources  %mock
    %+  turn  resources
    |=  [res=_+:*arvo-resource:gall det=(unit resource-deet:gall)]
    [/agent/wire res]^det
  ::
  ;<  y=yoke:gall  bind:m  (get-yoke %mock)
  ?.  &(?=(%live -.y) ?=(%| -.agent.y))
    (fail:m 'agent not suspended' ~)
  ::  reviving the agent should reiflate its resources
  ::
  ::  TODO:  branch maybe partial perms ?
  ;<  moz=(list move:gall)  bind:m
    (do-load %mock easy:mock [%.n ~])
  %+  ex-moves  (sort moz aor)
  ;:  welp
    :~  (ex-move default-duct %pass /sys/say [%d [%text "gall: bumped %mock"]])
        (ex-move ~[/sysduct] %pass gall-wire-b [%b %wait ~2345.6.7])
    ==
  ::
    %+  turn  t.resources
    |=  [res=_+:*arvo-resource:gall *]
    (ex-on-arvo /agent/wire [%revoked [/agent/wire res]])
  ::
    [(ex-on-agent /agent/wire %kick ~) ~]
  ==
--