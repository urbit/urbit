::  clay-build: tests for clay's build behavior
::
/+  *test
/=  clay-raw  /sys/vane/clay
/*  lib-def   %hoon  /lib/default-agent/hoon
/*  lib-skel  %hoon  /lib/skeleton/hoon
/*  zus       %hoon  /sys/zuse/hoon
/*  mar-noun  %hoon  /mar/noun/hoon
/*  mar-hoon  %hoon  /mar/hoon/hoon
/*  mar-txt   %hoon  /mar/txt/hoon
/*  mar-kel   %hoon  /mar/kelvin/hoon
/*  mar-mime  %hoon  /mar/mime/hoon
/*  mar-bill  %hoon  /mar/bill/hoon
/*  mar-seal  %hoon  /mar/seal/hoon
::
!:
=/  clay-gate  (clay-raw ~nul)
::
|%
::
::  test engine
::
++  form-raw
  |$  [a]
  $-(state (output-raw a))
::
++  state
  $:  gate=_clay-gate
      now=@da
      eny=@uvJ
  ==
::
++  output-raw
  |$  [a]
  (each [out=a =state] tang)
::
++  mare
  |*  a=mold
  |%
  ++  output  (output-raw a)
  ++  form  (form-raw a)
  ++  easy
    |=  g=$-(state a)
    ^-  form
    |=  =state
    [%& (g state) state]
  ++  pure
    |=  arg=a
    ^-  form
    |=  =state
    [%& arg state]
  ::
  ++  bind
    |*  b=mold
    |=  [m-b=(form-raw b) fun=$-(b form)]
    ^-  form
    |=  =state
    =/  b-res=(output-raw b)  (m-b state)
    ?-  -.b-res
      %&  ((fun out.p.b-res) state.p.b-res)
      %|  [%| p.b-res]
    ==
  --
::
++  eval-mare
  =/  m  (mare ,~)
  |=  computation=form:m
  ::NOTE  use +vi so that we can leverage persistent memoization,
  ::      which is important because we build lull, zuse and clay all the time.
  ::      comment this out if you need more detailed crash traces.
  :: =;  comp  (need (~(mole vi |) comp))
  :: |.  ^-  tang
  =/  res  (computation clay-gate ~1111.1.1 `@uvJ`0xdead.beef)
  ?-  -.res
    %&  ~
    %|  p.res
  ==
::
++  move  move:clay-gate
::
::  state helpers
::
++  wait  ::  pass time
  |=  =@dr
  =/  m  (mare ,~)
  ^-  form:m
  |=  =state
  [%& ~ state(now (add now.state dr))]
::
++  get-now
  =/  m  (mare ,@da)
  ^-  form:m
  |=  =state
  [%& now.state state]
::
++  get-dojo
  |=  =desk
  =/  m  (mare ,dojo:clay-gate)
  ^-  form:m
  |=  =state
  =/  dojo=(unit dojo:clay-gate)  (~(get by dos.rom.ruf.gate.state) desk)
  ?~  dojo  [%| [(rap 3 'missing desk ' desk ~) ~]]
  [%& u.dojo state]
::
::  raise failure
++  fail
  |=  =tang
  |=  =state
  [%| tang]
::
++  read-moves
  |=  [moves=(list move) =state]
  ^+  state
  state
::
++  scry-gate  ^-  roof
  |=  [gang pov=path =view =beam]
  ^-  (unit (unit cage))
  ~
::
++  call
  |=  [=duct wrapped-task=(hobo task:clay-gate)]
  =/  m  (mare ,(list move))
  ^-  form:m
  |=  =state
  =/  clay-core
    %:  gate.state
        now=now.state
        eny=`@uvJ`0xdead.beef
        scry=scry-gate
    ==
  =^  moves  gate.state
    (call:clay-core duct ~ wrapped-task)
  [%& moves (read-moves moves state)]
::
++  take
  |=  [=wire =duct =sign:clay-gate]
  =/  m  (mare ,(list move))
  ^-  form:m
  |=  =state
  =/  clay-core
    %:  gate.state
      now=now.state
      eny=`@uvJ`0xdead.beef
      scry=scry-gate
    ==
  =^  moves  gate.state
    (take:clay-core wire duct ~ sign)
  [%& moves state]
::
++  do-wick
  |=  ex-mov=(list $-(move tang))
  =/  m  (mare ,~)
  ;<  mov=(list move)  bind:m
    (take /wick ~[/blah] [%behn %wake ~])
  (expect-moves mov ex-mov)
::
++  do-pork  (call ~[/blah] [%pork ~])
::
++  do-park
  |=  [=desk kel=@ud fil=(list [path (each page:clay lobe:clay)])]
  %+  call  ~[/blah]
  ^-  (hobo task:clay-gate)
  =/  files
    %-  ~(gas by *(map path (each page:clay lobe:clay)))
    ^-  (list [path (each page:clay lobe:clay)])
    :-  [/sys/kelvin %& kelvin+[%zuse zuse]]
    =-  (welp - fil)
    ?:  =(%base desk)
      :~  ::[/sys/zuse/hoon [%& hoon+zus]]  ::REVIEW  remove?
          [/mar/noun/hoon [%& hoon+mar-noun]]
          [/mar/hoon/hoon [%& hoon+mar-hoon]]
          [/mar/mime/hoon [%& hoon+mar-mime]]
          [/mar/bill/hoon [%& hoon+mar-bill]]
          [/mar/seal/hoon [%& hoon+mar-seal]]
          [/mar/txt/hoon [%& hoon+mar-txt]]
          [/mar/kelvin/hoon [%& hoon+mar-kel]]
      ==
    ?.  =(%foo desk)  ~
    :~  [/app/bar/hoon [%& agent]]
        [/dep/hoon [%& hoon+'~']]
        [/non/hoon [%& hoon+'~']]
        [/desk/bill [%& noun+[%bar]~]]
        [/lib/skeleton/hoon [%& hoon+lib-skel]]
        [/lib/default-agent/hoon [%& hoon+lib-def]]
    ==
  =/  =yoki:clay  [%& [*(list tako:clay) files]]
  [%park desk yoki *rang:clay]
::
++  do-new-desk
  |=  =desk
  (do-park desk zuse ~)
::
+$  bump-data
  $:  =beak
      per=[peg=(set perm:gall) peq=(set perm:gall)]
      bump=(list [dude:gall agent:gall])
  ==
::
++  do-setup
  |=  =desk
  =/  m  (mare bump-data)
  ::  set up the base desk unconditionally
  ::
  ;<  *                 bind:m  (do-new-desk %base)
  ;<  *                 bind:m  do-pork
  ;<  mov=(list move)   bind:m  (call ~[/blah] [%zest %base `zest:clay`%live])
  ;<  ~                 bind:m  (expect-moves mov ex-wick ex-load ~)
  ;<  ~                 bind:m  (do-wick ~)  ::  just a formality
  ::  set up our testing desk
  ::
  ;<  *                 bind:m  (do-new-desk desk)
  ;<  *                 bind:m  (call ~[/blah] [%esse desk %.y])
  ;<  mov=(list move)   bind:m  (call ~[/blah] [%zest desk `zest:clay`%live])
  ;<  ~                 bind:m
    %+  expect-moves  mov
    :~  ex-wick
        ex-bump
    ==
  ;<  ~                 bind:m  (do-wick ~)  ::  just a formality
  (pure:m (extract-bump (snag 1 mov)))
::
++  extract-load
  |=  mov=move
  ^-  load:gall
  ?>(?=([* %pass * %g %load *] mov) load.q.q.mov)
::
++  extract-bump
  |=  mov=move
  ^-  bump-data
  ?>(?=([* %pass * %g %bump *] mov) [beak per bump]:q.q.mov)
::
::  expectation checkers
::
++  expect-moves
  |=  [mos=(list move) exes=(list $-(move tang))]
  =/  m  (mare ,~)
  ^-  form:m
  |=  =state
  =/  =tang
    |-  ^-  tang
    ?~  exes
      ?~  mos
        ~
      ['got more moves than expected' >mos< ~]
    ?~  mos
      ['expected more moves than got' ~]
    %+  weld
      (i.exes i.mos)
    $(exes t.exes, mos t.mos)
  ?~  tang
    [%& ~ state]
  [%| tang]
::
++  ex
  |=  mow=move
  |=  mov=move
  (expect-eq !>(mow) !>(mov))
::
++  ex-gift
  |=  =gift:clay-gate
  (ex ~[/blah] %give gift)
::
++  ex-pass
  |=  [=wire =note:clay-gate]
  (ex ~[/blah] %pass wire note)
::
++  ex-wick
  |=  mov=move
  ?:  ?=([[[%blah ~] ~] %pass [%wick ~] %b %wait @] mov)  ~
  ((ex ~[/blah] %pass /wick %b %wait ~1337.4.20) mov)
::
++  ex-text
  |=  =tape
  (ex ~ %pass /note [%d [%text tape]])
::
++  ex-load
  |=  mov=move
  ?:  ?=([* %pass * [%g [%load *]]] mov)  ~
  :~  'expected %pass %g %load'
      %^  rap  3  'got      %'
      ?.  ?=(%pass -.q.mov)  [-.q.mov ~]
      [- ' %' +< ~]:q.q.mov
  ==
::
++  ex-bump
  |=  mov=move
  ?:  ?=([%pass * [%g [%bump *]]] q.mov)  ~
  :~  'expected %pass %g %bump'
      %^  rap  3  'got      %'
      ?.  ?=(%pass -.q.mov)  [-.q.mov ~]
      [- ' %' +< ~]:q.q.mov
  ==
::
++  ex-what
  |=  mov=move
  ?:  ?=([* %pass * [%$ [%what *]]] mov)  ~
  :~  'expected %pass %$ %what'
      %^  rap  3  'got      %'
      ?.  ?=(%pass -.q.mov)  [-.q.mov ~]
      [- ' %' +< ~]:q.q.mov
  ==
::
::  data constructors
::
++  agent
  ^-  page:clay
  :-  %hoon
  '''
  /+  default-agent
  /*  mar-noun  %hoon  /mar/noun/hoon
  /*  deppy     %hoon  /dep/hoon
  |%
  +$  state-0  [%0 val=@ud]
  --
  =|  state-0
  =*  state  -
  ^-  agent:gall
  |_  =bowl:gall
  +*  this  .
      def   ~(. (default-agent this %.n) bowl)
  ::
  ++  on-init
    ^-  (quip card:agent:gall _this)
    `this(val 42)
  ::
  ++  on-save
    ^-  vase
    !>(state)
  ::
  ++  on-load
    |=  old=vase
    ^-  (quip card:agent:gall _this)
    `this(state [%0 42])
  ::
  ++  on-poke   on-poke:def
  ++  on-watch  on-watch:def
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-agent  on-agent:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
  '''
::
::  tests
::
++  test-rebuild-if-dep-changes
  ::  imported file changes, agent must rebuild
  ::
  %-  eval-mare
  =/  m  (mare ,~)
  ::  general setup, install desk & run the agent
  ::
  ;<  bump=bump-data  bind:m  (do-setup %foo)
  ::  changing a random file should not rebuild the agent
  ::
  ;<  mov=(list move)  bind:m
    %^  do-park  %foo  zuse
    [/non/hoon %& hoon+'2']~
  ;<  ~  bind:m
    %+  expect-moves  mov
    :~  ex-wick
        (ex-text ": /~nul/foo/2/non/hoon")
        ex-bump
    ==
  =/  bump-2=bump-data  (extract-bump (snag 2 mov))
  ::  loadout must not have changed, agents must not have been rebuild
  ::
  ?.  =(bump bump-2)
    (fail:m 'rebuilt/changed agent(s) unexpectedly' ~)
  ::  changing dep.hoon should rebuild the agent
  ::
  ;<  mov=(list move)  bind:m
    %^  do-park  %foo  zuse
    ~[[/dep/hoon %& hoon+'2'] [/non/hoon %& hoon+'2']]
  ;<  ~  bind:m
    %+  expect-moves  mov
    :~  ex-wick
        (ex-text ": /~nul/foo/3/dep/hoon")
        ex-bump
    ==
  =/  bump-3=bump-data  (extract-bump (snag 2 mov))
  ?:  =(bump bump-3)
    (fail:m 'agent(s) not rebuilt/changed when expected' ~)
  (pure:m ~)
::
++  test-rebuild-if-builtin-dep-on-base-changes
  ::  imported file is a built-in, the version of that file on base changes,
  ::  agent must rebuild
  ::
  %-  eval-mare
  =/  m  (mare ,~)
  ::  general setup, install desk & run the agent
  ::
  ;<  bump=bump-data       bind:m  (do-setup %foo)
  ::  changing /mar/noun on base should rebuild the agent
  ::
  ;<  mov=(list move)  bind:m
    %^  do-park  %base  zuse
    [/mar/noun/hoon %& hoon+(cat 3 '::test\0a' mar-noun)]~
  ;<  ~  bind:m
    %+  expect-moves  mov
    :~  (ex-pass /kiln/bump/zeal %c %zeal ~)
        ex-what
        (ex [~[/blah] %slip %c %pork ~])
    ==
  ;<  mov=(list move)  bind:m  do-pork
  ;<  ~  bind:m
    %+  expect-moves  mov
    :~  ex-wick
        (ex-text ": /~nul/base/2/mar/noun/hoon")
        ex-load
    ==
  =/  =load:gall  (extract-load (snag 2 mov))
  =/  load-map
    %+  roll  load
    |=  $:  l=[=dude:gall =beak per=[peg=(set perm:gall) peq=(set perm:gall)] =agent:gall]
            load-map=(map beak [per=[(set perm:gall) (set perm:gall)] (list [dude:gall agent:gall])])
        ==
    ^-  (map beak [per=[(set perm:gall) (set perm:gall)] (list [dude:gall agent:gall])])
    =/  entry=(unit [per=[(set perm:gall) (set perm:gall)] (list [dude:gall agent:gall])])
      (~(get by load-map) beak.l)
    ?~  entry   (~(put by load-map) beak.l [per [[dude agent] ~]]:l)
    (~(put by load-map) beak.l [per.u.entry [[dude agent]:l +.u.entry]])
  ?:  =((~(got by load-map) beak.bump) +.bump)
    (fail:m 'agent(s) not rebuilt/changed when expected' ~)
  (pure:m ~)
::
++  test-rebuild-if-builtin-override-changes
  ::  imported file is a built-in, the agent desk gains its own version,
  ::  agent must rebuild
  ::
  %-  eval-mare
  =/  m  (mare ,~)
  ::  general setup, install desk & run the agent
  ::
  ;<  bump=bump-data    bind:m  (do-setup %foo)
  ::  adding /mar/noun.hoon to %foo desk should rebuild the agent
  ::
  ;<  mov=(list move)  bind:m
    %^  do-park  %foo  zuse
    [/mar/noun/hoon %& hoon+(cat 3 '::test\0a' mar-noun)]~
  ;<  ~  bind:m
    %+  expect-moves  mov
    :~  ex-wick
        (ex-text "+ /~nul/foo/2/mar/noun/hoon")
        ex-bump
    ==
  =/  bump-2=bump-data  (extract-bump (snag 2 mov))
  ?:  =(bump bump-2)
    (fail:m 'agent(s) not rebuilt/changed when expected' ~)
  (pure:m ~)
--
