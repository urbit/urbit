/+  *test
/=  clay-raw  /sys/vane/clay
/*  lib-def   %hoon  /lib/default-agent/hoon
/*  lib-skel  %hoon  /lib/skeleton/hoon
/*  mar-noun  %hoon  /mar/noun/hoon
/*  mar-hoon  %hoon  /mar/hoon/hoon
/*  mar-txt   %hoon  /mar/txt/hoon
/*  mar-kel   %hoon  /mar/kelvin/hoon
/*  mar-bill  %hoon  /mar/bill/hoon
/*  zus       %hoon  /sys/zuse/hoon
/*  lull      %hoon  /sys/lull/hoon
/*  clay-src  %hoon  /sys/vane/clay/hoon
::
!:
=/  clay-gate  (clay-raw ~nul)
::
|%
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
  ++  ouptut  (output-raw a)
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
  ::      which is important because we build lull, zuse and clay all the time
  =;  comp  (need (~(mole vi |) comp))
  |.  ^-  tang
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
++  set-kelvin  ::  load new clay core at kelvin
  |=  kel=@ud
  ~>  %memo./test/build
  =/  nex=vase
    =/  lul  (slub !>(..part) (ream lull))
    =/  zus  (slub lul (ream (zuse-upd kel)))
    (slub zus (ream clay-src))
  =/  m  (mare ,~)
  ~&  'building files, stand by'
  ;<  ~  bind:m
    |=  =state
    :: apply update
    =/  old-ruf
      =<  stay
      %:  gate.state
        now=now.state
        eny=`@uvJ`0xdead.beef
        scry=scry-gate
      ==
    =.  gate.state
      %.  old-ruf
      =<  load
      ::TODO  gross! can we do better?
      %:  !<(_gate.state [-:!>(gate.state) q:(slam nex !>(~nul))])
        now=now.state
        eny=`@uvJ`0xdead.beef
        scry=scry-gate
      ==
    &+`state
  (pure:m ~)
::
++  get-now
  =/  m  (mare ,@da)
  ^-  form:m
  |=  =state
  [%& now.state state]
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
++  do-wick  (take /wick ~[/blah] [%behn %wake ~])
++  do-pork  (call ~[/blah] [%pork ~])
::
++  do-park
  |=  [=desk kel=@ud fil=(list [path (each page:clay lobe:clay)])]
  %+  call  ~[/blah]
  ^-  (hobo task:clay-gate)
  =/  files
    %-  ~(gas by *(map path (each page:clay lobe:clay)))
    ^-  (list [path (each page:clay lobe:clay)])
    ;:  welp
      fil
      ?:  =(%base desk)
        [/sys/zuse/hoon [%& ;;(page:clay hoon+(zuse-upd kel))]]~
      :~
        [/app/bar/hoon [%& agent]]
        [/lib/skeleton/hoon [%& ;;(page:clay hoon+lib-skel)]]
        [/lib/default-agent/hoon [%& ;;(page:clay hoon+lib-def)]]
        [/mar/bill/hoon [%& ;;(page:clay hoon+mar-bill)]]
        [/desk/bill [%& ;;(page:clay noun+:~(%bar))]]
      ==
    :~
      [/mar/noun/hoon [%& ;;(page:clay hoon+mar-noun)]]
      [/mar/hoon/hoon [%& ;;(page:clay hoon+mar-hoon)]]
      [/mar/txt/hoon [%& ;;(page:clay hoon+mar-txt)]]
      [/mar/kelvin/hoon [%& ;;(page:clay hoon+mar-kel)]]
      [/sys/kelvin [%& ;;(page:clay kelvin+[%zuse kel])]]
    ==
  ==
  =/  =yoki:clay  [%& [*(list tako:clay) files]]
  [%park desk yoki *rang:clay]
::
++  do-new-desk
  |=  =desk
  (do-park desk 409 ~)
::
++  do-setup-desks
  |=  desks=(list [=desk esse=?])
  =/  m  (mare ,~)
  ::  set up the base desk unconditionally
  ::
  ;<  *                bind:m  (do-new-desk %base)
  ;<  *                bind:m  do-pork
  ;<  mov=(list move)  bind:m  (call ~[/blah] [%zest %base `zest:clay`%live])
  ;<  ~                bind:m  (expect-moves mov ex-wick ex-load ~)
  ;<  *                bind:m  do-wick  ::  just a formality
  ::  set up any other desks we want
  ::
  |-  =*  loop  $
  ?~  desks  
    ::  TODO: check mov3 and mov4
    ;<  mov3=(list move)  bind:m  (call ~[/blah] [%tire `~])
    ~&  mov3/mov3
    ;<  mov4=(list move)  bind:m  (call ~[/blah] [%ward ~])
    (pure:m ~)
  =,  i.desks
  ;<  *                bind:m  (do-new-desk desk)
  ;<  *                bind:m
    ?.  esse  (pure:m ~)
    (call ~[/blah] [%esse desk %.y])
  ;<  mov=(list move)  bind:m  (call ~[/blah] [%zest desk `zest:clay`%live])
  ;<  ~                bind:m
    %+  expect-moves  mov
    :~  ex-wick
        ex-load
    ==
  ;<  *                bind:m  do-wick  ::  just a formality
  $(desks t.desks)
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
++  ex-ward-have
|=  [=desk peg=(set perm:gall)]
(ex-gift [%ward [%have desk peg=peg peq=perm-none]])
::
++  ex-ward-need
|=  [=desk per=(set perm:gall)]
(ex-gift [%ward [%need desk per=per]])
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
++  ex-zeal-held
  |=  =desk
  (ex ~[/blah] %pass [%perm %zeal [desk] ~] %c [%zeal [[desk %held] ~]])
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
++  ex-what
  |=  mov=move
  ?:  ?=([* %pass * [%$ [%what *]]] mov)  ~
  :~  'expected %pass %$ %what'
      %^  rap  3  'got      %'
      ?.  ?=(%pass -.q.mov)  [-.q.mov ~]
      [- ' %' +< ~]:q.q.mov
  ==
::
++  ex-kernel-build
  |=  liz=(list [=desk =zest:clay])
  ^-  (list $-(move tang))
  :~  (ex-pass /kiln/bump/zeal [%c %zeal liz])
      ex-what
      (ex [~[/blah] %slip %c %pork ~])
  ==
::
++  ex-resume-commit-missing-perm
  |=  [v=@ud susp=(list desk) perm=(list [desk peg=(set perm:gall) per=(set perm:gall)])]
  ^-  (list $-(move tang))
  ;:  welp
    ::  %base
    :~  ex-wick
        (ex-text ": /~nul/base/{<v>}/sys/zuse/hoon")
        (ex-text ": /~nul/base/{<v>}/sys/kelvin")
    ==
    ::  suspend desk on perm check
    %+  turn  susp
    |=(=desk (ex-zeal-held desk))
    ::
    ::  ward gift
    %-  zing
    %+  turn  (snoc perm [%base ~ ~])
    |=  [=desk peg=(set perm:gall) per=(set perm:gall)]
    %+  welp
      :~  (ex-ward-have desk peg)
      ==
    ?~  per  ~
    :~  (ex-ward-need desk per)
    ==
    :~  ex-load
    ==
  ==
::
++  expect-resume-commit-missing-kel
  |=  [v=@ud kel=@ud deku=(list [desk per=?]) deks=(list desk)]
  ^-  (list $-(move tang))
  ;:  welp
    ::  %base
    :~  ex-wick
        (ex-text ": /~nul/base/{<v>}/sys/zuse/hoon")
        (ex-text ": /~nul/base/{<v>}/sys/kelvin")
    ==
    ::  update per desk w/w-o perm seal file
    ^-  (list $-(move tang))
    %-  zing
    :: ^-  (list (list $-(move tang)))
    %+  turn  deku
    |=  [=desk per=?]
    ^-  (list $-(move tang))
    ?:  ?=(%base desk)  ~
    %+  welp
      :~  ex-wick
          (ex-text ": /~nul/{(scow %tas desk)}/{<v>}/sys/kelvin")
      ==
    ?.  per  ~
    :~  (ex-text "+ /~nul/{(scow %tas desk)}/{<v>}/desk/seal")
    ==
    ::  tire gift per desk
    %+  turn  deku
    |=([=desk *] (ex-gift [%tire %| [%warp desk [%zuse kel]]]))
    ::  suspend desk on perm check
    (turn deks |=(=desk (ex-zeal-held desk)))
    ::  todo: ward-gift
    ::
    :~  ex-load
    ==
  ==
::
::  data constuctors
::
++  perm-none  *(set perm:gall)
++  desk-seal
  ^-  (list [path (each page:clay lobe:clay)])
  [/desk/seal [%& ;;(page:clay seal+[%0 :~([%behn ~])])]]~
::
++  zuse-upd
  |=  kel=@ud
  ^-  @t
  =/  old-zuse  (trip zus)
  =/  i  (find "++  zuse" old-zuse)
  ?~  i  !!
  =/  next-zuse  "++  zuse  `@`%{(scow %ud kel)}"
    %-  crip
    ;:  welp
      (scag u.i old-zuse)
      next-zuse
      (slag (add u.i (lent next-zuse)) old-zuse)
    ==
::
++  agent
  ^-  page:clay
  :-  %hoon
  '''
  /+  default-agent
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
++  test-blocked-on-kelvin
::  non-essential desk ready before base desk commit
::  kelvin update applied on base desk and on non-essential desk
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                  bind:m  (do-setup-desks [%foo |] ~)
  ::  send next kelvin update to a desk
  ;<  mov=(list move)    bind:m  (do-park %foo 408 ~)
  ::TODO  should we test for wick here?
  ;<  mov2=(list move)   bind:m  do-wick
  ::  send next kelvin update to %base
  ;<  mov3=(list move)   bind:m  (do-park %base 408 ~)
  ::  applying zuse update to clay
  ;<  ~                  bind:m  (set-kelvin 408)
  ;<  mov4=(list move)   bind:m  do-pork
  ::
  ;<  ~  bind:m
    %+  expect-moves  mov4
    (expect-resume-commit-missing-kel 2 408 [[%foo |] ~] ~)
  ;<  mov6=(list move)  bind:m  do-wick
  (expect-moves mov6 ~)
::
++  test-blocked-on-kelvin-1
::  non-essential desk ready for kelvin and kelvin-1 update, before base desk
::  kelvin update skipped in favour of kelvin-1, 
::  kelvin-1 update applied on base desk and on non-essential desk
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ::  send kelvin-1 update to a desk
  ;<  mov2=(list move)  bind:m  (do-park %foo 407 ~)
  ;<  *                 bind:m  do-wick
  ;<  mov3=(list move)  bind:m  (do-park %base 407 ~)
  ::  applying kelvin-1 update to clay
  ;<  ~                 bind:m  (set-kelvin 407)
  ;<  mov5=(list move)  bind:m  do-pork
  ::
  ;<  ~  bind:m
    %+  expect-moves  mov5
    (expect-resume-commit-missing-kel 2 407 [[%foo |] ~] ~)
  ;<  mov6=(list move)  bind:m  do-wick
  (expect-moves mov6 ~)
::
++  test-blocked-on-kelvin-and-kelvin-1
::  non-essential desk ready for kelvin and kelvin-1 update, before base desk
::  kelvin update applied on base desk and non-essential desk
::  kelvin-1 update applied on base desk and non-essential desk
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ::  send kelvin update to a desk
  ;<  mov=(list move)   bind:m  (do-park %foo 408 ~)
  ;<  *                 bind:m  do-wick
  ::  send kelvin-1 update to a desk
  ;<  mov2=(list move)  bind:m  (do-park %foo 407 ~)
  ;<  *                 bind:m  do-wick
  ::  apply kelvin-1 update to base
  ;<  mov3=(list move)  bind:m  (do-park %base 407 ~)
  ;<  ~                 bind:m  (set-kelvin 407)
  ;<  mov4=(list move)  bind:m  do-pork
  ::
  ;<  ~  bind:m
    %+  expect-moves  mov4
    :~  ex-wick
        (ex-text ": /~nul/base/2/sys/zuse/hoon")
        (ex-text ": /~nul/base/2/sys/kelvin")
        ex-wick
        (ex-text ": /~nul/foo/2/sys/kelvin")
        (ex-gift [%tire %| [%warp %foo [%zuse 407]]])
        (ex-gift [%tire %| [%warp %foo [%zuse 408]]])
        ex-load
    ==
  ;<  mov5=(list move)  bind:m  do-wick
  (expect-moves mov5 ~)
::
++  test-blocked-on-essential-desk-kelvin-update
::  kelvin update recieved on base desk, but essential desk not ready
::  no-op, notify
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~                 bind:m  (do-setup-desks [%foo &] ~)
  ::  update base to next kelvin
  ;<  mov2=(list move)  bind:m  (do-park %base 408 ~)
  ::
  ::  NOTE:  %zest despite unchanged could be a walk tire bug
  %+  expect-moves  mov2
  :~  (ex-ward-have %foo perm-none)
      (ex-ward-have %base perm-none)
      (ex-gift [%tire %| [%zest %foo %live]])
      (ex-gift [%tire %| [%zest %base %live]])
      (ex-gift [%tire %| [%wait %base [%zuse 408]]])
  ==
::
++  test-apply-kel-suspend-foo
::  kelvin update recieved on base desk, but non-essential desk not ready
::  suspend non-essential desks, apply base update
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                bind:m  (do-setup-desks [%foo |] ~)
  ;<  mov=(list move)  bind:m  (do-park %base 408 ~)
  ;<  ~  bind:m
    %+  expect-moves  mov
    (ex-kernel-build [%foo %held]~)
  ;<  ~                 bind:m  (set-kelvin 408)
  ;<  mov2=(list move)  bind:m  (call ~[/blah] [%zeal [%foo %held]~])
  ;<  mov3=(list move)  bind:m  do-pork
  ::
  ;<  ~  bind:m
    %+  expect-moves  mov3
    (expect-resume-commit-missing-kel 2 408 ~ ~)
  ;<  mov4=(list move)  bind:m  do-wick
  (expect-moves mov4 ~)
::
++  test-missing-perm-on-commit
::  non-essential desk recieve commit, with updated required perms,
::  commit suspended, recieves perms, commit applied and notify on perms update
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ;<  mov=(list move)   bind:m  (do-park %foo 409 desk-seal)
  ;<  mov2=(list move)  bind:m  (call ~[/blah] [%seal %foo & (silt [%behn ~]~)])
  ::
  %+  expect-moves  mov2
  :~  ex-wick
      (ex-text "+ /~nul/foo/2/desk/seal")
      (ex-gift [%tire %| [%zest %foo %live]])
      (ex-gift [%tire %| [%zest %base %live]])
      ex-load
  ==
::
++  test-missing-perm-on-kel-update
::  kelvin update recieved on base desk, non-essential desk ready on kelvin, blocked on perms
::  non-essential desk suspended, kelvin update applied on base
::  non-essential desk recieves required perms
::  non-essential desk commit applied and revived
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ;<  mov=(list move)   bind:m  (do-park %foo 408 desk-seal)
  ;<  mov2=(list move)  bind:m  (do-park %base 408 ~)
  ;<  ~  bind:m
    %+  expect-moves  mov2
    (ex-kernel-build ~)
  ;<  ~                 bind:m  (set-kelvin 408)
  ;<  mov3=(list move)  bind:m  do-pork
  ;<  ~                 bind:m 
    %+  expect-moves  mov3
    (ex-resume-commit-missing-perm 2 [%foo ~] [[%foo perm-none (silt :~([%behn ~]))] ~])
  ;<  mov4=(list move)  bind:m  (call ~[/blah] [%zeal [%foo %held]~])
  ;<  mov5=(list move)  bind:m  (call ~[/blah] [%seal %foo & (silt [%behn ~]~)])
  ::
  ;<  now=@da           bind:m  get-now
  ;<  ~  bind:m
    %+  expect-moves  mov5
    :~  ex-wick
        (ex-text ": /~nul/foo/2/sys/kelvin")
        (ex-text "+ /~nul/foo/2/desk/seal")
        (ex-pass /park-held/foo [%b [%wait now]])
        (ex-gift [%tire %| [%warp %foo [%zuse 408]]])
        ex-load
    ==
  ;<  mov6=(list move)  bind:m  do-wick
  ;<  ~  bind:m  (expect-moves mov6 ~)
  ;<  mov7=(list move)  bind:m  (take /park-held/foo ~[/blah] [%behn %wake ~])
  %+  expect-moves  mov7 
  :~  (ex-gift [%tire %| [%zest %foo %live]]) 
      ex-load
  ==
::
++  test-missing-perm-on-kel-update-2
::  kelvin update recieved on base desk, non-essential desk ready on kelvin and on perms
::  kelvin update applied on base and non-essential desk
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ;<  mov=(list move)   bind:m  (do-park %foo 408 desk-seal)
  ;<  mov2=(list move)  bind:m  (call ~[/blah] [%seal %foo & (silt [%behn ~]~)])
  ;<  ~  bind:m  (expect-moves mov2 (ex-ward-have %foo (silt [%behn ~]~)) (ex-ward-have %base perm-none) ex-load ~)
  ;<  mov3=(list move)  bind:m  (do-park %base 408 ~)
  ;<  ~  bind:m
    %+  expect-moves  mov3
    (ex-kernel-build ~)
  ;<  ~  bind:m  (set-kelvin 408)
  ;<  mov4=(list move)  bind:m  do-pork
  %+  expect-moves  mov4
  (expect-resume-commit-missing-kel 2 408 [[%foo &] ~] ~)
::
++  foo-apply-kel2
::  non-essesntial desk, blocked on kelvin and kelvin-1 and perms
::
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ;<  mov=(list move)   bind:m  (do-park %foo 408 desk-seal)
  ;<  mov2=(list move)  bind:m  (do-park %foo 407 [/desk/seal [%& ;;(page:clay seal+[%0 :~([%behn ~] [%eyre ~])])]]~)
  %+  expect-moves  mov2 
  :~  ex-wick 
      (ex-ward-have %foo perm-none)
      (ex-ward-have %base perm-none)
      (ex-gift [%tire %| [%wait %foo [%zuse 407]]])
  ==
::
++  test-apply-kelvin2-and-perms2
::  non-essential desk ready for kelvin, blocked on perms and ready on kelvin-1 update, blocked on perms
::  kelvin update skipped in favour of kelvin-1
::  suspend non-essential desks, kelvin-1 update applied on base desk
::  non-essential desk recieves required perms
::  non-essential desk update to kelvin-1 applied and revived
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~                 bind:m  foo-apply-kel2
  ;<  mov=(list move)   bind:m  (do-park %base 407 ~)
  ;<  ~  bind:m
    %+  expect-moves  mov
    (ex-kernel-build ~)
  ;<  ~                 bind:m  (set-kelvin 407)
  ;<  mov2=(list move)  bind:m  do-pork
  =/  perms  (silt `(list perm:gall)`:~([%eyre ~] [%behn ~]))
  ;<  ~  bind:m
    %+  expect-moves  mov2
    (ex-resume-commit-missing-perm 2 [%foo ~] [[%foo perm-none perms] ~])
  ;<  mov3=(list move)  bind:m  (call ~[/blah] [%zeal [%foo %held]~])
  ;<  mov4=(list move)  bind:m  (call ~[/blah] [%seal %foo & perms])
  ;<  now=@da           bind:m  get-now
  ;<  ~  bind:m
    %+  expect-moves  mov4
    :~  ex-wick
        (ex-text ": /~nul/foo/2/sys/kelvin")
        (ex-text "+ /~nul/foo/2/desk/seal")
        (ex-pass /park-held/foo [%b [%wait now]])
        (ex-gift [%tire %| [%warp %foo zuse+407]])
        (ex-gift [%tire %| [%warp %foo zuse+408]])
        ex-load
    ==
  ;<  mov5=(list move)  bind:m  do-wick
  ;<  ~                 bind:m  (expect-moves mov5 ~)
  ;<  mov6=(list move)  bind:m  (take /park-held/foo ~[/blah] [%behn %wake ~])
  %+  expect-moves  mov6 
  :~  (ex-gift [%tire %| [%zest %foo %live]])
      ex-load
  ==
::
++  test-apply-kelvin-1-and-perms2
::  non-essential desk ready for kelvin, blocked on perms and ready on kelvin-1 update, blocked on perms
::  suspend non-essential desks, kelvin update applied on base desk
::  non-essential desk recieves required perms
::  non-essential desk kelvin update applied and revived
::  suspend non-essential desks, kelvin-1 update applied on base desk
::  non-essential desk recieves required perms
::  non-essential desk kelvin-1 update applied and revived
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~                 bind:m  foo-apply-kel2
  ;<  mov=(list move)   bind:m  (do-park %base 408 ~)
  ;<  ~  bind:m
    %+  expect-moves  mov
    (ex-kernel-build ~)
  ;<  ~                 bind:m  (set-kelvin 408)
  ;<  mov2=(list move)  bind:m  do-pork
  ;<  ~                 bind:m
    %+  expect-moves  mov2
    (ex-resume-commit-missing-perm 2 [%foo ~] [[%foo perm-none (silt :~([%behn ~]))] ~])
  ;<  mov3=(list move)  bind:m  (call ~[/blah] [%zeal [%foo %held]~])
  ;<  mov4=(list move)  bind:m  (call ~[/blah] [%seal %foo & (silt :~([%behn ~]))])
  ;<  now=@da           bind:m  get-now
  ;<  ~  bind:m  
    %+  expect-moves  mov4
    :~  ex-wick
        (ex-text ": /~nul/foo/2/sys/kelvin")
        (ex-text "+ /~nul/foo/2/desk/seal")
        (ex-pass /park-held/foo [%b [%wait now]])
        (ex-gift [%tire %| [%warp %foo [%zuse 408]]])
        ex-load
    ==
  ;<  *                 bind:m  do-wick
  ;<  mov5=(list move)  bind:m  (take /park-held/foo ~[/blah] [%behn %wake ~])
  ;<  ~  bind:m
    %+  expect-moves  mov5
    :~  (ex-gift [%tire %| [%zest %foo %live]])
        ex-load
    ==
  ;<  mov6=(list move)   bind:m  (do-park %base 407 ~)
  ;<  ~  bind:m
    %+  expect-moves  mov6
    (ex-kernel-build ~)
  ;<  ~                 bind:m  (set-kelvin 407)
  ;<  mov7=(list move)  bind:m  do-pork
  ;<  now=@da           bind:m  get-now
  ;<  ~  bind:m
    %+  expect-moves  mov7
    (ex-resume-commit-missing-perm 3 [%foo ~] [[%foo (silt :~([%behn ~])) (silt :~([%eyre ~]))] ~])
  ;<  *                 bind:m  do-wick
  ;<  mov8=(list move)  bind:m  (call ~[/blah] [%zeal [%foo %held]~])
  ;<  mov9=(list move)  bind:m  (call ~[/blah] [%seal %foo & (silt :~([%eyre ~]))])
  ;<  ~  bind:m
    %+  expect-moves  mov9
    :~  ex-wick
        (ex-text ": /~nul/foo/3/desk/seal")
        (ex-text ": /~nul/foo/3/sys/kelvin")
        (ex-pass /park-held/foo [%b [%wait now]])
        (ex-gift [%tire %| [%warp %foo [%zuse 407]]])
        ex-load
    ==
  ;<  *                  bind:m  do-wick
  ;<  mov10=(list move)  bind:m  (take /park-held/foo ~[/blah] [%behn %wake ~])
  %+  expect-moves  mov10 
  :~  (ex-gift [%tire %| [%zest %foo %live]])
      ex-load
  ==
--