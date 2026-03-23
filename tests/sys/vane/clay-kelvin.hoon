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
      ?:  =(%base desk)
        [/sys/zuse/hoon [%& ;;(page:clay hoon+(zuse-upd kel))]]~
      ?:  =(%foo desk)
        :~  [/app/bar/hoon [%& agent]]
            [/desk/bill [%& ;;(page:clay noun+:~(%bar))]]
            [/lib/skeleton/hoon [%& ;;(page:clay hoon+lib-skel)]]
            [/lib/default-agent/hoon [%& ;;(page:clay hoon+lib-def)]]
            [/mar/bill/hoon [%& ;;(page:clay hoon+mar-bill)]]
        ==
      ~
      :~
        [/mar/noun/hoon [%& ;;(page:clay hoon+mar-noun)]]
        [/mar/hoon/hoon [%& ;;(page:clay hoon+mar-hoon)]]
        [/mar/txt/hoon [%& ;;(page:clay hoon+mar-txt)]]
        [/mar/kelvin/hoon [%& ;;(page:clay hoon+mar-kel)]]
        [/sys/kelvin [%& ;;(page:clay kelvin+[%zuse kel])]]
      ==
      fil
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
  =/  dek  desks
  |-  =*  loop  $
  ?~  desks
    ;<  mov3=(list move)  bind:m  (call ~[/blah] [%tire `~])
    =/  =rock:tire:clay
      %-  malt
      %+  roll  `(list [desk ?])`(welp [%base %.y]~ dek)
      |=  [[=desk @] l=(list [desk [zest:clay (set weft)]])]
      [[desk [%live *(set weft)]] l]
    ;<  ~                 bind:m
      (expect-moves mov3 (ex-gift [%tire %.y rock]) ~)
    ;<  mov4=(list move)  bind:m  (call ~[/blah] [%ward ~])
    ;<  ~                 bind:m  (expect-moves mov4 ~)
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
|=  [=desk ped=(set perm:gall) peg=(set perm:gall)]
(ex-gift [%ward [%have desk ped=ped peg=peg peq=perm-none]])
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
  |=  [suspend=(list [=desk =zest:clay]) perm=(list [desk ese=? ped=(set perm:gall) peg=(set perm:gall) per=(set perm:gall)])]
  ^-  (list $-(move tang))
  ::  ward gift moves on awaiting required permission update
  ::  flag indicates essential desks readines for %base update
  ::
  =/  [movl=(list (list $-(move tang))) ese-ready=?]
    %^  spin  perm  &
    |=  [[=desk ese=? ped=(set perm:gall) peg=(set perm:gall) per=(set perm:gall)] ese-ready=?]
    ::  checking required perms
    =/  ward-have
      ?:(&(=(~ ped) =(~ peg)) ~ [(ex-ward-have desk ped peg) ~])
    ?~  per  [ward-have ese-ready]
    ?:  ese
      [(snoc ward-have (ex-ward-need desk per)) %.n]
    [(snoc ward-have (ex-ward-need desk per)) ese-ready]
  ::
  ::  list of desks to suspend and
  ::  ward gifts on awaiting required permissions for non-esential desks
  ::
  =/  [nese-zeal=(list [desk zest:clay]) nese-ward=(list $-(move tang))]
    %+  roll  perm
    |=  [[=desk ese=? ped=(set perm:gall) peg=(set perm:gall) per=(set perm:gall)] [nese-zeal=(list [desk zest:clay]) nese-ward=(list $-(move tang))]]
    ?:  ese  [nese-zeal nese-ward]
    =/  ward-have
      ?:(&(=(~ ped) =(~ peg)) ~ [(ex-ward-have desk ped peg) ~])
    =/  ward-mov=(list $-(move tang))
      ?~  per
        ward-have
      (snoc ward-have (ex-ward-need desk per))
    [?~(per nese-zeal [[desk %held] nese-zeal]) (welp ward-mov nese-ward)]
  ::
  ?.  ese-ready
    ::  essential not ready on perms
    ::
    %+  welp  `(list $-(move tang))`(zing movl)
    [(ex-gift [%tire %| [%wait %base [%zuse 408]]]) ~]
  ::  esential desks ready to apply base update,
  ::  suspending desks without commit or with insufficient permissions
  ::
  %+  welp  nese-ward
  :~  %+  ex-pass  /kiln/bump/zeal
      :+  %c  %zeal
      (welp suspend nese-zeal)
      ::
      ex-what
      (ex [~[/blah] %slip %c %pork ~])
  ==
::
++  ex-resume-commit
  |=  [v=@ud kel=@ud deku=(list [=desk ese=? ped=(set perm:gall) peg=(set perm:gall)])]
  ^-  (list $-(move tang))
  ;:  welp
    ::  %base
    :~  ex-wick
        (ex-text ": /~nul/base/{<v>}/sys/zuse/hoon")
        (ex-text ": /~nul/base/{<v>}/sys/kelvin")
    ==
    ::
    =/  one-ese  =(1 (lent (skim deku |=([@ ese=? *] ese))))
    ::  update per desk w/w-o perm seal file
    ::
    =/  l  *(list $-(move tang))
    |-  ^-  (list $-(move tang))
    ?~  deku  l
    =,  i.deku
    ::  if essential and none of the remaining desks on the list are esential
    =/  is-last-ese=?  |(&(ese (levy t.deku |=([@ e=? *] !e))) !ese)
    %=  $
      l
        ;:  welp
          l
          :~  ex-wick
              (ex-text ": /~nul/{(scow %tas desk)}/{<v>}/sys/kelvin")
          ==
          ?~  ped  ~
          [(ex-text "+ /~nul/{(scow %tas desk)}/{<v>}/desk/seal") ~]
          ?~(peg ~ [(ex-ward-have [desk ped peg]) ~])        ::  ward gift per desk
          [(ex-gift [%tire %| [%warp desk [%zuse kel]]]) ~]  ::  tire gift per desk
          ::  if desk updated is essential ex-gift tire for %base,
          ::  unless %base is ready to proceed with update
          ?.  one-ese
            ?:  is-last-ese  ~
            [(ex-gift [%tire %| [%warp %base [%zuse kel]]]) ~]
          [(ex-gift [%tire %| [%warp %base [%zuse kel]]]) ~]
        ==
      deku  t.deku
    ==
    ::
    :~  ex-load
    ==
  ==
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
::  data constuctors
::
++  perm-none  *(set perm:gall)
++  perl-1  (perl-n 1)
++  perl-2  (perl-n 2)
++  pers-1  (pers-n 1)
++  pers-2  (pers-n 2)
++  perl-n  (curr scag def-perms)
++  pers-n  (cork (curr scag def-perms) sy)
++  def-perms
  ^-  (list perm:gall)
  :~  [%behn ~]
      [%eyre ~]
  ==
++  desk-seal
  |=  pern=@ud
  ^-  (list [path (each page:clay lobe:clay)])
  [/desk/seal [%& ;;(page:clay seal+[%0 (perl-n pern)])]]~
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
++  test-apply-kel-update
::  non-essential desk ready before base desk commit
::  kelvin update applied on base desk and on non-essential desk
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                  bind:m  (do-setup-desks [%foo |] ~)
  ::  send next kelvin update to a desk
  ;<  mov=(list move)    bind:m  (do-park %foo 408 ~)
  ;<  ~  bind:m
    %+  expect-moves  mov
    :~  ex-wick
        (ex-gift [%tire %| [%wait %foo [%zuse 408]]])
    ==
  ;<  mov2=(list move)   bind:m  do-wick
  ;<  ~  bind:m  (expect-moves mov2 ~)
  ::  send next kelvin update to %base
  ;<  mov3=(list move)   bind:m  (do-park %base 408 ~)
  ;<  ~  bind:m
    (expect-moves mov3 (ex-kernel-build ~ [[%foo | ~ ~ ~] ~]))
  ::  applying zuse update to clay
  ;<  ~                  bind:m  (set-kelvin 408)
  ;<  mov4=(list move)   bind:m  do-pork
  ::
  ;<  ~  bind:m
    %+  expect-moves  mov4
    (ex-resume-commit 2 408 [[%foo | perm-none perm-none] ~])
  ;<  mov6=(list move)  bind:m  do-wick
  (expect-moves mov6 ~)
::
++  test-skip-kelvin
::  non-essential desk ready for kelvin and kelvin-1 update, before base desk
::  kelvin update skipped in favour of kelvin-1,
::  kelvin-1 update applied on base desk and on non-essential desk
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ::  send kelvin-1 update to a desk
  ;<  mov2=(list move)  bind:m  (do-park %foo 407 ~)
  ;<  ~  bind:m
    %+  expect-moves  mov2
    :~  ex-wick
        (ex-gift [%tire %| [%wait %foo [%zuse 407]]])
    ==
  ;<  *                 bind:m  do-wick
  ;<  mov3=(list move)  bind:m  (do-park %base 407 ~)
  ;<  ~  bind:m
    (expect-moves mov3 (ex-kernel-build ~ [[%foo | ~ ~ ~] ~]))
  ::  applying kelvin-1 update to clay
  ;<  ~                 bind:m  (set-kelvin 407)
  ;<  mov4=(list move)  bind:m  do-pork
  ::
  ;<  ~  bind:m
    %+  expect-moves  mov4
    (ex-resume-commit 2 407 [[%foo | perm-none perm-none] ~])
  ;<  mov5=(list move)  bind:m  do-wick
  (expect-moves mov5 ~)
::
++  test-skip-kelvin-with-non-esse
::  non-essential desk ready for kelvin and kelvin-1 update, before base desk
::  kelvin-1 update applied on base
::  kelvin update applied on non-essential desk
::  kelvin-1 update applied on non-essential desk
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
++  test-update-blocked-on-essential-desk
::  kelvin update received on base desk, but essential desk not ready
::  no-op, notify
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo &] ~)
  ::  update base to next kelvin
  ;<  mov2=(list move)  bind:m  (do-park %base 408 ~)
  ::
  (expect-moves mov2 (ex-gift [%tire %| [%wait %base [%zuse 408]]]) ~)
::
++  test-update-blocked-on-multiple-essential-desks
::  kelvin update received on base desk, multiple essential desks not ready
::  no-op, notify
::  update received on one essential desk, waiting on others
::  update received on all essential desks,
::  update applied on base and essential desks
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo &] [%baz &] ~)
  ::
  ;<  mov=(list move)  bind:m  (do-park %base 408 ~)
  ::
  ::  NOTE:  %zest despite unchanged could be a walk tire bug
  ;<  ~                 bind:m
    %+  expect-moves  mov
    :~  (ex-gift [%tire %| [%zest %foo %live]])
        (ex-gift [%tire %| [%zest %baz %live]])
        (ex-gift [%tire %| [%zest %base %live]])
        (ex-gift [%tire %| [%wait %base [%zuse 408]]])
    ==
  ::
  ;<  mov2=(list move)  bind:m  (do-park %foo 408 ~)
  ;<  ~                 bind:m
    %+  expect-moves  mov2
    :~  ex-wick
        (ex-gift [%tire %| [%wait %foo [%zuse 408]]])
    ==
  ;<  mov3=(list move)  bind:m  do-wick
  ;<  ~                 bind:m
    (expect-moves mov3 ~)
  ;<  mov4=(list move)  bind:m  (do-park %baz 408 ~)
  ;<  ~                 bind:m
    %+  expect-moves  mov4
    :~  ex-wick
        (ex-gift [%tire %| [%wait %baz [%zuse 408]]])
    ==
  ;<  mov5=(list move)  bind:m  do-wick
  ;<  ~                 bind:m
    %+  expect-moves  mov5
    (ex-kernel-build ~ ~)
  ;<  ~                 bind:m  (set-kelvin 408)
  ;<  mov6=(list move)  bind:m  do-pork
  ;<  ~                 bind:m
    %+  expect-moves  mov6
    (ex-resume-commit 2 408 [%foo & perm-none perm-none] [%baz & perm-none perm-none]~)
  ;<  mov6=(list move)  bind:m  do-wick
  (expect-moves mov6 ~)
::
++  test-update-blocked-by-perms-on-essential-desk
::
::  NOTE: maybe remove in favour of +test-update-blocked-on-desks
::
::  kelvin update received on base desk,
::  essential desk recieved update, but blocked on perm
::  no-op, send ward
::  essential desk permissions granted, apply update
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                bind:m  (do-setup-desks [%foo &] ~)
  ;<  mov=(list move)  bind:m  (do-park %foo 408 (desk-seal 1))
  ;<  ~  bind:m
    %+  expect-moves  mov
    :~  ex-wick
        (ex-gift [%tire %| [%wait %foo [%zuse 408]]])
    ==
  ;<  mov2=(list move)  bind:m  (do-park %base 408 ~)
  ;<  ~  bind:m
    %+  expect-moves  mov2
    (ex-kernel-build ~ [[%foo & perm-none perm-none pers-1] ~])
  ;<  mov3=(list move)  bind:m  (call ~[/blah] [%seal %foo & pers-1])
  ;<  ~                 bind:m
    %+  expect-moves  mov3
    :~  ex-wick
        (ex-ward-have %foo perm-none pers-1)
        (ex-ward-need %foo perm-none)
        ex-load
    ==
  ;<  mov4=(list move)  bind:m  do-wick
  ;<  ~                 bind:m  (expect-moves mov4 (ex-kernel-build ~ [%foo & perm-none pers-1 perm-none] ~))
  ;<  ~                 bind:m  (set-kelvin 408)
  ;<  mov5=(list move)  bind:m  do-pork
  ;<  ~                 bind:m
    %+  expect-moves  mov5
    (ex-resume-commit 2 408 [%foo & pers-1 pers-1] ~)
  ;<  mov6=(list move)  bind:m  do-wick
  (expect-moves mov6 ~)
::
++  test-update-blocked-on-desks
::  kelvin update received on base desk,
::  essential desk recieved update, but blocked on perm
::  no-op, send ward
::  essential desk permissions granted
::  non-essential desk not ready,
::  suspend non-essential and apply update
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                bind:m  (do-setup-desks [%foo &] [%baz |] ~)
  ;<  mov=(list move)  bind:m  (do-park %foo 408 (desk-seal 1))
  ;<  ~  bind:m
    %+  expect-moves  mov
    :~  ex-wick
        (ex-gift [%tire %| [%wait %foo [%zuse 408]]])
    ==
  ;<  mov2=(list move)  bind:m  (do-park %base 408 ~)
  ;<  ~  bind:m
    %+  expect-moves  mov2
    (ex-kernel-build ~ [%foo & perm-none perm-none pers-1] ~)
  ;<  mov3=(list move)  bind:m  (call ~[/blah] [%seal %foo & pers-1])
  ;<  ~                 bind:m
    %+  expect-moves  mov3
    :~  ex-wick
        (ex-ward-have %foo perm-none pers-1)
        (ex-ward-need %foo perm-none)
        ex-load
    ==
  ;<  mov4=(list move)  bind:m  do-wick
  ;<  ~                 bind:m
    %+  expect-moves  mov4
    (ex-kernel-build [[%baz %held] ~] [%foo & perm-none pers-1 perm-none] ~)
  ;<  mov5=(list move)  bind:m  (call ~[/blah] [%zeal [%baz %held]~])
  ;<  ~  bind:m
    %+  expect-moves  mov5
    :~  ex-wick
        (ex-gift [%tire %| [%zest %baz %held]])
        ex-load
    ==
  ;<  ~                 bind:m  (set-kelvin 408)
  ;<  mov7=(list move)  bind:m  do-pork
  ;<  ~                 bind:m
    %+  expect-moves  mov7
    (ex-resume-commit 2 408 [%foo & pers-1 pers-1] ~)
  ;<  mov8=(list move)  bind:m  do-wick
  (expect-moves mov8 ~)
::
++  test-apply-update-non-esse-up-to-date
::  non-essential desk received commit compatabile with current version
::  and future version,
::  kelvin update received on base desk, apply update, non-essential desk stays live
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  =/  sys-kel=(list [path (each page:clay lobe:clay)])
    [/sys/kelvin [%& ;;(page:clay kelvin+[[%1 ~] (silt :~([%zuse 409] [%zuse 408]))])]]~
  ;<  mov2=(list move)  bind:m  (do-park %foo 409 sys-kel)
  ;<  ~                 bind:m
    %+  expect-moves  mov2
    :~  ex-wick
        (ex-text ": /~nul/foo/2/sys/kelvin")
        (ex-gift [%tire %| [%wait %foo [%zuse 408]]])
        ex-load
    ==
  ;<  mov3=(list move)  bind:m  (do-park %base 408 ~)
  ;<  ~                 bind:m  (expect-moves mov3 (ex-kernel-build ~ [%foo | ~ ~ ~] ~))
  ;<  ~                 bind:m  (set-kelvin 408)
  ;<  mov4=(list move)  bind:m  do-pork
  %+  expect-moves  mov4
  :~  ex-wick
      (ex-text ": /~nul/base/2/sys/zuse/hoon")
      (ex-text ": /~nul/base/2/sys/kelvin")
      ex-wick
      (ex-gift [%tire %| [%warp %foo [%zuse 408]]])
      ex-load
  ==
::
++  test-apply-update-held-non-essential-desk
::  kelvin update received on base desk, non-essential desk not ready
::  suspend non-essential desks, apply update
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ;<  mov=(list move)   bind:m  (do-park %base 408 ~)
  ;<  ~  bind:m  (expect-moves mov (ex-kernel-build [[%foo %held] ~] ~))
  ;<  mov2=(list move)  bind:m  (call ~[/blah] [%zeal [%foo %held]~])
  ;<  ~                 bind:m
    %+  expect-moves  mov2
    :~  ex-wick
        (ex-gift [%tire %| [%zest %foo %held]])
        ex-load
    ==
  ;<  ~                 bind:m  (set-kelvin 408)
  ;<  mov3=(list move)  bind:m  do-pork
  ;<  ~                 bind:m  (expect-moves mov3 (ex-resume-commit 2 408 ~))
  ::
  %-  branch
  |^  :~  'got-update'^got-update
          'got-update-insufficient-perms'^got-update-insufficient-perms
          'got-next-update'^got-next-update
      ==
  ::
  ++  got-update
  ::  non-essential desk received update, update applied
  ::
    ;<  mov=(list move)  bind:m  (do-park %foo 408 ~)
    ;<  now=@da          bind:m  get-now
    ;<  ~  bind:m
      %+  expect-moves  mov
      :~  ex-wick
          (ex-text ": /~nul/foo/2/sys/kelvin")
          (ex-pass /park-held/foo [%b [%wait now]])
      ==
    ;<  mov1=(list move)  bind:m  do-wick
    ;<  ~  bind:m  (expect-moves mov1 ~)
    ;<  mov2=(list move)  bind:m  (take /park-held/foo ~[/blah] [%behn %wake ~])
    (expect-moves mov2 (ex-gift [%tire %| [%zest %foo %live]]) ex-load ~)
  ::
  ++  got-update-insufficient-perms
  ::  non-essential desk received update, blocked on required perms
  ::  perms granted to non-essential desk, update applied and revived
  ::
    ;<  mov=(list move)  bind:m  (do-park %foo 408 (desk-seal 1))
    ;<  ~                 bind:m
      (expect-moves mov (ex-ward-need %foo pers-1) ~)
    ;<  mov2=(list move)  bind:m  (call ~[/blah] [%seal %foo & pers-1])
    ;<  now=@da           bind:m  get-now
    ;<  ~  bind:m
      %+  expect-moves  mov2
      :~  ex-wick
          (ex-text ": /~nul/foo/2/sys/kelvin")
          (ex-text "+ /~nul/foo/2/desk/seal")
          (ex-ward-have %foo pers-1 pers-1)
          (ex-ward-need %foo perm-none)
          (ex-pass /park-held/foo [%b [%wait now]])
          ex-load
      ==
    ;<  mov3=(list move)  bind:m  do-wick
    ;<  ~  bind:m  (expect-moves mov3 ~)
    ;<  mov4=(list move)  bind:m  (take /park-held/foo ~[/blah] [%behn %wake ~])
    (expect-moves mov4 (ex-gift [%tire %| [%zest %foo %live]]) ex-load ~)
  ::
  ++  got-next-update
  ::  non-essential desk receives update, update doesn't match to current version, stays %held
  ::  re-evaluates on next update
    ;<  mov=(list move)   bind:m  (do-park %foo 407 ~)
    ;<  ~                 bind:m  (expect-moves mov ex-wick (ex-gift [%tire %| [%wait %foo [%zuse 407]]]) ~)
    ;<  mov2=(list move)  bind:m  do-wick
    ;<  ~                 bind:m  (expect-moves mov2 ~)
    ::
    ;<  mov3=(list move)  bind:m  (do-park %base 407 ~)
    ;<  ~                 bind:m
      %+  expect-moves  mov3
      (ex-kernel-build ~ [[%foo | perm-none perm-none perm-none] ~])
    ;<  ~                 bind:m  (set-kelvin 407)
    ;<  mov4=(list move)  bind:m  do-pork
    ::
    ;<  now=@da           bind:m  get-now
    ;<  ~                 bind:m
      %+  expect-moves  mov4
      :~  ex-wick
          (ex-text ": /~nul/base/3/sys/zuse/hoon")
          (ex-text ": /~nul/base/3/sys/kelvin")
          ex-wick
          (ex-text ": /~nul/foo/2/sys/kelvin")
          (ex-pass /park-held/foo [%b [%wait now]])
          (ex-gift [%tire %| [%warp %foo [%zuse 407]]])
          ex-load
      ==
    ;<  mov5=(list move)  bind:m  do-wick
    ;<  ~                 bind:m  (expect-moves mov5 ~)
    ;<  mov6=(list move)  bind:m  (take /park-held/foo ~[/blah] [%behn %wake ~])
    (expect-moves mov6 (ex-gift [%tire %| [%zest %foo %live]]) ex-load ~)
  --
::
++  test-apply-update-non-essential-desk-dead
::  kelvin update received on base desk, non-essential desk set to %dead
::  apply update,
::  trying to set non-essential desk live, fails to set live stays dead
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ;<  mov=(list move)   bind:m  (call ~[/blah] [%zeal [%foo %dead]~])
  ;<  ~                 bind:m
    %+  expect-moves  mov
    :~  ex-wick
        (ex-gift [%tire %| [%zest %foo %dead]])
        ex-load
    ==
  ;<  mov2=(list move)  bind:m  (do-park %base 408 ~)
  ;<  ~                 bind:m  (expect-moves mov2 (ex-kernel-build ~ ~))
  ;<  ~                 bind:m  (set-kelvin 408)
  ;<  mov3=(list move)  bind:m  do-pork
  ;<  ~                 bind:m
    (expect-moves mov3 (ex-resume-commit 2 408 ~))
  ;<  mov4=(list move)   bind:m  (call ~[/blah] [%zeal [%foo %live]~])
  ::  NOTE: should set desk to %held, tried to revive awaiting update
  ;<  ~                 bind:m  (expect-moves mov4 ex-wick ex-load ~)
  ;<  mov5=(list move)  bind:m  do-wick
  (expect-moves mov5 ~)
::
++  test-non-essential-desk-missing-perm-on-kel-update
::  kelvin update received on base desk, non-essential desk ready on kelvin, blocked on perms
::  non-essential desk suspended, kelvin update applied on base
::  non-essential desk receives required perms
::  non-essential desk commit applied and revived
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ;<  mov=(list move)   bind:m  (do-park %foo 408 (desk-seal 1))
  ;<  ~  bind:m
    %+  expect-moves  mov
    :~  ex-wick
        (ex-gift [%tire %| [%wait %foo zuse+408]])
    ==
  ;<  mov2=(list move)  bind:m  (do-park %base 408 ~)
  ;<  ~  bind:m
    %+  expect-moves  mov2
    (ex-kernel-build ~ [[%foo | perm-none perm-none pers-1] ~])
  ;<  *                 bind:m  (call ~[/blah] [%zeal [%foo %held]~])
  ;<  ~                 bind:m  (set-kelvin 408)
  ;<  mov3=(list move)  bind:m  do-pork
  ;<  ~                 bind:m
    %+  expect-moves  mov3
    (ex-resume-commit 2 408 ~)
  ;<  mov5=(list move)  bind:m  (call ~[/blah] [%seal %foo & pers-1])
  ::
  ;<  now=@da           bind:m  get-now
  ;<  ~  bind:m
    %+  expect-moves  mov5
    :~  ex-wick
        (ex-text ": /~nul/foo/2/sys/kelvin")
        (ex-text "+ /~nul/foo/2/desk/seal")
        (ex-ward-have %foo pers-1 pers-1)
        (ex-ward-need %foo perm-none)
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
++  test-apply-kel-update-with-perms
::  kelvin update received on base desk, non-essential desk ready on kelvin and on perms
::  kelvin update applied on base and non-essential desk
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ;<  mov=(list move)   bind:m  (do-park %foo 408 (desk-seal 1))
  ;<  mov2=(list move)  bind:m  (call ~[/blah] [%seal %foo & pers-1])
  ;<  ~  bind:m  (expect-moves mov2 (ex-ward-have %foo perm-none pers-1) ex-load ~)
  ;<  mov3=(list move)  bind:m  (do-park %base 408 ~)
  ;<  ~  bind:m
    %+  expect-moves  mov3
    (ex-kernel-build ~ ~)
  ;<  ~  bind:m  (set-kelvin 408)
  ;<  mov4=(list move)  bind:m  do-pork
  %+  expect-moves  mov4
  (ex-resume-commit 2 408 [[%foo | pers-1 pers-1] ~])
::
++  foo-apply-kel2
::  non-essential desk, blocked on kelvin and kelvin-1 and perms
::
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ;<  mov=(list move)   bind:m  (do-park %foo 408 (desk-seal 1))
  ::  NOTE: shouldn't recieving tire %foo %live
  ;<  ~                 bind:m
    %+  expect-moves  mov
    :~  ex-wick
        (ex-gift [%tire %| [%zest %foo %live]])
        (ex-gift [%tire %| [%wait %foo [%zuse 408]]])
        (ex-gift [%tire %| [%zest %base %live]])
    ==
  ;<  mov2=(list move)  bind:m  (do-park %foo 407 (desk-seal 2))
  %+  expect-moves  mov2
  :~  ex-wick
      (ex-gift [%tire %| [%wait %foo [%zuse 407]]])
  ==
::
++  test-skip-kelvin-and-revive-non-esse
::  non-essential desk ready for kelvin, blocked on perms and ready on kelvin-1 update, blocked on perms
::  kelvin update skipped in favour of kelvin-1
::  suspend non-essential desks, kelvin-1 update applied on base desk
::  non-essential desk receives required perms
::  non-essential desk update to kelvin-1 applied and revived
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~                 bind:m  foo-apply-kel2
  ;<  mov=(list move)   bind:m  (do-park %base 407 ~)
  ;<  ~  bind:m
    %+  expect-moves  mov
    (ex-kernel-build ~ [[%foo | perm-none perm-none pers-2] ~])
  ;<  mov2=(list move)  bind:m  (call ~[/blah] [%zeal [%foo %held]~])
  ;<  ~                 bind:m  (set-kelvin 407)
  ;<  mov3=(list move)  bind:m  do-pork
  ;<  ~  bind:m
    %+  expect-moves  mov3
    (ex-resume-commit 2 407 ~)
  ;<  mov4=(list move)  bind:m  (call ~[/blah] [%seal %foo & pers-2])
  ;<  now=@da           bind:m  get-now
  ;<  ~  bind:m
    %+  expect-moves  mov4
    :~  ex-wick
        (ex-text ": /~nul/foo/2/sys/kelvin")
        (ex-text "+ /~nul/foo/2/desk/seal")
        (ex-ward-have %foo pers-2 pers-2)
        (ex-ward-need %foo perm-none)
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
++  test-apply-updates-revive-non-esse-desk
::  non-essential desk ready for kelvin, blocked on perms and ready on kelvin-1 update, blocked on perms
::  suspend non-essential desks, kelvin update applied on base desk
::  non-essential desk receives required perms
::  non-essential desk kelvin update applied and revived
::  suspend non-essential desks, kelvin-1 update applied on base desk
::  non-essential desk receives required perms
::  non-essential desk kelvin-1 update applied and revived
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~                 bind:m  foo-apply-kel2
  ;<  mov=(list move)   bind:m  (do-park %base 408 ~)
  ;<  ~  bind:m
    %+  expect-moves  mov
    (ex-kernel-build ~ [[%foo | perm-none perm-none pers-1] ~])
  ;<  mov2=(list move)  bind:m  (call ~[/blah] [%zeal [%foo %held]~])
  ;<  ~                 bind:m  (set-kelvin 408)
  ;<  mov3=(list move)  bind:m  do-pork
  ;<  ~                 bind:m
    %+  expect-moves  mov3
    (ex-resume-commit 2 408 ~)
  ;<  mov4=(list move)  bind:m  (call ~[/blah] [%seal %foo & pers-1])
  ;<  now=@da           bind:m  get-now
  ;<  ~  bind:m
    %+  expect-moves  mov4
    :~  ex-wick
        (ex-text ": /~nul/foo/2/sys/kelvin")
        (ex-text "+ /~nul/foo/2/desk/seal")
        (ex-ward-have %foo pers-1 pers-1)
        (ex-ward-need %foo perm-none)
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
    ::  not passing perms here, already got %ward for them on %seal
    (ex-kernel-build ~ [[%foo | ~ ~ (silt :~([%eyre ~]))] ~])
  ;<  mov7=(list move)  bind:m  (call ~[/blah] [%zeal [%foo %held]~])
  ;<  ~                 bind:m  (set-kelvin 407)
  ;<  mov8=(list move)  bind:m  do-pork
  ;<  now=@da           bind:m  get-now
  ;<  ~  bind:m
    %+  expect-moves  mov8
    (ex-resume-commit 3 407 ~)
  ;<  *                 bind:m  do-wick
  ;<  mov9=(list move)  bind:m  (call ~[/blah] [%seal %foo & (silt :~([%eyre ~]))])
  ;<  ~  bind:m
    %+  expect-moves  mov9
    :~  ex-wick
        (ex-text ": /~nul/foo/3/desk/seal")
        (ex-text ": /~nul/foo/3/sys/kelvin")
        (ex-ward-have %foo pers-2 pers-2)
        (ex-ward-need %foo perm-none)
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
::
++  test-downgrade-base
  ::  %base got downgrade commit, prevent applying commit
  ::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks ~)
  ::  send next kelvin update to a desk
  ;<  mov=(list move)   bind:m  (do-park %base 410 ~)
  (expect-moves mov ~)
::
++  test-downgrade-desk
  ::  desk got downgrade commit, prevent applying commit
  ::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ::  send next kelvin update to a desk
  ;<  mov=(list move)   bind:m  (do-park %foo 410 ~)
  ;<  ~                 bind:m  (expect-moves mov ~)
  =/  files
    %-  ~(gas by *(map path (each page:clay lobe:clay)))
    :~  [/mar/noun/hoon [%& hoon+mar-noun]]
        [/mar/hoon/hoon [%& hoon+mar-hoon]]
        [/mar/txt/hoon [%& hoon+mar-txt]]
        [/mar/kelvin/hoon [%& hoon+mar-kel]]
        [/sys/kelvin [%& kelvin+[[%1 ~] (silt :~(zuse+411 zuse+410))]]]
    ==
  ;<  mov2=(list move)  bind:m
    %+  call  ~[/blah]
    [%park %foo `yoki:clay`[%& [*(list tako:clay) files]] *rang:clay]
  (expect-moves mov2 ~)
::
::  desk liveness tests
::
++  test-held-to-dead
::  non-essential desk held, awaiting base update, set to dead
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ;<  *                 bind:m  (do-park %base 408 ~)
  ;<  *                 bind:m  (call ~[/blah] [%zeal [%foo %held]~])
  ;<  ~                 bind:m  (set-kelvin 408)
  ;<  *                 bind:m  do-pork
  ;<  mov=(list move)   bind:m  (do-park %foo 407 ~)
  ;<  ~                 bind:m
    %+  expect-moves  mov
    :~  ex-wick
        (ex-gift [%tire %| [%wait %foo zuse+407]])
    ==
  ;<  mov2=(list move)  bind:m  do-wick
  ;<  ~                 bind:m  (expect-moves mov2 ~)
  ;<  mov3=(list move)  bind:m  (call ~[/blah] [%zest %foo %dead])
  ;<  ~                 bind:m
    %+  expect-moves  mov3
    :~  ex-wick
        (ex-gift [%tire %| [%zest %foo %dead]])
        ex-load
    ==
  ;<  mov4=(list move)  bind:m  do-wick
  (expect-moves mov4 ~)
::
::  commit behaviour tests
::
++  test-commit-new-sys-kelvin
::  non-essential desk receives commit with updated /sys/kelvin,
::  /sys/kel compatible with %base and old kel version
::  /sys/kel compatible with %base and new kel version
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  =/  sys-kel=(list [path (each page:clay lobe:clay)])
    [/sys/kelvin [%& ;;(page:clay kelvin+[[%1 ~] (silt :~([%zuse 409] [%zuse 410]))])]]~
  ;<  mov2=(list move)  bind:m  (do-park %foo 409 sys-kel)
  ;<  ~  bind:m
    %+  expect-moves  mov2
    :~  ex-wick
        (ex-text ": /~nul/foo/2/sys/kelvin")
        ex-load
    ==
  =/  sys-kel=(list [path (each page:clay lobe:clay)])
    [/sys/kelvin [%& ;;(page:clay kelvin+[[%1 ~] (silt :~([%zuse 409] [%zuse 408]))])]]~
  ;<  mov3=(list move)  bind:m  (do-park %foo 409 sys-kel)
  %+  expect-moves  mov3
    :~  ex-wick
        (ex-text ": /~nul/foo/3/sys/kelvin")
        (ex-gift [%tire %| [%wait %foo [%zuse 408]]])
        ex-load
    ==
::
++  test-commit-missing-perm
::  non-essential desk receives commit, with updated required perms,
::  commit suspended, receives perms, commit applied and notify on perms update
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ;<  mov=(list move)   bind:m  (do-park %foo 409 (desk-seal 1))
  ;<  mov2=(list move)  bind:m  (call ~[/blah] [%seal %foo & pers-1])
  ::
  %+  expect-moves  mov2
  :~  ex-wick
      (ex-text "+ /~nul/foo/2/desk/seal")
      (ex-ward-have %foo pers-1 pers-1)
      (ex-ward-need %foo perm-none)
      ex-load
  ==
::
++  test-commit-new-desk-seal-on-dead-desk
::  non-essential %dead desk receives commit with updated /desk/seal
::  commit applied to a %dead desk, ward update sent
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                bind:m  (do-setup-desks [%foo |] ~)
  ;<  *                bind:m  (call ~[/blah] [%zest %foo `zest:clay`%dead])
  ;<  mov=(list move)  bind:m  (do-park %foo 409 (desk-seal 1))
  ;<  ~  bind:m
    %+  expect-moves  mov
    :~  ex-wick
        (ex-text "+ /~nul/foo/2/desk/seal")
        (ex-ward-have %foo pers-1 perm-none)
    ==
  ;<  mov2=(list move)  bind:m  do-wick
  (expect-moves mov2 ~)
::
--