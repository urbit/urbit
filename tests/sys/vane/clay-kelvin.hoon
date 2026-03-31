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
++  do-zeal
  |=  dek=(list [=desk =zest:clay])
  =/  m  (mare ,~)
  ;<  mov=(list move)  bind:m  (call ~[/blah] [%zeal dek])
  =/  ex-mov=(list $-(move tang))  [ex-wick ~]
  |-
  ?~  dek
    (expect-moves mov (snoc ex-mov ex-load))
  =,  i.dek
  ;<  ~  bind:m  (ex-zest desk zest)
  =/  ex  (ex-gift [%tire %| [%zest desk zest]])
  $(dek t.dek, ex-mov (snoc ex-mov ex))
::
++  do-zest
  |=  [=desk =zest:clay]
  =/  m  (mare ,~)
  ;<  mov=(list move)  bind:m  (call ~[/blah] [%zest desk zest])
  ;<  ~                bind:m  (ex-zest desk zest)
  ;<  ~                bind:m
    %+  expect-moves  mov
    :~  ex-wick
        (ex-gift [%tire %| [%zest desk zest]])
        ex-load
    ==
  (do-wick ~)
::
::  granting permissions to a held desk
++  do-seal-held
  |=  [=desk peg=(set perm:gall) ped=(set perm:gall)]
  =/  m  (mare ,~)
  ;<  mov=(list move)  bind:m  (call ~[/blah] [%seal desk & peg])
  =/  mis  (~(dif in ped) peg)
  ?.  =(~ mis)
    (expect-moves mov (ex-ward-have desk ped peg) (ex-ward-need desk mis) ex-load ~)
  ::
  ;<  ~                bind:m
    %+  expect-moves  mov
    :~  (ex-ward-have desk ped peg)
        (ex-pass /park-held/[desk] [%b [%wait ~1111.1.1]])
        ex-load
    ==
  ;<  mov2=(list move)  bind:m  (take /park-held/[desk] ~[/blah] [%behn %wake ~])
  %+  expect-moves  mov2
  :~  (ex-ward-need desk perm-none)
      (ex-gift [%tire %| [%zest desk %live]])
      ex-load
  ==
::
++  do-pork  (call ~[/blah] [%pork ~])
::
++  do-park
  |=  [=desk kel=$@(@ud (lest @ud)) fil=(list [path (each page:clay lobe:clay)])]
  %+  call  ~[/blah]
  ^-  (hobo task:clay-gate)
  =/  files
    %-  ~(gas by *(map path (each page:clay lobe:clay)))
    ^-  (list [path (each page:clay lobe:clay)])
    ;:  welp
      ?:  =(%base desk)
        [/sys/zuse/hoon [%& ;;(page:clay hoon+(zuse-upd ?@(kel kel i.kel)))]]~
      ?.  =(%foo desk)  ~
        :~  [/app/bar/hoon [%& agent]]
            [/desk/bill [%& ;;(page:clay noun+:~(%bar))]]
            [/lib/skeleton/hoon [%& ;;(page:clay hoon+lib-skel)]]
            [/lib/default-agent/hoon [%& ;;(page:clay hoon+lib-def)]]
            [/mar/bill/hoon [%& ;;(page:clay hoon+mar-bill)]]
        ==
      :~
        [/mar/noun/hoon [%& ;;(page:clay hoon+mar-noun)]]
        [/mar/hoon/hoon [%& ;;(page:clay hoon+mar-hoon)]]
        [/mar/txt/hoon [%& ;;(page:clay hoon+mar-txt)]]
        [/mar/kelvin/hoon [%& ;;(page:clay hoon+mar-kel)]]
      ::
        :-  /sys/kelvin
        :-  %&  ;;  page:clay
        kelvin+?@(kel [%zuse kel] [%1 ~]^(sy (turn kel (lead %zuse))))
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
  ;<  ~                bind:m  (do-wick ~)  ::  just a formality
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
  ;<  ~                bind:m  (do-wick ~)  ::  just a formality
  $(desks t.desks)
::
++  do-sys-update
  |=  [kel=@ud ex-pork=(list $-(move tang))]
  =/  m  (mare ,~)
  ?~  ex-pork  (pure:m ~)
  ::  applying zuse update to clay
  ;<  ~                 bind:m  (set-kelvin kel)
  ;<  mov=(list move)   bind:m  do-pork
  ;<  ~  bind:m  (expect-moves mov ex-pork)
  (do-wick ~)
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
++  ex-zest
  |=  [=desk =zest:clay]
  =/  m  (mare ,~)
  ^-  form:m
  ;<  =dojo:clay-gate  bind:m  (get-dojo desk)
  ?:  =(zest liv.dom.dojo)  (pure:m ~)
  %-  fail
  :~  (rap 3 'expected ' zest ~)
      (rap 3 'got      ' liv.dom.dojo ~)
  ==
::
++  ex-pew
  |=  [=desk perm=(unit (set perm:gall))]
  =/  m  (mare ,~)
  ^-  form:m
  ;<  =dojo:clay-gate  bind:m  (get-dojo desk)
  =/  pew=(unit (set perm:gall))  ?~(pew=pew.dom.dojo ~ `-.u.pew)
  ?:  =(perm pew)  (pure:m ~)
  %-  fail
  :~  (crip "expected pew {<?~(perm perm ~(tap in u.perm))>}")
      (crip "got          {<?~(pew pew ~(tap in u.pew))>}")
  ==
::
++  ex-wait
  |=  [=desk kel=@ud]
  ^-  (list $-(move tang))
  :~  ex-wick
      (ex-gift [%tire %| [%wait desk [%zuse kel]]])
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
  ++  ex-commit
    |=  [base=(unit [v=@ud kel=@ud]) desks=(list [=desk vd=@ud ese=? held=? ped=(set perm:gall) peg=(set perm:gall)])]
    =/  one-ese  =(1 (lent (skim desks |=([@ @ ese=? *] ese))))
    |%
    ::
    ++  moves  ::  TODO: rename
      |=  [res=? kel-compat=? perm-compat=?]
      ;:  welp
          ex-base
          (ex-desks res kel-compat perm-compat)
          [ex-load]~
      ==
    ::
    ++  resume       (moves & | |)
    ::
    ++  park         (moves | | |)
    ::
    ++  park-compat  (moves | & |)
    ::
    ++  ex-base
      ?~  base  ~
      :~  ex-wick
          (ex-text ": /~nul/base/{<v.u.base>}/sys/zuse/hoon")
          (ex-text ": /~nul/base/{<v.u.base>}/sys/kelvin")
      ==
    ::
    ++  ex-desks
      |=  [res=? kel-compat=? perm-compat=?]
      =+  l=;;((list $-(move tang)) ~)
      |-  ^-  (list $-(move tang))
      ?~  desks  l
      =,  i.desks
      %=  $
        l      (welp l (ex-desk res kel-compat perm-compat))
        desks  t.desks
      ==
    ::
    ++  ex-desk
      |=  [res=? kel-compat=? perm-compat=?]
      ^-  (list $-(move tang))
      ?~  desks  [ex-wick]~
      =,  i.desks
      ;:  welp
        ?:(|(=(~ peg) perm-compat) ~ [(ex-ward-have desk ~ peg) ~])
        [ex-wick ~]
        ::
        ::  ex-text
        ?:  kel-compat  ~
        ex-text-desk
        ::
        ::  ward gifts per desk
        ?~(ped ~ [(ex-ward-have [desk ped peg]) ~])
        ?:  |(held perm-compat &(=(~ ped) =(~ peg)))  ~
        [(ex-ward-need desk perm-none) ~]
        ::
        ?:(held ex-held ~)
        %+  welp  (ex-tire desk)
        ?:(res ex-tire-base ~)
      ==
    ::
    ++  ex-held
      ?~  desks  ~
      [(ex-pass /park-held/[desk.i.desks] [%b [%wait ~1111.1.1]])]~
    ::
    ++  ex-text-desk
      ?~  desks  ~
      =,  i.desks
      =/  movs=(list $-(move tang))
        %+  welp
          [(ex-text ": /~nul/{(scow %tas desk)}/{<vd>}/sys/kelvin")]~
        ?~  ped  ~
        =/  =tape
          (welp ?:((gth vd 2) ":" "+") " /~nul/{(scow %tas desk)}/{<vd>}/desk/seal")
        [(ex-text tape)]~
      ?:((gth vd 2) (flop movs) movs)
    ::
    ++  ex-tire
      |=  =desk
      ?~  base  ~
      [(ex-gift [%tire %| [%warp desk [%zuse kel.u.base]]])]~
    ::
    ++  ex-tire-base
      ?~  desks  ~
      =,  i.desks
      =/  last=?  |(&(ese (levy t.desks |=([@ @ e=? *] !e))) !ese)
      ?:(&(!one-ese last) ~ (ex-tire %base))
  --
::
++  ex-resume-commit
  |=  [v=@ud kel=@ud desks=(list [=desk ese=? ped=(set perm:gall) peg=(set perm:gall)])]
  ^-  (list $-(move tang))
  =/  dat
    :-  `[v kel]
    %+  turn  desks
    |=  [=desk ese=? ped=(set perm:gall) peg=(set perm:gall)]
    [desk v ese | ped peg]
  resume:(ex-commit dat)
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
::  data constructors
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
  ;<  ~                  bind:m  (expect-moves mov (ex-wait %foo 408))
  ::
  ;<  ~                  bind:m  (do-wick ~)
  ::  send next kelvin update to %base
  ;<  mov2=(list move)   bind:m  (do-park %base 408 ~)
  ;<  ~  bind:m
    (expect-moves mov2 (ex-kernel-build ~ [[%foo | ~ ~ ~] ~]))
  (do-sys-update 408 (ex-resume-commit 2 408 [[%foo | perm-none perm-none] ~]))
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
  ;<  ~                 bind:m  (expect-moves mov2 (ex-wait %foo 407))
  ;<  ~                 bind:m  (do-wick ~)
  ::
  ;<  mov3=(list move)  bind:m  (do-park %base 407 ~)
  ;<  ~  bind:m
    (expect-moves mov3 (ex-kernel-build ~ [[%foo | ~ ~ ~] ~]))
  (do-sys-update 407 (ex-resume-commit 2 407 [[%foo | perm-none perm-none] ~]))
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
  ;<  ~                 bind:m  (do-wick ~)
  ::  send kelvin-1 update to a desk
  ;<  mov2=(list move)  bind:m  (do-park %foo 407 ~)
  ;<  ~                 bind:m  (do-wick ~)
  ::  apply kelvin-1 update to base
  ;<  mov3=(list move)  bind:m  (do-park %base 407 ~)
  =/  ex-com  (ex-commit `[2 407] [%foo 2 & | ~ ~]~)
  %+  do-sys-update  407
  ;:  welp
      ex-base:ex-com
      (ex-desk:ex-com | | |)
      :~  (ex-gift [%tire %| [%warp %foo [%zuse 408]]])
          ex-load
      ==
  ==
::
++  test-skip-kelvin-with-esse
::  kelvin and kelvin-1 update received on base,
::  essential desk not ready, no-op, notify
::  got compat update, update applied on base and essential desk
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo &] ~)
  ;<  mov=(list move)   bind:m  (do-park %base 408 ~)
  ;<  ~                 bind:m  (expect-moves mov (ex-gift [%tire %| [%wait %base [%zuse 408]]]) ~)
  ;<  mov2=(list move)  bind:m  (do-park %base 407 ~)
  ;<  ~                 bind:m  (expect-moves mov2 (ex-gift [%tire %| [%wait %base [%zuse 407]]]) ~)
  ;<  mov3=(list move)  bind:m  (do-park %foo 407 ~)
  ;<  ~                 bind:m  (expect-moves mov3 (ex-wait %foo 407))
  ;<  ~                 bind:m
    (do-wick (ex-kernel-build ~ [%foo & ~ ~ ~] ~))
  ::
  =/  ex-pork=(list $-(move tang))
    (ex-resume-commit 2 407 [[%foo & perm-none perm-none] ~])
  %+  do-sys-update  407
  %^  into  ex-pork  (sub (lent ex-pork) 1)
  `$-(move tang)`(ex-gift [%tire %| [%warp %base [%zuse 408]]])
::
::  essential desk kelvin update behavior tests
::
++  test-update-ready-essential-desk-compat
::  kelvin update received on base desk,
::  essential desk compatible, apply update
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                bind:m  (do-setup-desks [%foo &] ~)
  ;<  mov=(list move)  bind:m  (do-park %foo ~[409 408] ~)
  ;<  ~                bind:m
    %+  expect-moves  mov
    :~  ex-wick
        (ex-text ": /~nul/foo/2/sys/kelvin")
        (ex-gift [%tire %| [%wait %foo [%zuse 408]]])
        ex-load
    ==
  ::  update base to next kelvin
  ;<  mov2=(list move)  bind:m  (do-park %base 408 ~)
  ;<  ~                 bind:m  (expect-moves mov2 (ex-kernel-build ~ [%foo & ~ ~ ~] ~))
  %+  do-sys-update  408
  park-compat:(ex-commit `[2 408] [%foo 2 & | ~ ~] ~)
::
++  test-update-ready-essential-desk-ready
::  kelvin update received on base desk,
::  essential desk ready, apply update
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo &] ~)
  ;<  mov=(list move)   bind:m  (do-park %foo 408 ~)
  ;<  ~                 bind:m  (expect-moves mov (ex-wait %foo 408))
  ;<  ~                 bind:m  (do-wick ~)
  ;<  mov2=(list move)  bind:m  (do-park %base 408 ~)
  ;<  ~                 bind:m  (expect-moves mov2 (ex-kernel-build ~ [%foo & ~ ~ ~] ~))
  %+  do-sys-update  408
  park:(ex-commit `[2 408] [%foo 2 & | ~ ~] ~)
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
++  test-update-blocked-on-essential-desk-wrong-wic
::  essential desk recieved kelvin-2 update, waiting for kernel
::  kelvin update received on base desk, essential desk not ready
::  no-op, notify
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo &] ~)
  ::  update base to next kelvin
  ;<  mov2=(list move)  bind:m  (do-park %foo 407 ~)
  ;<  ~  bind:m
    (expect-moves mov2 ex-wick (ex-gift [%tire %| [%wait %foo [%zuse 407]]]) ~)
  ;<  mov3=(list move)  bind:m  (do-park %base 408 ~)
  ::
  (expect-moves mov3 (ex-gift [%tire %| [%wait %base [%zuse 408]]]) ~)
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
  ;<  mov=(list move)   bind:m  (do-park %base 408 ~)
  ;<  ~                 bind:m
    %+  expect-moves  mov
    [(ex-gift [%tire %| [%wait %base [%zuse 408]]])]~
  ::
  ;<  mov2=(list move)  bind:m  (do-park %foo 408 ~)
  ;<  ~                 bind:m  (expect-moves mov2 (ex-wait %foo 408))
  ;<  ~                 bind:m  (do-wick ~)
  ;<  mov3=(list move)  bind:m  (do-park %baz 408 ~)
  ;<  ~                 bind:m  (expect-moves mov3 (ex-wait %baz 408))
  ;<  ~                 bind:m  (do-wick (ex-kernel-build ~ ~))
  (do-sys-update 408 (ex-resume-commit 2 408 [%foo & perm-none perm-none] [%baz & perm-none perm-none]~))
::
++  test-update-blocked-by-perms-on-essential-desk
::
::  NOTE: maybe remove in favour of +test-update-blocked-on-desks
::
::  kelvin update received on base desk,
::  essential desk received update, but blocked on perm
::  no-op, send ward
::  essential desk permissions granted, apply update
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo &] ~)
  ;<  mov=(list move)   bind:m  (do-park %foo 408 (desk-seal 1))
  ;<  ~                 bind:m  (expect-moves mov (ex-wait %foo 408))
  ;<  mov2=(list move)  bind:m  (do-park %base 408 ~)
  ;<  ~  bind:m
    %+  expect-moves  mov2
    (ex-kernel-build ~ [[%foo & perm-none perm-none pers-1] ~])
  ;<  mov3=(list move)  bind:m  (call ~[/blah] [%seal %foo & pers-1])
  ;<  ~                 bind:m
    %+  expect-moves  mov3
    :~  (ex-ward-have %foo perm-none pers-1)
        ex-wick
        ex-load
    ==
  ;<  ~                 bind:m
    (do-wick (ex-kernel-build ~ [%foo & perm-none pers-1 perm-none] ~))
  =/  ex-com  (ex-commit `[2 408] [%foo 2 & | pers-1 pers-1] ~)
  %+  do-sys-update  408
  ;:  welp
      ex-base:ex-com
      `(list $-(move tang))`(tail (ex-desk:ex-com & | |))
      [ex-load ~]
  ==
::
++  test-update-blocked-on-desks
::  kelvin update received on base desk,
::  essential desk received update, but blocked on perm
::  no-op, send ward
::  essential desk permissions granted
::  non-essential desk not ready,
::  suspend non-essential and apply update
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                bind:m  (do-setup-desks [%foo &] [%baz |] ~)
  ;<  mov=(list move)  bind:m  (do-park %foo 408 (desk-seal 1))
  ;<  ~  bind:m  (expect-moves mov (ex-wait %foo 408))
  ::
  ;<  mov2=(list move)  bind:m  (do-park %base 408 ~)
  ;<  ~  bind:m
    %+  expect-moves  mov2
    (ex-kernel-build ~ [%foo & perm-none perm-none pers-1] ~)
  ;<  mov3=(list move)  bind:m  (call ~[/blah] [%seal %foo & pers-1])
  ;<  ~                 bind:m
    %+  expect-moves  mov3
    :~  (ex-ward-have %foo perm-none pers-1)
        ex-wick
        ex-load
    ==
  ;<  ~                 bind:m
    %-  do-wick
    (ex-kernel-build [[%baz %held] ~] [%foo & perm-none pers-1 perm-none] ~)
  ;<  ~  bind:m  (do-zeal [%baz %held]~)
  =/  ex-com  (ex-commit `[2 408] [%foo 2 & | pers-1 pers-1] ~)
  %+  do-sys-update  408
  ;:  welp
      ex-base:ex-com
      `(list $-(move tang))`(tail (ex-desk:ex-com & | |))
      [ex-load ~]
  ==
::
::
::  non-essential desk kelvin update behavior tests
::
++  test-apply-update-non-esse-up-to-date
::  non-essential desk received commit compatabile with current version
::  and future version,
::  kelvin update received on base desk, apply update, non-essential desk stays live
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ;<  mov2=(list move)  bind:m  (do-park %foo ~[409 408] ~)
  ;<  ~                 bind:m
    %+  expect-moves  mov2
    :~  ex-wick
        (ex-text ": /~nul/foo/2/sys/kelvin")
        (ex-gift [%tire %| [%wait %foo [%zuse 408]]])
        ex-load
    ==
  ;<  mov3=(list move)  bind:m  (do-park %base 408 ~)
  ;<  ~                 bind:m  (expect-moves mov3 (ex-kernel-build ~ [%foo | ~ ~ ~] ~))
  ::
  (do-sys-update 408 park-compat:(ex-commit `[2 408] [%foo 2 | | ~ ~] ~))
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
  ;<  ~  bind:m  (do-zeal [%foo %held]~)
  ;<  ~  bind:m  (do-sys-update 408 (ex-resume-commit 2 408 ~))
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
    ;<  ~                 bind:m  (do-wick ~)
    ;<  mov2=(list move)  bind:m  (take /park-held/foo ~[/blah] [%behn %wake ~])
    (expect-moves mov2 (ex-gift [%tire %| [%zest %foo %live]]) ex-load ~)
  ::
  ++  got-update-insufficient-perms
  ::  non-essential desk received update, update applied, stays %held awaiting perms
  ::  perms granted to non-essential desk, update applied and revived
  ::
    ;<  mov=(list move)  bind:m  (do-park %foo 408 (desk-seal 1))
    =/  ex-com  (ex-commit ~ [[%foo 2 | & pers-1 ~]]~)
    ;<  ~                 bind:m
      (expect-moves mov (ex-desk:ex-com | | &))
    ;<  ~                 bind:m  (do-wick ~)
    ;<  mov2=(list move)  bind:m  (take /park-held/foo ~[/blah] [%behn %wake ~])
    ;<  ~                 bind:m
      (expect-moves mov2 (ex-ward-need %foo pers-1) ex-load ~)
    (do-seal-held %foo pers-1 pers-1)
  ::
  ++  got-next-update
  ::  non-essential desk receives update, update doesn't match to current version, stays %held
  ::  re-evaluates on next update
    ;<  mov=(list move)   bind:m  (do-park %foo 407 ~)
    ;<  ~                 bind:m  (expect-moves mov ex-wick (ex-gift [%tire %| [%wait %foo [%zuse 407]]]) ~)
    ;<  ~                 bind:m  (do-wick ~)
    ::
    ;<  mov2=(list move)  bind:m  (do-park %base 407 ~)
    ;<  ~                 bind:m
      %+  expect-moves  mov2
      (ex-kernel-build ~ [[%foo | perm-none perm-none perm-none] ~])
    ::
    ;<  ~  bind:m
      %+  do-sys-update  407
      park:(ex-commit `[3 407] [%foo 2 | & ~ ~] ~)
    ::
    ;<  mov3=(list move)  bind:m  (take /park-held/foo ~[/blah] [%behn %wake ~])
    (expect-moves mov3 (ex-gift [%tire %| [%zest %foo %live]]) ex-load ~)
  --
::
++  test-apply-update-non-essential-desk-with-pew
::  non-essential desk received update
::  non-essential desk recieved commit, blocked on perms
::  kelvin update received on base desk, apply update, non-essential desk stays live, commit stays in pew
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                bind:m  (do-setup-desks [%foo |] ~)
  ;<  *                bind:m  (do-park %foo ~[409 408] ~)
  ;<  *                bind:m  (do-park %foo ~[409 408] (desk-seal 1))
  ;<  ~                bind:m  (ex-pew %foo `pers-1)
  ;<  mov=(list move)  bind:m  (do-park %base 408 ~)
  ;<  ~                bind:m  (expect-moves mov (ex-kernel-build ~ [%foo | ~ ~ ~] ~))
  ;<  ~  bind:m
    %+  do-sys-update  408
    park-compat:(ex-commit `[2 408] [%foo 1 | | ~ ~]~)
  (ex-pew %foo `pers-1)
::
++  test-apply-update-non-essential-desk-dead
::  kelvin update received on base desk, non-essential desk set to %dead
::  apply update,
::  trying to set non-essential desk live, fails to set live stays dead
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ;<  ~                 bind:m  (do-zeal [%foo %dead]~)
  ;<  mov2=(list move)  bind:m  (do-park %base 408 ~)
  ;<  ~                 bind:m  (expect-moves mov2 (ex-kernel-build ~ ~))
  ;<  ~  bind:m  (do-sys-update 408 (ex-resume-commit 2 408 ~))
  ;<  mov3=(list move)   bind:m  (call ~[/blah] [%zeal [%foo %live]~])
  ;<  ~                  bind:m
    %+  expect-moves  mov3
    :~  ex-wick
        (ex-gift [%tire %| [%zest %foo %held]])
        ex-load
    ==
  (do-wick ~)
::
++  test-non-essential-desk-missing-perm-on-kel-update
::  kelvin update received on base desk,
::  non-essential desk ready on kelvin, blocked on perms
::  non-essential desk held, kelvin update applied on base and non-esse desks
::  non-essential desk receives required perms, revived
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ;<  mov=(list move)   bind:m  (do-park %foo 408 (desk-seal 1))
  ;<  ~                 bind:m  (expect-moves mov (ex-wait %foo 408))
  ::
  ;<  mov2=(list move)  bind:m  (do-park %base 408 ~)
  ;<  ~  bind:m
    %+  expect-moves  mov2
    (ex-kernel-build ~ [[%foo | perm-none perm-none pers-1] ~])
  ;<  ~                 bind:m  (do-zeal [%foo %held]~)
  ;<  ~  bind:m  (do-sys-update 408 park:(ex-commit `[2 408] [%foo 2 | & pers-1 ~]~))
  ;<  ~  bind:m  (do-wick ~)
  (do-seal-held %foo pers-1 pers-1)
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
  (do-sys-update 408 (moves:(ex-commit `[2 408] [[%foo 2 | | pers-1 pers-1] ~]) | | &))
::
::  multiple desks tests
::
::
++  test-apply-update-partial-desks-revival
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] [%baz |] ~)
  ;<  *                 bind:m  (do-park %foo 407 ~)
  ;<  *                 bind:m  (do-park %baz 407 (desk-seal 1))
  ;<  *                 bind:m  (do-park %base 408 ~)
  ;<  ~                 bind:m  (do-zeal [[%foo %held] [%baz %held] ~])
  ;<  ~                 bind:m  (set-kelvin 408)
  ;<  *                 bind:m  do-pork
  ;<  mov=(list move)   bind:m  (do-park %base 407 ~)
  ;<  ~                 bind:m  (expect-moves mov (ex-kernel-build ~ ~))
  =/  ex-com  (ex-commit `[3 407] [%foo 2 | & ~ ~] [%baz 2 | & pers-1 ~] ~)
  ;<  ~                 bind:m  (do-sys-update 407 park:ex-com)
  ;<  mov2=(list move)  bind:m  (take /park-held/foo ~[/blah] [%behn %wake ~])
  ;<  ~  bind:m  (expect-moves mov2 (ex-gift [%tire %| [%zest %foo %live]]) ex-load ~)
  ;<  mov3=(list move)  bind:m  (take /park-held/baz ~[/blah] [%behn %wake ~])
  (expect-moves mov3 (ex-ward-need %baz pers-1) ex-load ~)
::
++  test-apply-update-suspend-and-revive-non-esse-desks
::  non-essential desk, blocked on kelvin, suspended
::  kelvin update applied on base desk
::  non-essential desk ready for kelvin-1 update
::  kelvin-1 update applied on base desk and non-essential desks, non-esse revived
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] [%baz |] ~)
  ;<  *                 bind:m  (do-park %foo 407 ~)
  ;<  *                 bind:m  (do-park %baz 407 ~)
  ;<  mov=(list move)   bind:m  (do-park %base 408 ~)
  ;<  ~                 bind:m
    %+  expect-moves  mov
    (ex-kernel-build [[%foo %held] [%baz %held] ~] ~)
  ;<  ~                 bind:m  (do-zeal [[%foo %held] [%baz %held] ~])
  ;<  ~                 bind:m  (do-sys-update 408 park:(ex-commit `[2 408] ~))
  ::
  ;<  mov2=(list move)  bind:m  (do-park %base 407 ~)
  ;<  ~                 bind:m  (expect-moves mov2 (ex-kernel-build ~ ~))
  =/  ex-com  (ex-commit `[3 407] [%foo 2 | & ~ ~] [%baz 2 | & ~ ~] ~)
  ;<  ~                 bind:m  (do-sys-update 407 park:ex-com)
  ::
  ;<  mov3=(list move)  bind:m  (take /park-held/foo ~[/blah] [%behn %wake ~])
  ;<  ~                 bind:m
    (expect-moves mov3 (ex-gift [%tire %| [%zest %foo %live]]) ex-load ~)
  ;<  mov4=(list move)  bind:m  (take /park-held/baz ~[/blah] [%behn %wake ~])
  (expect-moves mov4 (ex-gift [%tire %| [%zest %baz %live]]) ex-load ~)
::
++  test-update-blocked-on-esse-got-compat-commit
::  non-essential desk, ready for kelvin-1 update
::  essential desk recieved kelvin update, kelvin update received on base desk
::  update applie, suspend non-esse
::  kelvin-1 received on base, essential desk not ready, no-op
::  essential desk got compat commit, update appplied on base and desks
::  non-esse revived
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo &] [%baz |] ~)
  ;<  *                 bind:m  (do-park %foo 408 ~)
  ;<  *                 bind:m  (do-park %baz 407 ~)
  ;<  mov=(list move)   bind:m  (do-park %base 408 ~)
  ;<  ~                 bind:m
    (expect-moves mov (ex-kernel-build [[%baz %held] ~] [%foo & ~ ~ ~] ~))
  ;<  ~                 bind:m  (do-zeal [[%baz %held] ~])
  ;<  ~                 bind:m
    (do-sys-update 408 park:(ex-commit `[2 408] [%foo 2 & | ~ ~] ~))
  ::
  ;<  mov2=(list move)  bind:m  (do-park %base 407 ~)
  ;<  ~                 bind:m
    (expect-moves mov2 (ex-gift [%tire %| [%wait %base [%zuse 407]]]) ~)
  ::
  ;<  mov3=(list move)  bind:m  (do-park %foo ~[408 407] ~)
  ;<  ~                 bind:m
    %+  expect-moves  mov3
    :~  ex-wick
        (ex-text ": /~nul/foo/3/sys/kelvin")
        (ex-gift [%tire %| [%wait %foo [%zuse 407]]])
        ex-load
    ==
  ;<  ~                 bind:m
    (do-wick (ex-kernel-build ~ [%foo & ~ ~ ~] [%baz | ~ ~ ~]~))
  =/  ex-com  (ex-commit `[3 407] [%baz 2 | & ~ ~] [%foo 3 & | ~ ~]~)
  ;<  ~                 bind:m
  %+  do-sys-update  407
  ;:  welp
    ex-base:ex-com
    ::  (ex-desk:(ex-commit `[3 407] [%foo 3 & | ~ ~]~) & & |)
    [ex-wick]~
    (ex-tire:ex-com %foo)
    ex-tire-base:ex-com
    (ex-desk:ex-com | | |)
    [ex-load]~
  ==
  ::
  ;<  mov4=(list move)  bind:m  (take /park-held/baz ~[/blah] [%behn %wake ~])
  (expect-moves mov4 (ex-gift [%tire %| [%zest %baz %live]]) ex-load ~)
::
++  setup-non-esse-2wic  ::  TODO: change name
::  non-essential desk, blocked on kelvin and kelvin-1 and perms
::
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ;<  mov=(list move)   bind:m  (do-park %foo 408 (desk-seal 1))
  ;<  ~                 bind:m  (expect-moves mov (ex-wait %foo 408))
  ;<  mov2=(list move)  bind:m  (do-park %foo 407 (desk-seal 2))
  %+  expect-moves  mov2
  :~  ex-wick
      (ex-gift [%tire %| [%wait %foo [%zuse 407]]])
  ==
::
++  test-skip-kelvin-and-revive-non-esse
::  non-essential desk ready for kelvin, blocked on perms
::  and ready on kelvin-1 update, blocked on perms
::  kelvin update skipped in favour of kelvin-1
::  suspend non-essential desks, kelvin-1 update applied on desks
::  non-essential desk receives required perms, revived
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~                 bind:m  setup-non-esse-2wic
  ;<  mov=(list move)   bind:m  (do-park %base 407 ~)
  ;<  ~  bind:m
    %+  expect-moves  mov
    (ex-kernel-build ~ [[%foo | perm-none perm-none pers-2] ~])
  ;<  ~  bind:m  (do-zeal [%foo %held]~)
  =/  ex-com  (ex-commit `[2 407] [%foo 2 | & pers-2 ~]~)
  ;<  ~  bind:m
    %+  do-sys-update  407
    ;:  welp
        ex-base:ex-com
        (ex-desk:ex-com | | |)
        :~  (ex-gift [%tire %| [%warp %foo [%zuse 408]]])
            ex-load
        ==
    ==
  ;<  mov3=(list move)  bind:m  (take /park-held/foo ~[/blah] [%behn %wake ~])
  ;<  ~                 bind:m  (expect-moves mov3 ex-load ~)
  (do-seal-held %foo pers-2 pers-2)
::
++  test-apply-updates-revive-non-esse-desk
::  non-essential desk ready for kelvin, blocked on perms and ready on kelvin-1 update, blocked on perms
::  applying kelvin update: suspend non-esse desk, update applied on desks
::  non-essential desk receives required perms, revived
::  applying kelvin-1 update: suspend non-esse desk, kelvin-1 applied on desks
::  non-essential desk receives required perms, revived
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~                 bind:m  setup-non-esse-2wic
  ;<  mov=(list move)   bind:m  (do-park %base 408 ~)
  ;<  ~  bind:m
    %+  expect-moves  mov
    (ex-kernel-build ~ [[%foo | perm-none perm-none pers-1] ~])
  ;<  ~  bind:m  (do-zeal [%foo %held]~)
  ;<  ~  bind:m  (do-sys-update 408 park:(ex-commit `[2 408] [%foo 2 | & pers-1 ~] ~))
  ;<  mov2=(list move)  bind:m  (take /park-held/foo ~[/blah] [%behn %wake ~])
  ;<  ~                 bind:m  (expect-moves mov2 ex-load ~)
  ::
  ;<  ~                 bind:m  (do-seal-held %foo pers-1 pers-1)
  ::
  ;<  mov3=(list move)  bind:m  (do-park %base 407 ~)
  ;<  ~                 bind:m
    %+  expect-moves  mov3
    ::  not passing perms here, already got %ward on %seal
    (ex-kernel-build ~ [[%foo | ~ ~ (silt :~([%eyre ~]))] ~])
  ;<  ~        bind:m  (do-zeal [%foo %held]~)
  ;<  ~        bind:m  (do-sys-update 407 (moves:(ex-commit `[3 407] [%foo 3 | & pers-2 pers-1]~) | | &))
  ;<  mov4=(list move)  bind:m  (take /park-held/foo ~[/blah] [%behn %wake ~])
  ;<  ~                 bind:m  (expect-moves mov4 ex-load ~)
  ::
  (do-seal-held %foo pers-2 pers-2)
::
::
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
++  test-dead-to-live
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                  bind:m  (do-setup-desks [%foo |] ~)
  ;<  ~                  bind:m  (do-zest %foo %dead)
  (do-zest %foo %live)

++  test-held-to-dead
::  non-essential desk held, awaiting base update, set to dead
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                bind:m  (do-setup-desks [%foo |] ~)
  ;<  mov=(list move)  bind:m  (do-park %foo ~[409 407] ~)
  ;<  ~                bind:m
    %+  expect-moves  mov
    :~  ex-wick
        (ex-text ": /~nul/foo/2/sys/kelvin")
        (ex-gift [%tire %| [%wait %foo [%zuse 407]]])
        ex-load
    ==
  ;<   mov2=(list move)  bind:m  (do-park %base 408 ~)
  ;<  ~                  bind:m  (expect-moves mov2 (ex-kernel-build [%foo %held]~ ~))
  ;<  ~                  bind:m  (do-zeal [%foo %held]~)
  ;<  ~                  bind:m  (set-kelvin 408)
  ;<  *                  bind:m  do-pork
  ;<  ~                  bind:m  (do-wick ~)
  (do-zest %foo %dead)
::
++  test-revive-desk-awaiting-update
  ::  non-essential desk set to %dead, base got update, update applied
  ::  try to revive non-esse desk, blocked on kelvin update, set to %held
  ::  got update for non-esse desk, update applied, desk revived
  ::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ;<  ~                 bind:m  (do-zest %foo %dead)
  ;<  *                 bind:m  (do-park %base 408 ~)
  ;<  ~                 bind:m  (set-kelvin 408)
  ;<  *                 bind:m  do-pork
  ;<  mov=(list move)   bind:m  (call ~[/blah] [%zeal [%foo %live]~])
  ;<  *                 bind:m
    %+  expect-moves  mov
    :~  ex-wick
        (ex-gift [%tire %| [%zest %foo %held]])
        ex-load
    ==
  ;<  mov2=(list move)  bind:m  (do-park %foo 408 ~)
  ;<  now=@da           bind:m  get-now
  ;<  ~  bind:m
    %+  expect-moves  mov2
    :~  ex-wick
        (ex-text ": /~nul/foo/2/sys/kelvin")
        (ex-pass /park-held/foo [%b [%wait now]])
    ==
  ;<  ~                 bind:m  (do-wick ~)
  ;<  mov3=(list move)  bind:m  (take /park-held/foo ~[/blah] [%behn %wake ~])
  (expect-moves mov3 (ex-gift [%tire %| [%zest %foo %live]]) ex-load ~)
::
++  test-revive-desk-awaiting-perms
  ::  non-essential desk got update, set to %dead,
  ::  base got update, update applied to base and non-esse (stays %dead)
  ::  try to revive non-esse desk, blocked on perms, set to %held
  ::  perms granted, desk revived
  ::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *  bind:m  (do-setup-desks [%foo |] ~)
  ;<  *  bind:m  (do-park %foo 408 (desk-seal 1))
  ;<  *  bind:m  (do-zeal [%foo %dead]~)
  ;<  mov=(list move)   bind:m  (do-park %base 408 ~)
  ;<  ~  bind:m  (expect-moves mov (ex-kernel-build ~ ~))
  ;<  ~  bind:m
    (do-sys-update 408 (moves:(ex-commit `[2 408] [%foo 2 | | pers-1 perm-none]~) | | &))
  ::
  ;<  mov2=(list move)   bind:m  (call ~[/blah] [%zeal [%foo %live]~])
  ;<  ~  bind:m
    %+  expect-moves  mov2
    :~  (ex-ward-need %foo pers-1)
        ex-wick
        (ex-gift [%tire %| [%zest %foo %held]])
        ex-load
    ==
  ;<  ~  bind:m  (do-wick ~)
  (do-seal-held %foo pers-1 pers-1)
::
::  commit behaviour tests
::
++  test-commit-non-esse
::  apply commit to live non-essential desk
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                bind:m  (do-setup-desks [%baz |] ~)
  ;<  mov=(list move)  bind:m
    (do-park %baz 409 [/lib/skeleton/hoon [%& ;;(page:clay hoon+lib-skel)]]~)
  %+  expect-moves  mov
  :~  ex-wick
      (ex-text "+ /~nul/baz/2/lib/skeleton/hoon")
      ex-load
  ==
::
++  test-base-commit
::  base desk got commit, commit applied
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks ~)
  ;<  mov=(list move)   bind:m
    (do-park %base 409 [/ted/new/hoon [%& ;;(page:clay hoon+'~')]]~)
  ;<  ~  bind:m  (expect-moves mov (ex-kernel-build ~ ~))
  ;<  ~  bind:m
    %+  do-sys-update  409
    :~  ex-wick
        (ex-text "+ /~nul/base/2/ted/new/hoon")
        ex-load
    ==
  (do-wick ~)
::
++  test-commit-new-sys-kelvin
::  non-essential desk receives commit with updated /sys/kelvin,
::  /sys/kel compatible with %base and old kel version
::  /sys/kel compatible with %base and new kel version
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ;<  mov2=(list move)  bind:m  (do-park %foo ~[409 410] ~)
  ;<  ~  bind:m
    %+  expect-moves  mov2
    :~  ex-wick
        (ex-text ": /~nul/foo/2/sys/kelvin")
        ex-load
    ==
  ;<  mov3=(list move)  bind:m  (do-park %foo ~[409 408] ~)
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
  ;<  ~                 bind:m  (ex-pew %foo `pers-1)
  ;<  mov2=(list move)  bind:m  (call ~[/blah] [%seal %foo & pers-1])
  ::
  %+  expect-moves  mov2
  :~  (ex-ward-have %foo ~ pers-1)
      ex-wick
      (ex-text "+ /~nul/foo/2/desk/seal")
      (ex-ward-have %foo pers-1 pers-1)
      (ex-ward-need %foo perm-none)
      ex-load
  ==
::
++  test-commit-new-desk-seal
::  non-essential desk receives commit with updated /desk/seal
::  permission checked passed, commit applied
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ;<  mov=(list move)   bind:m  (call ~[/blah] [%seal %foo & pers-1])
  ;<  ~                 bind:m
    %+  expect-moves  mov
    :~  (ex-ward-have %foo perm-none pers-1)
        ex-load
    ==
  ;<  ~                 bind:m  (do-wick ~)
  ;<  mov2=(list move)  bind:m  (do-park %foo 409 (desk-seal 1))
  ;<  ~  bind:m
    %+  expect-moves  mov2
    :~  ex-wick
        (ex-text "+ /~nul/foo/2/desk/seal")
        (ex-ward-have %foo pers-1 pers-1)
        ex-load
    ==
  (do-wick ~)
::
++  test-commit-new-desk-seal-on-dead-desk
::  non-essential %dead desk receives commit with updated /desk/seal
::  commit applied to a %dead desk, ward update sent
::
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  *                bind:m  (do-setup-desks [%foo |] ~)
  ;<  *                bind:m  (do-zest %foo %dead)
  ;<  mov=(list move)  bind:m  (do-park %foo 409 (desk-seal 1))
  ;<  ~  bind:m
    %+  expect-moves  mov
    :~  ex-wick
        (ex-text "+ /~nul/foo/2/desk/seal")
        (ex-ward-have %foo pers-1 perm-none)
    ==
  (do-wick ~)
::
--