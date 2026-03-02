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
++  move  move:clay-gate
::
::  advance time
::
++  wait
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
++  test-blocked-on-kelvin
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
    :~  ex-wick
        (ex-text ": /~nul/base/2/sys/zuse/hoon")
        (ex-text ": /~nul/base/2/sys/kelvin")
        ex-wick
        (ex-text ": /~nul/foo/2/sys/kelvin")
        ex-load
    ==
  ;<  mov6=(list move)  bind:m  do-wick
  (expect-moves mov6 ~)
::
++  test-blocked-on-kelvin-1
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
    :~  ex-wick
        (ex-text ": /~nul/base/2/sys/zuse/hoon")
        (ex-text ": /~nul/base/2/sys/kelvin")
        ex-wick
        (ex-text ": /~nul/foo/2/sys/kelvin")
        ex-load
    ==
  ;<  mov6=(list move)  bind:m  do-wick
  (expect-moves mov6 ~)
::
++  test-blocked-on-kelvin-and-kelvin-1
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
        ex-load
    ==
  ;<  mov5=(list move)  bind:m  do-wick
  (expect-moves mov5 ~)
::
++  test-blocked-on-esse
  %-  eval-mare
  =/  m  (mare ,~)
  ;<  ~                 bind:m  (do-setup-desks [%foo &] ~)
  ;<  mov=(list move)   bind:m  (call ~[/blah] [%tire `~])
  ::  update base to next kelvin
  ;<  mov2=(list move)  bind:m  (do-park %base 408 ~)
  ::
  ::  NOTE:  %zest despite unchanged could be a walk tire bug
  %+  expect-moves  mov2
  :~  (ex-gift [%tire %| [%zest %foo %live]])
      (ex-gift [%tire %| [%zest %base %live]])
      (ex-gift [%tire %| [%wait %base [%zuse 408]]])
  ==
::
++  test-apply-kel-suspend-foo
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
    :~  ex-wick
        (ex-text ": /~nul/base/2/sys/zuse/hoon")
        (ex-text ": /~nul/base/2/sys/kelvin")
        ex-load
    ==
  ;<  mov4=(list move)  bind:m  do-wick
  (expect-moves mov4 ~)
::
:: ++  test-missing-perm-on-commit
::   %-  eval-mare
::   =/  m  (mare ,~)
::   ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
::   ;<  mov=(list move)   bind:m  (do-park %foo 409 desk-seal)
::   ;<  mov2=(list move)  bind:m  (call ~[/blah] [%seal %foo & (silt [%behn %timer]~)])
::   ::
::   %+  expect-moves  mov2
::   :~  ex-wick
::       (ex-text ": /~nul/foo/2/desk/seal")
::       ex-load
::   ==
::
:: ++  test-missing-perm-on-kel-update
::   %-  eval-mare
::   =/  m  (mare ,~)
::   ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
::   ;<  mov=(list move)   bind:m  (do-park %foo 408 desk-seal)
::   ;<  *                 bind:m  do-wick
::   ;<  mov2=(list move)  bind:m  (do-park %base 408 ~)
::   ;<  ~  bind:m
::     %+  expect-moves  mov2
::     (ex-kernel-build [%foo %held]~)
::   ;<  ~  bind:m  (set-kelvin 408)
::   ;<  mov3=(list move)  bind:m  (call ~[/blah] [%zeal [%foo %held]~])
::   ;<  mov4=(list move)  bind:m  do-pork
::   ;<  mov5=(list move)  bind:m  (call ~[/blah] [%seal %foo & (silt [%behn %timer]~)])
::   ::
::   ;<  ~  bind:m
::     %+  expect-moves  mov5
::     :~  ex-wick
::         (ex-text ": /~nul/foo/2/desk/seal")
::         (ex-text ": /~nul/foo/2/sys/kelvin")
::         (ex-pass /park-held/foo [%b [%wait now]])
::         ex-load
::     ==
::   ;<  *                 bind:m  do-wick
::   ;<  mov6=(list move)  bind:m  (take /park-held/foo ~[/blah] [%behn %wake ~])
::   (expect-moves mov6 ex-load ~)
::
::  case: blocking on required perms and next kelvin,
::  receive perms, then receive kelvin, must proceed.
:: ++  test-missing-perm-on-kel-update-2
::   %-  eval-mare
::   =/  m  (mare ,~)
::   ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
::   ;<  mov=(list move)   bind:m  (do-park %foo 408 desk-seal)
::   ;<  mov2=(list move)  bind:m  (call ~[/blah] [%seal %foo & (silt [%behn %timer]~)])
::   ;<  ~  bind:m  (expect-moves mov2 ex-wick ex-load ~)
::   ;<  *                 bind:m  do-wick
::   ;<  mov3=(list move)  bind:m  (do-park %base 408 ~)
::   ;<  ~  bind:m
::     %+  expect-moves  mov3
::     (ex-kernel-build ~)
::   ;<  ~  bind:m  (set-kelvin 408)
::   ;<  mov4=(list move)  bind:m  do-pork
::   %+  expect-moves  mov4
::   :~  ex-wick
::       (ex-text ": /~nul/base/2/sys/zuse/hoon")
::       (ex-text ": /~nul/base/2/sys/kelvin")
::       ex-wick
::       (ex-text ": /~nul/foo/2/desk/seal")
::       (ex-text ": /~nul/foo/2/sys/kelvin")
::       ex-load
::   ==
::
::  case: blocking on perms1+kelvin1 and perms2+kelvin2
++  foo-apply-kel2
  =/  m  (mare ,~)
  ;<  *                 bind:m  (do-setup-desks [%foo |] ~)
  ;<  mov=(list move)   bind:m  (do-park %foo 408 desk-seal)
  ;<  mov2=(list move)  bind:m  (do-park %foo 407 [/desk/seal [%& ;;(page:clay seal+[%0 :~([%behn %timer] [%eyre %serve])])]]~)
  (expect-moves mov2 ~)
::
:: ++  test-apply-kelvin2-and-perms2
::   %-  eval-mare
::   =/  m  (mare ,~)
::   ;<  ~                 bind:m  foo-apply-kel2
::   ;<  mov=(list move)   bind:m  (do-park %base 407 ~)
::   ;<  ~  bind:m
::     %+  expect-moves  mov
::     (ex-kernel-build [%foo %held]~)
::   ;<  mov2=(list move)  bind:m  (call ~[/blah] [%zeal [%foo %held]~])
::   ;<  ~                 bind:m  (set-kelvin 407)
::   ;<  mov3=(list move)  bind:m  do-pork
::   ;<  ~  bind:m
::     %+  expect-moves  mov3
::     :~  ex-wick
::         (ex-text ": /~nul/base/2/sys/zuse/hoon")
::         (ex-text ": /~nul/base/2/sys/kelvin")
::         ex-load
::     ==
::   =/  perms
::     (silt `(list perm:gall)`:~([%eyre %serve] [%behn %timer]))
::   ;<  mov4=(list move)  bind:m  (call ~[/blah] [%seal %foo & perms])
::   ;<  ~  bind:m
::     %+  expect-moves  mov4
::     :~  ex-wick
::         (ex-text ": /~nul/foo/2/desk/seal")
::         (ex-text ": /~nul/foo/2/sys/kelvin")
::         (ex-pass /park-held/foo [%b [%wait now]])
::         ex-load
::     ==
::   ;<  mov5=(list move)  bind:m  do-wick
::   ;<  mov6=(list move)  bind:m  (take /park-held/foo ~[/blah] [%behn %wake ~])
::   (expect-moves mov6 ex-load ~)
:: ::
:: ++  test-apply-kelvin-1-and-perms2
::   %-  eval-mare
::   =/  m  (mare ,~)
::   ;<  ~                 bind:m  foo-apply-kel2
::   ;<  mov=(list move)   bind:m  (do-park %base 408 ~)
::   ;<  ~  bind:m
::     %+  expect-moves  mov
::     (ex-kernel-build [%foo %held]~)
::   ;<  mov2=(list move)  bind:m  (call ~[/blah] [%zeal [%foo %held]~])
::   ;<  ~                 bind:m  (set-kelvin 408)
::   ;<  mov3=(list move)  bind:m  do-pork
::   ;<  ~  bind:m
::     %+  expect-moves  mov3
::     :~  ex-wick
::         (ex-text ": /~nul/base/2/sys/zuse/hoon")
::         (ex-text ": /~nul/base/2/sys/kelvin")
::         ex-load
::     ==
::   ;<  mov4=(list move)  bind:m  (call ~[/blah] [%seal %foo & (silt :~([%behn %timer]))])
::   ::  NOTE, current bug: desk isn't live,
::   ::   still has an asssumption of missing permissions,
::   ::   even tho permissions for current version of zuse are fulfilled
::   ;<  ~  bind:m  (expect-moves mov4 ex-load ~)
::   ;<  mov5=(list move)   bind:m  (do-park %base 407 ~)
::   ;<  ~  bind:m
::     %+  expect-moves  mov5
::     (ex-kernel-build [%foo %held]~)
::   ;<  mov6=(list move)  bind:m  (call ~[/blah] [%zeal [%foo %held]~])
::   ;<  ~                 bind:m  (set-kelvin 407)
::   ;<  mov7=(list move)  bind:m  do-pork
::   ;<  now=@da           bind:m  get-now
::   ;<  ~  bind:m
::     %+  expect-moves  mov7
::     :~  ex-wick
::         (ex-text ": /~nul/base/3/sys/zuse/hoon")
::         (ex-text ": /~nul/base/3/sys/kelvin")
::         ex-load
::     ==
::   ;<  mov8=(list move)  bind:m  (call ~[/blah] [%seal %foo & (silt :~([%eyre %serve]))])
::   ;<  ~  bind:m
::     %+  expect-moves  mov8
::     :~  ex-wick
::         (ex-text ": /~nul/foo/2/desk/seal")
::         (ex-text ": /~nul/foo/2/sys/kelvin")
::         (ex-pass /park-held/foo [%b [%wait now]])
::         ex-load
::     ==
::   ;<  *                 bind:m  do-wick
::   ;<  mov9=(list move)  bind:m  (take /park-held/foo ~[/blah] [%behn %wake ~])
::   (expect-moves mov9 ex-load ~)
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
  ::  set up any other desks we want
  ::
  |-  =*  loop  $
  ?~  desks  (pure:m ~)
  =,  i.desks
  ;<  *                bind:m  (do-new-desk desk)
  ;<  *                bind:m
    ?.  esse  (pure:m ~)
    (call ~[/blah] [%esse desk %.y])
  ;<  mov=(list move)  bind:m  (call ~[/blah] [%zest desk `zest:clay`%live])
  ;<  ~                bind:m  (expect-moves mov ex-wick ex-load ~)
  $(desks t.desks)
::
::  applying zuse update to clay
++  set-kelvin
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
++  do-new-desk
  |=  =desk
  (do-park desk 409 ~)
::   =/  files
::     %-  ~(gas by *(map path (each page:clay lobe:clay)))
::     ^-  (list [path (each page:clay lobe:clay)])
::     %+  welp
::       ?:  =(%base desk)
::         [/sys/zuse/hoon [%& ;;(page:clay hoon+zus)]]~
::       :~
::         [/app/bar/hoon [%& agent]]
::         [/lib/skeleton/hoon [%& ;;(page:clay hoon+lib-skel)]]
::         [/lib/default-agent/hoon [%& ;;(page:clay hoon+lib-def)]]
::         [/mar/bill/hoon [%& ;;(page:clay hoon+mar-bill)]]
::         [/desk/bill [%& ;;(page:clay noun+:~(%bar))]]
::         [/desk/seal [%& ;;(page:clay seal+[%0 ~])]]
::       ==
::     :~
::       [/mar/noun/hoon [%& ;;(page:clay hoon+mar-noun)]]
::       [/mar/hoon/hoon [%& ;;(page:clay hoon+mar-hoon)]]
::       [/mar/txt/hoon [%& ;;(page:clay hoon+mar-txt)]]
::       [/mar/kelvin/hoon [%& ;;(page:clay hoon+mar-kel)]]
::       [/sys/kelvin [%& ;;(page:clay kelvin+[%zuse 409])]]
::     ==
::   =/  =yoki:clay  [%& [*(list tako:clay) files]]
::   [%park desk yoki *rang:clay]
:: ::
++  desk-seal
  ^-  (list [path (each page:clay lobe:clay)])
  [/desk/seal [%& ;;(page:clay seal+[%0 :~([%behn %timer])])]]~
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
--