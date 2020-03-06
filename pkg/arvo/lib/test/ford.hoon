/+  *test
::
/=  ford-vane   /:  /===/sys/vane/ford  /!noun/
::
/=  hoon-scry   /:  /===/sys/hoon       /hoon/
/=  arvo-scry   /:  /===/sys/arvo       /hoon/
/=  zuse-scry   /:  /===/sys/zuse       /hoon/
/=  txt-scry    /:  /===/mar/txt        /hoon/
/=  diff-scry   /:  /===/mar/txt-diff   /hoon/
::
!:
=,  ford
=,  format
::
=/  test-pit=vase  !>(..zuse)
=/  ford-gate  (ford-vane test-pit)
::
|%
++  verify-post-made
  |=  $:  move=move:ford-gate
          =duct
          =type
          date=@da
          title=@tas
          contents=tape
      ==
  ^-  tang
  ::
  ?>  ?=([* %give %made @da %complete %success ^ *] move)
  =/  result  build-result.result.p.card.move
  ?>  ?=([%success %scry %noun type-a=* @tas *] head.result)
  ?>  ?=([%success ^ *] tail.result)
  ?>  ?=([%success %ride type-title-a=* %post-a] head.tail.result)
  ?>  ?=([%success %ride type-title-b=* %post-b] tail.tail.result)
  ::
  ;:  welp
    %+  expect-eq
      !>  duct
      !>  duct.move
  ::
    %+  expect-eq
      !>  date
      !>  date.p.card.move
  ::
    %+  expect-eq
      !>  [%success %scry %noun *^type [title=title contents=contents]]
      !>  head.result(p.q.cage *^type)
  ::
    %+  expect-eq
      !>  &
      !>  (~(nest ut p.q.cage.head.result) | type)
  ::
    %+  expect-eq
      !>  'post-a'
      vase.head.tail.result
  ::
    %+  expect-eq
      !>  'post-b'
      vase.tail.tail.result
  ==
++  scry-with-results
  |=  results=(map [=term =beam] cage)
  |=  [* (unit (set monk)) =term =beam]
  ^-  (unit (unit cage))
  ::
  =/  date=@da  ?>(?=(%da -.r.beam) p.r.beam)
  ::
  ?^  reef=((scry-reef date) +<.$)
    reef
  ::
  ~|  scry-with-results+[term=term beam=beam]
  ::
  [~ ~ (~(got by results) [term beam])]
::  +scry-with-results-and-failures
::
++  scry-with-results-and-failures
  |=  results=(map [=term =beam] (unit cage))
  |=  [* (unit (set monk)) =term =beam]
  ^-  (unit (unit cage))
  ::
  =/  date=@da  ?>(?=(%da -.r.beam) p.r.beam)
  ::
  ?^  reef=((scry-reef date) +<.$)
    reef
  ::
  ~|  scry-with-results+[term=term beam=beam]
  ::
  [~ (~(got by results) [term beam])]
::  +scry-succeed: produces a scry function with a known request and answer
::
++  scry-succeed
  |=  [date=@da result=cage]  ^-  sley
  |=  [* (unit (set monk)) =term =beam]
  ^-  (unit (unit cage))
  ::
  ?^  reef=((scry-reef date) +<.$)
    reef
  ::
  ~|  scry-succeed+[beam+beam term+term]
  ?>  =(term %cx)
  ?>  =(beam [[~nul %desk %da date] /bar/foo])
  ::
  [~ ~ result]
::  +scry-fail: produces a scry function with a known request and failed answer
::
++  scry-fail
  |=  date=@da  ^-  sley
  |=  [* (unit (set monk)) =term =beam]
  ^-  (unit (unit cage))
  ::
  ?^  reef=((scry-reef date) +<.$)
    reef
  ::
  ~|  scry-fail+[beam+beam term+term]
  ?>  =(term %cx)
  ?>  =(beam [[~nul %desk %da date] /bar/foo])
  ::
  [~ ~]
::  +scry-block: produces a scry function with known request and blocked answer
::
++  scry-block
  |=  date=@da  ^-  sley
  |=  [* (unit (set monk)) =term =beam]
  ^-  (unit (unit cage))
  ::
  ?^  reef=((scry-reef date) +<.$)
    reef
  ::
  ~|  scry-block+[beam+beam term+term]
  ?>  =(term %cx)
  ?>  =(beam [[~nul %desk %da date] /bar/foo])
  ::
  ~
::  +scry-blocks: block on a file at multiple dates; does not include %reef
::
++  scry-blocks
  |=  dates=(set @da)  ^-  sley
  |=  [* (unit (set monk)) =term =beam]
  ^-  (unit (unit cage))
  ::
  ~|  scry-block+[beam+beam term+term]
  ?>  =(term %cx)
  ?>  ?=([%da @da] r.beam)
  ?>  (~(has in dates) p.r.beam)
  ::
  ~
::  +scry-is-forbidden: makes sure ford does not attempt to scry
::
++  scry-is-forbidden  ^-  sley
  |=  [* (unit (set monk)) =term =beam]
  ^-  (unit (unit cage))
  ::
  =/  date=@da  ?>(?=(%da -.r.beam) p.r.beam)
  ::
  ?^  reef=((scry-reef date) +<.$)
    reef
  ::
  ~|  scry-is-forbidden+[beam+beam term+term]
  !!
::
++  scry-reef
  |=  date=@da  ^-  sley
  |=  [* (unit (set monk)) =term =beam]
  ^-  (unit (unit cage))
  ::
  =-  ?~  res=(~(get by -) [term beam])
        ~
      `res
  ::
  (with-reef date ~)
::
++  with-reef
  |=  [date=@da scry-results=(map [term beam] cage)]
  ^+  scry-results
  %-  ~(gas by scry-results)
  :~  :-  [%cx [[~nul %home %da date] /hoon/hoon/sys]]
      [%hoon !>(hoon-scry)]
      :-  [%cx [[~nul %home %da date] /hoon/arvo/sys]]
      [%hoon !>(arvo-scry)]
      :-  [%cx [[~nul %home %da date] /hoon/zuse/sys]]
      [%hoon !>(zuse-scry)]
  ::
      :-  [%cw [[~nul %home %da date] /hoon/hoon/sys]]
      [%cass !>([ud=1 da=date])]
  ==
::
++  with-reef-unit
  |=  [date=@da scry-results=(map [term beam] (unit cage))]
  ^+  scry-results
  %-  ~(gas by scry-results)
  :~  :-  [%cx [[~nul %home %da date] /hoon/hoon/sys]]
      `[%noun !>(~)]
      :-  [%cx [[~nul %home %da date] /hoon/arvo/sys]]
      `[%noun !>(~)]
      :-  [%cx [[~nul %home %da date] /hoon/zuse/sys]]
      `[%noun !>(~)]
  ::
      :-  [%cw [[~nul %home %da date] /hoon/hoon/sys]]
      `[%cass !>([ud=1 da=date])]
  ==
::
++  ford-call
  |=  $:  ford-gate=_ford-gate
          now=@da
          scry=sley
          call-args=[=duct type=* wrapped-task=(hobo task:able:ford-gate)]
          expected-moves=(list move:ford-gate)
      ==
  ^-  [tang _ford-gate]
  ::
  =/  ford  (ford-gate our=~nul now=now eny=`@`0xdead.beef scry=scry)
  ::
  =^  moves  ford-gate
    %-  call:ford  [duct ~ type wrapped-task]:call-args
  ::
  =/  output=tang
    %+  expect-eq
      !>  expected-moves
      !>  moves
  ::
  [output ford-gate]
::
++  ford-take
  |=  $:  ford-gate=_ford-gate
          now=@da
          scry=sley
          take-args=[=wire =duct wrapped-sign=(hypo sign:ford-gate)]
          expected-moves=(list move:ford-gate)
      ==
  ^-  [tang _ford-gate]
  ::
  =/  ford  (ford-gate our=~nul now=now eny=`@`0xdead.beef scry=scry)
  ::
  =^  moves  ford-gate
    %-  take:ford  [wire duct ~ wrapped-sign]:take-args
  ::
  =/  output=tang
    %+  expect-eq
      !>  expected-moves
      !>  moves
  ::
  [output ford-gate]
::  +ford-call-with-comparator
::
::    Sometimes we can't just do simple comparisons between the moves statements
::    and must instead specify a gate that performs the comparisons.
::
++  ford-call-with-comparator
  |=  $:  ford-gate=_ford-gate
          now=@da
          scry=sley
          call-args=[=duct type=* wrapped-task=(hobo task:able:ford-gate)]
          move-comparator=$-((list move:ford-gate) tang)
      ==
  ^-  [tang _ford-gate]
  ::
  =/  ford  (ford-gate our=~nul now=now eny=`@`0xdead.beef scry=scry)
  ::
  =^  moves  ford-gate
    %-  call:ford  [duct ~ type wrapped-task]:call-args
  ::
  =/  output=tang  (move-comparator moves)
  ::
  [output ford-gate]
::  +ford-take-with-comparator
::
++  ford-take-with-comparator
  |=  $:  ford-gate=_ford-gate
          now=@da
          scry=sley
          take-args=[=wire =duct wrapped-sign=(hypo sign:ford-gate)]
          move-comparator=$-((list move:ford-gate) tang)
      ==
  ^-  [tang _ford-gate]
  ::
  =/  ford  (ford-gate our=~nul now=now eny=`@`0xdead.beef scry=scry)
  ::
  =^  moves  ford-gate
    %-  take:ford  [wire duct ~ wrapped-sign]:take-args
  ::
  =/  output=tang  (move-comparator moves)
  ::
  [output ford-gate]
::  +expect-cage: assert that the actual cage has the right mark and vase
::
++  expect-cage
  |=  [mark=term expected=vase actual=cage]
  %+  weld
    %+  expect-eq
      !>  mark
      !>  p.actual
  ::
  (expect-eq expected q.actual)
::  +expect-ford-empty: assert that ford's state is one empty ship
::
::    At the end of every test, we want to assert that we have cleaned up all
::    state.
::
++  expect-ford-empty
  |=  [ford-gate=_ford-gate ship=@p]
  ^-  tang
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      call-args=[duct=~[/empty] type=~ [%keep 0 0]]
      expected-moves=~
    ==
  ::
  =/  ford  *ford-gate
  =/  state  state.ax.+>+<.ford
  ::
  =/  default-state  *ford-state:ford
  ::
  =.  max-size.compiler-cache.state     max-size.compiler-cache.default-state
  =.  max-size.queue.build-cache.state  max-size.queue.build-cache.default-state
  =.  next-anchor-id.build-cache.state  0
  ::
  %+  welp  results1
  ::
  ?:  =(default-state state)
    ~
  ::
  =/  build-state=(list tank)
    %-  zing
    %+  turn  ~(tap by builds.state)
    |=  [build=build:ford build-status=build-status:ford]
    :~  [%leaf (build-to-tape:ford build)]
        [%leaf "requesters: {<requesters.build-status>}"]
        [%leaf "clients: {<~(tap in ~(key by clients.build-status))>}"]
    ==
  ::
  =/  braces  [[' ' ' ' ~] ['{' ~] ['}' ~]]
  ::
  :~  [%leaf "failed to cleanup"]
      [%leaf "builds.state:"]
      [%rose braces build-state]
  ==
--
