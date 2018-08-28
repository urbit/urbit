/+  tester
::
/=  clay-raw  /:  /===/sys/vane/clay  /!noun/
::
!:
=,  format
::
=/  test-pit=vase  !>(..zuse)
=/  clay-gate  (clay-raw test-pit)
::
|_  _tester:tester
++  test-info  ^-  tang
  ::
  =^  results1  clay-gate
    %-  clay-call-with-comparator  :*
      clay-gate
      now=~1111.1.1
      scry=*sley
      ^=  call-args
        :+  duct=~[/info]  type=-:!>(*task:able:clay)
        ^-  task:able:clay
        :^  %info  ~nul  %home
        ^-  nori:clay
        :-  %&
        ^-  soba:clay
        :~  [/file1/noun `miso:clay`[%ins [%noun %noun 'file1']]]
            [/file2/noun `miso:clay`[%ins [%noun %noun 'file2']]]
        ==
      ^=  move-comparator
        |=  moves=(list move:clay-gate)
        ^-  tang
        ::
        ?.  ?=([* * * ~] moves)
          [%leaf "wrong number of moves: {<(lent moves)>}"]~
        ::
        ^-  tang
        ;:  weld
          %-  expect-eq  !>
          :_  i.moves
          ^-  move:clay-gate
          :-  duct=~[/info]
          ^-  (wind note:clay-gate gift:able:clay)
          :+  %pass  /castifying/~nul/home/~1111.1.1
          ^-  note:clay-gate
          :-  %f
          [%build ~nul live=%.n [%pin ~1111.1.1 [%list ~]]]
        ::
          %-  expect-eq  !>
          :_  i.t.moves
          ^-  move:clay-gate
          :-  duct=~[/info]
          ^-  (wind note:clay-gate gift:able:clay)
          :+  %pass  /diffing/~nul/home/~1111.1.1
          ^-  note:clay-gate
          :-  %f
          [%build ~nul live=%.n [%pin ~1111.1.1 [%list ~]]]
        ::
          ^-  tang
          ::
          =/  move=move:clay-gate  i.t.t.moves
          =/  =duct                                      p.move
          =/  card=(wind note:clay-gate gift:able:clay)  q.move
          ::
          %+  weld
            (expect-eq !>([~[/info] duct]))
          ::
          ?.  ?=(%pass -.card)
            [%leaf "bad move, not a %pass: {<move>}"]~
          ::
          =/  =wire  p.card
          ::
          %+  weld
            (expect-eq !>([/inserting/~nul/home/~1111.1.1 wire]))
          ::
          =/  note=note:clay-gate  q.card
          ::
          ?.  ?=([%f %build *] note)
            [%leaf "bad move, not a %build: {<move>}"]~
          ::
          %+  weld
            (expect-eq !>([~nul our.note]))
          ::
          %+  weld
            (expect-eq !>([%.n live.note]))
          ::
          ?.  ?=(%pin -.schematic.note)
            [%leaf "bad move, not a %pin: {<move>}"]~
          ::
          %+  weld
            (expect-eq !>([~1111.1.1 date.schematic.note]))
          ::
          =/  list-schematic=schematic:ford  schematic.schematic.note
          ::
          ?.  ?=(%list -.list-schematic)
            [%leaf "bad move, not a %list: {<move>}"]~
          ::
          ?.  ?=([* * ~] schematics.list-schematic)
            [%leaf "bad move, wrong number of sub-schematics: {<move>}"]~
          ::
          =/  s1=schematic:ford  i.schematics.list-schematic
          =/  s2=schematic:ford  i.t.schematics.list-schematic
          ::  test :s1
          ::
          ?.  ?=([^ *] s1)
            [%leaf "bad move, s1 not cell: {<move>}"]~
          ::
          =/  path1=schematic:ford  -.s1
          ?.  ?=([%$ %path *] path1)
            [%leaf "bad move, path1 not %path: {<move>}"]~
          ::
          =/  vase1=vase  q.literal.path1
          ::
          %+  weld
            (expect-eq !>([[-:!>(*path) /file1/noun] vase1]))
          ::
          =/  cast1=schematic:ford  +.s1
          ::
          ?.  ?=([%cast [%~nul %home] %noun *] cast1)
            [%leaf "bad move, wrong cast1: {<move>}"]~
          ::
          =/  lit1=schematic:ford  input.cast1
          ::
          %+  weld
            (expect-eq !>([[%$ %noun %noun 'file1'] lit1]))
          ::  test :s2
          ::
          ?.  ?=([^ *] s2)
            [%leaf "bad move, s2 not cell: {<move>}"]~
          ::
          =/  path2=schematic:ford  -.s2
          ?.  ?=([%$ %path *] path2)
            [%leaf "bad move, path2 not %path: {<move>}"]~
          ::
          =/  vase2=vase  q.literal.path2
          ::
          %+  weld
            (expect-eq !>([[-:!>(*path) /file2/noun] vase2]))
          ::
          =/  cast2=schematic:ford  +.s2
          ::
          ?.  ?=([%cast [%~nul %home] %noun *] cast2)
            [%leaf "bad move, wrong cast2: {<move>}"]~
          ::
          =/  lit2=schematic:ford  input.cast2
          ::
          (expect-eq !>([[%$ %noun %noun 'file2'] lit2]))
    ==  ==
  ::
  ;:  welp
    results1
  ==
::  |utilities: helper functions for testing
::
::+|  utilities
::
::  +clay-call: have clay run a +task and assert it produces :expected-moves7890
::
::    TODO: make this generic for any vane
::
++  clay-call
  |=  $:  clay-gate=_clay-gate
          now=@da
          scry=sley
          call-args=[=duct =type wrapped-task=(hobo task:able:clay)]
          expected-moves=(list move:clay-gate)
      ==
  ^-  [tang _clay-gate]
  ::
  =/  clay-core  (clay-gate now=now eny=0xdead.beef scry=scry)
  ::
  =^  moves  clay-gate  (call:clay-core call-args)
  ::
  =/  output=tang
    %-  expect-eq  !>
    :-  expected-moves
    moves
  ::
  [output clay-gate]
::  +clay-call-with-comparator: run a clay +task and test output moves
::
::    TODO: make this generic for any vane
::
++  clay-call-with-comparator
  |=  $:  clay-gate=_clay-gate
          now=@da
          scry=sley
          call-args=[=duct =type wrapped-task=(hobo task:able:clay)]
          move-comparator=$-((list move:clay-gate) tang)
      ==
  ^-  [tang _clay-gate]
  ::
  =/  clay-core  (clay-gate now=now eny=0xdead.beef scry=scry)
  ::
  =^  moves  clay-gate  (call:clay-core call-args)
  ::
  =/  output=tang  (move-comparator moves)
  ::
  [output clay-gate]
--
