/+  tester, test-ford
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
          %+  expect-eq
            !>  ^-  move:clay-gate
                :-  duct=~[/info]
                ^-  (wind note:clay-gate gift:able:clay)
                :+  %pass  /castifying/~nul/home/~1111.1.1
                ^-  note:clay-gate
                :-  %f
                [%build ~nul live=%.n [%pin ~1111.1.1 [%list ~]]]
            !>  i.moves
        ::
          %+  expect-eq
            !>  ^-  move:clay-gate
                :-  duct=~[/info]
                ^-  (wind note:clay-gate gift:able:clay)
                :+  %pass  /diffing/~nul/home/~1111.1.1
                ^-  note:clay-gate
                :-  %f
                [%build ~nul live=%.n [%pin ~1111.1.1 [%list ~]]]
            !>  i.t.moves
        ::
          ^-  tang
          ::
          =/  move=move:clay-gate  i.t.t.moves
          =/  =duct                                      p.move
          =/  card=(wind note:clay-gate gift:able:clay)  q.move
          ::
          %+  weld
            (expect-eq !>(~[/info]) !>(duct))
          ::
          ?.  ?=(%pass -.card)
            [%leaf "bad move, not a %pass: {<move>}"]~
          ::
          =/  =wire  p.card
          ::
          %+  weld
            (expect-eq !>(/inserting/~nul/home/~1111.1.1) !>(wire))
          ::
          =/  note=note:clay-gate  q.card
          ::
          ?.  ?=([%f %build *] note)
            [%leaf "bad move, not a %build: {<move>}"]~
          ::
          %+  weld
            (expect-eq !>(~nul) !>(our.note))
          ::
          %+  weld
            (expect-eq !>(%.n) !>(live.note))
          ::
          %-  expect-schematic:test-ford
          :_  [-:!>(*schematic:ford) schematic.note]
          ^-  schematic:ford
          :+  %pin  ~1111.1.1
          :-  %list
          :~  :-  [%$ %path -:!>(*path) /file1/noun]
              :^  %cast  [~nul %home]  %noun
              [%$ %noun %noun 'file1']
          ::
              :-  [%$ %path -:!>(*path) /file2/noun]
              :^  %cast  [~nul %home]  %noun
              [%$ %noun %noun 'file2']
          ==
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
    %+  expect-eq
      !>  expected-moves
      !>  moves
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
