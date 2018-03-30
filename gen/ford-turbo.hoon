/+  ford-turbo, tester
::
:-  %say
|=  [[now=@da eny=@ =beak] ~ ~]
:-  %noun
=+  tester:tester
|^
^-  wall
;:  weld
  test-is-schematic-live
  test-date-from-schematic
  test-unify-jugs
  test-dependency-wire-encoding
  test-literal
  test-autocons-same
  test-autocons-different
  test-scry-clay-succeed
  test-scry-clay-fail
  test-scry-clay-block
  test-scry-clay-live
  test-pinned-in-live
==
++  test-is-schematic-live
  ~&  %test-is-schematic-live
  ::
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-is-forbidden)
  ::  pinned-schematic shows a once build
  ::
  =/  pinned-schematic=schematic:ford
    [%pin now [%scry [%clay-live care=%x bel=[[~nul %desk] /foo/bar]]]]
  ::  live-schematic shows a live build
  ::
  =/  live-schematic=schematic:ford
    [%scry [%clay-live care=%x bel=[[~nul %desk] /baz/doo]]]
  ::  eternal-schematic shows a trivial live build
  ::
  =/  eternal-schematic=schematic:ford
    [head=[%$ %noun !>(42)] tail=[%$ %noun !>(43)]]
  ::
  ;:  welp
  ::
    %-  expect-eq  !>
    [%.y (is-schematic-live:ford live-schematic)]
  ::
    %-  expect-eq  !>
    [%.n (is-schematic-live:ford pinned-schematic)]
  ::
    %-  expect-eq  !>
    [%.y (is-schematic-live:ford [%alts ~[live-schematic pinned-schematic]])]
  ::
    %-  expect-eq  !>
    [%.y (is-schematic-live:ford eternal-schematic)]
  ==
::
++  test-date-from-schematic
  ~&  %test-date-from-schematic
  ::
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-is-forbidden)
  ::
  =/  six-schematic=schematic:ford
    [%pin ~1234.5.6 [%scry [%clay-live care=%x bel=[[~nul %desk] /foo/bar]]]]
  =/  nine-schematic=schematic:ford
    [%pin ~1234.5.9 [%scry [%clay-live care=%x bel=[[~nul %desk] /baz/doo]]]]
  =/  three-schematic=schematic:ford
    [%pin ~1234.5.3 [%scry [%clay-live care=%x bel=[[~nul %desk] /car/dor]]]]
  ;:  welp
  ::
    %-  expect-eq  !>
    [~1234.5.9 (date-from-schematic:ford [three-schematic nine-schematic])]
  ::
    %-  expect-eq  !>
    :-  ~1234.5.9
    %-  date-from-schematic:ford
    [six-schematic [%alts ~[three-schematic nine-schematic]]]
  ==
::
++  test-unify-jugs
  ~&  %test-unify-jugs
  ::
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-is-forbidden)
  ::
  %-  expect-eq  !>
  :-  %+  unify-jugs:ford
        `(jug @tas @ud)`(my ~[[%a (sy 1 2 ~)] [%b (sy 3 4 ~)]])
      `(jug @tas @ud)`(my ~[[%b (sy 5 6 ~)] [%c (sy 7 8 ~)]])
  ::
  `(jug @tas @ud)`(my ~[[%a (sy 1 2 ~)] [%b (sy 3 4 5 6 ~)] [%c (sy 7 8 ~)]])
::
++  test-dependency-wire-encoding
  ~&  %test-dependency-wire-encoding
  ::
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-is-forbidden)
  ::
  ;:  welp
    %-  expect-eq  !>
    :-  `path`(to-wire:ford [%clay-live care=%x bel=[[~nul %desk] /foo/bar]])
    /c/x/~nul/desk/0/bar/foo
  ::
    %-  expect-eq  !>
    :-  `dependency:ford`[%clay-live care=%x bel=[[~nul %desk] /foo/bar]]
    (from-wire:ford /c/x/~nul/desk/0/bar/foo)
  ::
    %-  expect-eq  !>
    :-  ^-  path
      (to-wire:ford [%gall-live care=%x bel=[[~nul %desk] /foo/bar]])
    /g/x/~nul/desk/0/bar/foo
  ::
    %-  expect-eq  !>
    :-  ^-  dependency:ford
      [%gall-live care=%x bel=[[~nul %desk] /foo/bar]]
    (from-wire:ford /g/x/~nul/desk/0/bar/foo)
  ==
::
++  test-literal
  ~&  %test-literal
  ::
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-is-forbidden)
  ::
  =^  moves  ford
    %-  call:ford
    :*  duct=~  type=~  %make  ~nul
        [%pin ~1234.5.6 [%$ %noun !>(**)]]
    ==
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~  %give  %made  ~1234.5.6
            %complete  %result  %pin  ~1234.5.6  %result  %$  %noun  !>(**)
    ==  ==
  ::
  %-  expect-eq  !>
  :-  state-by-ship.+>+<.ford
  (my [~nul *ford-state:ford-turbo]~)
::
++  test-autocons-same
  ~&  %test-autocons-same
  ::
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-is-forbidden)
  ::
  =^  moves  ford
    %-  call:ford
    :*  duct=~  type=~  %make  ~nul
        [%pin ~1234.5.6 [%$ %noun !>(**)] [%$ %noun !>(**)]]
    ==
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~  %give  %made  ~1234.5.6  %complete
            %result  %pin  ~1234.5.6  %result
            [%result %$ %noun !>(**)]
            [%result %$ %noun !>(**)]
    ==  ==
  ::
  %-  expect-eq  !>
  :-  state-by-ship.+>+<.ford
  (my [~nul *ford-state:ford-turbo]~)
::
++  test-autocons-different
  ~&  %test-autocons-different
  ::
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-is-forbidden)
  ::
  =^  moves  ford
    %-  call:ford
    :*  duct=~  type=~  %make  ~nul
        [%pin ~1234.5.6 [%$ %noun !>(42)] [%$ %noun !>(43)]]
    ==
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~  %give  %made  ~1234.5.6  %complete
            %result  %pin  ~1234.5.6  %result
            [%result %$ %noun !>(42)]
            [%result %$ %noun !>(43)]
    ==  ==
  ::
  %-  expect-eq  !>
  :-  state-by-ship.+>+<.ford
  (my [~nul *ford-state:ford-turbo]~)
::
++  test-scry-clay-succeed
  ~&  %test-scry-clay-succeed
  ::
  =/  scry  (scry-succeed ~1234.5.6 [%noun !>(42)])
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry)
  ::
  =^  moves  ford
    %-  call:ford
    :*  duct=~  type=~  %make  ~nul
        [%pin ~1234.5.6 [%scry %clay-live ren=%x rail=[[~nul %desk] /bar/foo]]]
    ==
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~  %give  %made  ~1234.5.6  %complete  %result
            [%pin ~1234.5.6 %result [%scry %noun !>(42)]]
    ==  ==
  ::
  %-  expect-eq  !>
  :-  state-by-ship.+>+<.ford
  (my [~nul *ford-state:ford-turbo]~)
::
++  test-scry-clay-fail
  ~&  %test-scry-clay-fail
  ::
  =/  scry  (scry-fail ~1234.5.6)
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry)
  ::
  =^  moves  ford
    %-  call:ford
    :*  duct=~  type=~  %make  ~nul
        [%pin ~1234.5.6 [%scry %clay-live ren=%x rail=[[~nul %desk] /bar/foo]]]
        ::
    ==
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~  %give  %made  ~1234.5.6  %complete
        %result  %pin  ~1234.5.6  %error
        :~  leaf+"clay-live scry failed for"
            leaf+"%cx /~nul/desk/~1234.5.6/foo/bar"
    ==  ==  ==
  ::
  %-  expect-eq  !>
  :-  state-by-ship.+>+<.ford
  (my [~nul *ford-state:ford-turbo]~)
::
++  test-scry-clay-block
  ~&  %test-scry-clay-block
  ::
  =/  scry-blocked  (scry-block ~1234.5.6)
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-blocked)
  ::
  =^  moves  ford
    %-  call:ford
    :*  duct=~  type=~  %make  ~nul
        [%pin ~1234.5.6 [%scry %clay-live ren=%x rail=[[~nul %desk] /bar/foo]]]
    ==
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~  %pass
            wire=/~nul/dependency/c/x/~nul/desk/0/foo/bar
            %c  %warp  [~nul ~nul]  %desk
            ~  %sing  %x  [%da ~1234.5.6]  /bar/foo
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.7 eny=0xbeef.dead scry=scry-is-forbidden)
  ::
  =^  moves2  ford
    %-  take:ford
    :*  wire=/~nul/dependency/c/x/~nul/desk/0/foo/bar  duct=~
        ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type  ::  ^-  sign:ford
        [%c %writ ~ [%x [%da ~1234.5.6] %desk] /bar/foo %noun !>(42)]
    ==
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves2
    :~  :*  duct=~  %give  %made  ~1234.5.6  %complete  %result
            [%pin ~1234.5.6 %result [%scry %noun !>(42)]]
    ==  ==
  ::
  %-  expect-eq  !>
  :-  state-by-ship.+>+<.ford
  (my [~nul *ford-state:ford-turbo]~)
::
++  test-scry-clay-live
  ~&  %test-scry-clay-live
  ::
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  =/  scry-43  (scry-succeed ~1234.5.7 [%noun !>(43)])
  ::
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-42)
  ::
  =^  moves  ford
    %-  call:ford
    :*  duct=~  type=~  %make  ~nul
        [%scry %clay-live care=%x rail=[[~nul %desk] /bar/foo]]
    ==
  ;:  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~  %give  %made  ~1234.5.6  %complete  %result
            [%scry %noun !>(42)]
        ==
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.6] (sy [%x /foo/bar]~)]
    ==  ==
  ::
    =.  ford  (ford now=~1234.5.7 eny=0xbeef.dead scry=scry-43)
    =^  moves2  ford
      %-  take:ford
      :*  wire=/~nul/clay-sub/~nul/desk  duct=~
          ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
          [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
      ==
    %-  expect-eq  !>
    :-  moves2
    :~  :*  duct=~  %give  %made  ~1234.5.7  %complete  %result
            [%scry %noun !>(43)]
        ==
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.7] (sy [%x /foo/bar]~)]
    ==  ==
  ::
    =.  ford  (ford now=~1234.5.8 eny=0xbeef.dead scry=scry-is-forbidden)
    =^  moves3  ford
      (call:ford [duct=~ type=~ %kill ~nul])
    ::
    %+  welp
      %-  expect-eq  !>
      :-  moves3
      :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
              %c  %warp  [~nul ~nul]  %desk  ~
      ==  ==
    ::
    %-  expect-eq  !>
    :-  state-by-ship.+>+<.ford
    (my [~nul *ford-state:ford-turbo]~)
  ==
++  test-pinned-in-live
  ~&  %test-pinned-in-live
  ::
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  =/  scry-43  (scry-succeed ~1234.5.7 [%noun !>(43)])
  ::
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-42)
  ::
  =/  schematic=schematic:ford
    :*  %same  %pin  ~1234.5.6
        [%scry %clay-live care=%x rail=[[~nul %desk] /bar/foo]]
    ==
  =/  build=build:ford  [~1234.5.6 schematic]
  =/  result=build-result:ford
    [%result %same %result %pin ~1234.5.6 %result [%scry %noun !>(42)]]
  ::
  =^  moves  ford  (call:ford [duct=~ type=~ %make ~nul schematic])
  ;:  welp
    %-  expect-eq  !>
    :-  moves
    [duct=~ %give %made ~1234.5.6 %complete result]~
  ::
    %-  expect-eq  !>
    :-  results:(~(got by state-by-ship.+>+<.ford) ~nul)
    %-  my  :~
      :-  [~1234.5.6 [%scry %clay-live care=%x rail=[[~nul %desk] /bar/foo]]]
      [%result ~1234.5.6 %result %scry %noun !>(42)]
    ::
      :-  :*  ~1234.5.6  %pin  ~1234.5.6
              [%scry %clay-live care=%x rail=[[~nul %desk] /bar/foo]]
          ==
      [%result ~1234.5.6 %result %pin ~1234.5.6 %result %scry %noun !>(42)]
    ::
      :-  :*  ~1234.5.6  %same  %pin  ~1234.5.6
              [%scry %clay-live care=%x rail=[[~nul %desk] /bar/foo]]
          ==
      :*  %result  ~1234.5.6  %result  %same  %result  %pin  ~1234.5.6
          %result  %scry  %noun  !>(42)
      ==
    ==
  ::
    =.  ford  (ford now=~1234.5.8 eny=0xbeef.dead scry=scry-is-forbidden)
    =^  moves2  ford
      (call:ford [duct=~ type=~ %kill ~nul])
    ::
    %+  welp
      %-  expect-eq  !>
      [moves2 ~]
    ::
    %-  expect-eq  !>
    :-  state-by-ship.+>+<.ford
    (my [~nul *ford-state:ford-turbo]~)
  ==
::
::  |utilities: helper arms
::
::+|  utilities
::
::  +scry-succeed: produces a scry function with a known request and answer
::
++  scry-succeed
  |=  [date=@da result=cage]  ^-  sley
  |=  [* (unit (set monk)) =term =beam]
  ^-  (unit (unit cage))
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
  ~|  scry-block+[beam+beam term+term]
  ?>  =(term %cx)
  ?>  =(beam [[~nul %desk %da date] /bar/foo])
  ::
  ~
::  +scry-is-forbidden: makes sure ford does not attempt to scry
::
++  scry-is-forbidden  ^-  sley
  |=  [* (unit (set monk)) =term =beam]
  ^-  (unit (unit cage))
  ::
  ~|  scry-is-forbidden+[beam+beam term+term]
  !!
--
