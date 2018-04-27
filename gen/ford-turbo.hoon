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
  test-scry-clay-live-again
  test-pinned-in-live
  test-live-build-that-blocks
  test-live-and-once
  test-live-two-deep
  test-live-three-deep
  test-live-triangle
  test-live-and-pinned-triangle
  test-slim
  test-ride
  test-ride-scry-succeed
  test-ride-scry-fail
  test-ride-scry-block
  test-ride-scry-promote
  test-five-oh-fora
  test-alts
  test-alts-and-live
  test-double-alts
==
++  test-is-schematic-live
  ~&  %test-is-schematic-live
  ::
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-is-forbidden)
  ::  pinned-schematic shows a once build
  ::
  =/  pinned-schematic=schematic:ford
    [%pin now [%scry [%c care=%x bel=[[~nul %desk] /foo/bar]]]]
  ::  live-schematic shows a live build
  ::
  =/  live-schematic=schematic:ford
    [%scry [%c care=%x bel=[[~nul %desk] /baz/doo]]]
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
    [%pin ~1234.5.6 [%scry [%c care=%x bel=[[~nul %desk] /foo/bar]]]]
  =/  nine-schematic=schematic:ford
    [%pin ~1234.5.9 [%scry [%c care=%x bel=[[~nul %desk] /baz/doo]]]]
  =/  three-schematic=schematic:ford
    [%pin ~1234.5.3 [%scry [%c care=%x bel=[[~nul %desk] /car/dor]]]]
  %+  welp
    %-  expect-eq  !>
    [~1234.5.9 (date-from-schematic:ford [three-schematic nine-schematic])]
  %-  expect-eq  !>
  :-  ~1234.5.9
  %-  date-from-schematic:ford
  [six-schematic [%alts ~[three-schematic nine-schematic]]]
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
    :-  `path`(dependency-to-path:ford [%c care=%x bel=[[~nul %desk] /foo/bar]])
    /cx/~nul/desk/0/bar/foo
  ::
    %-  expect-eq  !>
    :-  `dependency:ford`[%c care=%x bel=[[~nul %desk] /foo/bar]]
    (need (path-to-dependency:ford /cx/~nul/desk/0/bar/foo))
  ::
    %-  expect-eq  !>
    :-  ^-  path
      (dependency-to-path:ford [%g care=%x bel=[[~nul %desk] /foo/bar]])
    /gx/~nul/desk/0/bar/foo
  ::
    %-  expect-eq  !>
    :-  ^-  dependency:ford
      [%g care=%x bel=[[~nul %desk] /foo/bar]]
    (need (path-to-dependency:ford /gx/~nul/desk/0/bar/foo))
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
  (expect-ford-empty ford ~nul)
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
  (expect-ford-empty ford ~nul)
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
  (expect-ford-empty ford ~nul)
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
        [%pin ~1234.5.6 [%scry %c ren=%x rail=[[~nul %desk] /bar/foo]]]
    ==
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~  %give  %made  ~1234.5.6  %complete  %result
            [%pin ~1234.5.6 %result [%scry %noun !>(42)]]
    ==  ==
  ::
  (expect-ford-empty ford ~nul)
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
        [%pin ~1234.5.6 [%scry %c ren=%x rail=[[~nul %desk] /bar/foo]]]
        ::
    ==
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~  %give  %made  ~1234.5.6  %complete
        %result  %pin  ~1234.5.6  %error
        :~  leaf+"scry failed for"
            leaf+"%cx /~nul/desk/~1234.5.6/foo/bar"
    ==  ==  ==
  ::
  (expect-ford-empty ford ~nul)
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
        [%pin ~1234.5.6 [%scry %c ren=%x rail=[[~nul %desk] /bar/foo]]]
    ==
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~  %pass
            wire=/~nul/dependency/cx/~nul/desk/0/foo/bar
            %c  %warp  [~nul ~nul]  %desk
            ~  %sing  %x  [%da ~1234.5.6]  /bar/foo
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.7 eny=0xbeef.dead scry=scry-is-forbidden)
  ::
  =^  moves2  ford
    %-  take:ford
    :*  wire=/~nul/dependency/cx/~nul/desk/0/foo/bar  duct=~
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
  (expect-ford-empty ford ~nul)
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
        [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
    ==
  %+  welp
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
  %+  welp
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
  (expect-ford-empty ford ~nul)
::
++  test-scry-clay-live-again
  ~&  %test-scry-clay-live-again
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  ::
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-42)
  ::
  =^  moves  ford
    %-  call:ford
    :*  duct=~[/first]  type=~  %make  ~nul
        [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
    ==
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~[/first]  %give  %made  ~1234.5.6  %complete  %result
            [%scry %noun !>(42)]
        ==
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.6] (sy [%x /foo/bar]~)]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.7 eny=0xdead.beef scry=scry-is-forbidden)
  ::
  =^  moves2  ford
    %-  call:ford
    :*  duct=~[/second]  type=~  %make  ~nul
        [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
    ==
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves2
    :~  :*  duct=~[/second]  %give  %made  ~1234.5.7  %complete  %result
            [%scry %noun !>(42)]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.8 eny=0xdead.beef scry=scry-is-forbidden)
  ::
  =^  moves3  ford  (call:ford [duct=~[/first] type=~ %kill ~nul])
  ::
  %+  welp
    %-  expect-eq  !>
    [moves3 ~]
  ::
  =^  moves4  ford  (call:ford [duct=~[/second] type=~ %kill ~nul])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves4
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==
  ::
  (expect-ford-empty ford ~nul)
::
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
        [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
    ==
  =/  build=build:ford  [~1234.5.6 schematic]
  =/  result=build-result:ford
    [%result %same %result %pin ~1234.5.6 %result [%scry %noun !>(42)]]
  ::
  =^  moves  ford  (call:ford [duct=~ type=~ %make ~nul schematic])
  %+  welp
    %-  expect-eq  !>
    :-  moves
    [duct=~ %give %made ~1234.5.6 %complete result]~
  ::
  %+  welp
    %-  expect-eq  !>
    :-  results:(~(got by state-by-ship.+>+<.ford) ~nul)
    %-  my  :~
      :-  [~1234.5.6 [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]]
      [%result ~1234.5.6 %result %scry %noun !>(42)]
    ::
      :-  :*  ~1234.5.6  %pin  ~1234.5.6
              [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
          ==
      [%result ~1234.5.6 %result %pin ~1234.5.6 %result %scry %noun !>(42)]
    ::
      :-  :*  ~1234.5.6  %same  %pin  ~1234.5.6
              [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
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
  (expect-ford-empty ford ~nul)
::
++  test-live-build-that-blocks
  ~&  %test-live-build-that-blocks
  ::
  =/  scry-blocked  (scry-block ~1234.5.6)
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  =/  scry-43  (scry-succeed ~1234.5.7 [%noun !>(43)])
  ::
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-blocked)
  ::
  =^  moves  ford
    %-  call:ford
    :*  duct=~  type=~  %make  ~nul
        [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
    ==
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~  %pass
            wire=/~nul/dependency/cx/~nul/desk/0/foo/bar
            %c  %warp  [~nul ~nul]  %desk
            ~  %sing  %x  [%da ~1234.5.6]  /bar/foo
        ==
    ::
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
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves2
    :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.7] (sy [%x /foo/bar]~)]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.8 eny=0xbeef.dead scry=scry-42)
  =^  moves3  ford
    %-  take:ford
    :*  wire=/~nul/dependency/cx/~nul/desk/0/foo/bar  duct=~
        ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type  ::  ^-  sign:ford
        [%c %writ ~ [%x [%da ~1234.5.6] %desk] /bar/foo %noun !>(42)]
    ==
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves3
    :~  :*  duct=~  %give  %made  ~1234.5.6  %complete  %result
            [%scry %noun !>(42)]
        ==
        :*  duct=~  %give  %made  ~1234.5.7  %complete  %result
            [%scry %noun !>(43)]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.9 eny=0xbeef.dead scry=scry-is-forbidden)
  =^  moves4  ford
    (call:ford [duct=~ type=~ %kill ~nul])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves4
    :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==
  ::
  (expect-ford-empty ford ~nul)
::
++  test-live-and-once
  ~&  %test-live-and-once
  ::
  =/  scry-blocked  (scry-block ~1234.5.6)
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  ::
  ::
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-blocked)
  ::
  =^  moves  ford
    %-  call:ford
    :*  duct=~[/live]  type=~  %make  ~nul
        [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
    ==
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~  %pass
            wire=/~nul/dependency/cx/~nul/desk/0/foo/bar
            %c  %warp  [~nul ~nul]  %desk
            ~  %sing  %x  [%da ~1234.5.6]  /bar/foo
        ==
    ::
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.6] (sy [%x /foo/bar]~)]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.7 eny=0xbeef.dead scry=scry-blocked)
  =^  moves  ford
    %-  call:ford
    :*  duct=~[/once]  type=~  %make  ~nul
        [%pin ~1234.5.6 [%scry %c ren=%x rail=[[~nul %desk] /bar/foo]]]
    ==
  ::
  %+  welp
    %-  expect-eq  !>
    [moves ~]
  ::
  =.  ford  (ford now=~1234.5.8 eny=0xbeef.dead scry=scry-is-forbidden)
  =^  moves2  ford
  %-  take:ford
  :*  wire=/~nul/dependency/cx/~nul/desk/0/foo/bar  duct=~
      ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type  ::  ^-  sign:ford
      [%c %writ ~ [%x [%da ~1234.5.6] %desk] /bar/foo %noun !>(42)]
  ==
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves2
    :~  :*  duct=~[/live]  %give  %made  ~1234.5.6  %complete
            [%result [%scry %noun !>(42)]]
        ==
        :*  duct=~[/once]  %give  %made  ~1234.5.6  %complete
            [%result [%pin ~1234.5.6 %result [%scry %noun !>(42)]]]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.9 eny=0xbeef.dead scry=scry-is-forbidden)
  =^  moves4  ford
    (call:ford [duct=~[/live] type=~ %kill ~nul])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves4
    :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==
  ::
  (expect-ford-empty ford ~nul)
::
++  test-live-two-deep
  ~&  %test-live-two-deep
  ::
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  =/  scry-43  (scry-succeed ~1234.5.7 [%noun !>(43)])
  ::
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-42)
  ::
  =^  moves  ford
    %-  call:ford
    :*  duct=~  type=~  %make  ~nul
        [%same [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]]
    ==
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~  %give  %made  ~1234.5.6  %complete  %result
            %same  %result  [%scry %noun !>(42)]
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
  %+  welp
    %-  expect-eq  !>
    :-  moves2
    :~  :*  duct=~  %give  %made  ~1234.5.7  %complete  %result
            %same  %result  [%scry %noun !>(43)]
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
  (expect-ford-empty ford ~nul)
::
++  test-live-three-deep
  ~&  %test-live-three-deep
  ::
  =/  scry-type=type  [%atom %tas ~]
  ::
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %desk %da ~1234.5.6] /bar/foo]]
      [%noun scry-type %it-doesnt-matter]
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.7] /bar/foo]]
      [%noun scry-type %changed]
    ==
  ::
  =/  scry  (scry-with-results scry-results)
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry)
  ::
  =/  formula=hoon  (ream '`@tas`%constant')
  =/  subject-schematic=schematic:ford  [%scry %c %x [~nul %desk] /bar/foo]
  ::
  =/  ride=schematic:ford  [%ride formula subject-schematic]
  =/  same=schematic:ford  [%same ride]
  ::
  =^  moves  ford  (call:ford [duct=~[/ride] type=~ %make ~nul same])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~[/ride]  %give  %made  ~1234.5.6  %complete
            [%result [%same [%result [%ride scry-type %constant]]]]
        ==
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.6] (sy [%x /foo/bar] ~)]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.7 eny=0xbeef.dead scry=scry)
  ::
  =^  moves2  ford
    %-  take:ford
    :*  wire=/~nul/clay-sub/~nul/desk  duct=~
        ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
        [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
    ==
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves2
    :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.7] (sy [%x /foo/bar] ~)]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.8 eny=0xbeef.dead scry=scry-is-forbidden)
  =^  moves3  ford
    (call:ford [duct=~[/ride] type=~ %kill ~nul])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves3
    :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==
  ::
  (expect-ford-empty ford ~nul)
::
++  test-live-triangle
  ~&  %test-live-triangle
  ::
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %desk %da ~1234.5.6] /bar/foo]]
      [%noun !>(%it-does-in-fact-matter)]
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.7] /bar/foo]]
      [%noun !>(%changed)]
    ==
  ::
  =/  scry  (scry-with-results scry-results)
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry)
  ::
  =/  formula=hoon  (ream '`@tas`%constant')
  =/  subject-schematic=schematic:ford  [%scry %c %x [~nul %desk] /bar/foo]
  ::
  =/  ride-type=type  [%atom %tas ~]
  =/  ride=schematic:ford  [%ride formula subject-schematic]
  =/  autocons=schematic:ford  [ride subject-schematic]
  ::
  =^  moves  ford  (call:ford [duct=~[/ride] type=~ %make ~nul autocons])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~[/ride]  %give  %made  ~1234.5.6  %complete  %result
            [%result [%ride ride-type %constant]]
            [%result [%scry %noun !>(%it-does-in-fact-matter)]]
        ==
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.6] (sy [%x /foo/bar] ~)]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.7 eny=0xbeef.dead scry=scry)
  ::
  =^  moves2  ford
    %-  take:ford
    :*  wire=/~nul/clay-sub/~nul/desk  duct=~
        ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
        [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
    ==
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves2
    :~  :*  duct=~[/ride]  %give  %made  ~1234.5.7  %complete  %result
            [%result [%ride ride-type %constant]]
            [%result [%scry %noun !>(%changed)]]
        ==
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.7] (sy [%x /foo/bar] ~)]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.8 eny=0xbeef.dead scry=scry-is-forbidden)
  =^  moves3  ford
    (call:ford [duct=~[/ride] type=~ %kill ~nul])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves3
    :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==
  ::
  (expect-ford-empty ford ~nul)
::  like +test-live-triangle, but with another pinned build
::
::    Ensures that we deal with various issues with live builds which
::    were partially pinned and their interaction with other live builds.
::
++  test-live-and-pinned-triangle
  ~&  %test-live-and-pinned-triangle
  ::
  =/  scry-type=type  [%atom %tas ~]
  ::
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %desk %da ~1234.5.6] /bar/foo]]
      [%noun scry-type %it-does-in-fact-matter]
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.7] /bar/foo]]
      [%noun scry-type %it-does-in-fact-matter]
    ==
  ::
  =/  scry  (scry-with-results scry-results)
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry)
  ::
  =/  formula=hoon  (ream '`@tas`%constant')
  =/  subject-schematic=schematic:ford  [%scry %c %x [~nul %desk] /bar/foo]
  ::
  =/  ride-type=type  [%atom %tas ~]
  =/  ride=schematic:ford  [%ride formula subject-schematic]
  =/  autocons=schematic:ford  [ride subject-schematic]
  ::
  =/  static=schematic:ford  [%same [%pin ~1234.5.6 autocons]]
  ::
  =^  moves  ford  (call:ford [duct=~[/static] type=~ %make ~nul static])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~[/static]  %give  %made  ~1234.5.6  %complete  %result
            %same  %result  %pin  ~1234.5.6  %result
            [%result [%ride ride-type %constant]]
            [%result [%scry %noun scry-type %it-does-in-fact-matter]]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.7 eny=0xbeef.dead scry=scry)
  ::
  =^  moves2  ford  (call:ford [duct=~[/autocons] type=~ %make ~nul autocons])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves2
    :~  :*  duct=~[/autocons]  %give  %made  ~1234.5.7  %complete  %result
            [%result [%ride ride-type %constant]]
            [%result [%scry %noun scry-type %it-does-in-fact-matter]]
        ==
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.7] (sy [%x /foo/bar] ~)]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.8 eny=0xbeef.dead scry=scry-is-forbidden)
  ::
  =^  moves3  ford  (call:ford [duct=~[/static] type=~ %kill ~nul])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves3
    ~
  ::
  =.  ford  (ford now=~1234.5.8 eny=0xbeef.dead scry=scry-is-forbidden)
  ::
  =^  moves4  ford  (call:ford [duct=~[/autocons] type=~ %kill ~nul])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves4
    :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==
  ::
  (expect-ford-empty ford ~nul)
::
++  test-slim
  ~&  %test-slim
  ::
  =/  formula=hoon  (ream '(add 2 2)')
  =/  subject-type=type  -:!>(.)
  ::
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-is-forbidden)
  ::
  =^  moves  ford
    %-  call:ford
    :*  duct=~[/dead]  type=~  %make  ~nul
        [%slim subject-type formula]
    ==
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~[/dead]  %give  %made  ~1234.5.6  %complete
            [%result [%slim (~(mint ut subject-type) [%noun formula])]]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.7 eny=0xbeef.dead scry=scry-is-forbidden)
  =^  moves2  ford
    (call:ford [duct=~[/dead] type=~ %kill ~nul])
  ::
  %+  welp
    %-  expect-eq  !>
    [moves2 ~]
  ::
  (expect-ford-empty ford ~nul)
::
++  test-ride
  ~&  %test-ride
  ::
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-is-forbidden)
  ::
  =/  fun  |=(a=@ (add 2 a))
  =/  formula=hoon  (ream '!:  (fun 3)')
  =/  subject-schematic=schematic:ford  [%$ %noun !>(.)]
  ::
  =^  moves  ford
    %-  call:ford
    :*  duct=~[/dead]  type=~  %make  ~nul
        [%ride formula subject-schematic]
    ==
  ::
  ?>  =(1 (lent moves))
  ?>  ?=(^ moves)
  ?>  ?=([* %give %made @da %complete %result %ride *] i.moves)
  ::
  %+  welp
    %-  expect-eq  !>
    ::  compare the move to the expected move, omitting type checking on vase
    ::
    ::    Types can't be compared using simple equality, so normalize the type
    ::    to check the rest of the move.
    ::
    :-  i.moves(&8 *type)
    :*  duct=~[/dead]  %give  %made  ~1234.5.6  %complete
        [%result [%ride *type 5]]
    ==
  ::
  %+  welp
    %-  expect-eq  !>
    :-  (~(nest ut &8:i.moves) | -:!>(*@))
    &
  ::
  =.  ford  (ford now=~1234.5.7 eny=0xbeef.dead scry=scry-is-forbidden)
  =^  moves2  ford
    (call:ford [duct=~[/dead] type=~ %kill ~nul])
  ::
  %+  welp
    %-  expect-eq  !>
    [moves2 ~]
  ::
  (expect-ford-empty ford ~nul)
::
++  test-ride-scry-succeed
  ~&  %test-ride-scry-succeed
  ::
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-42)
  ::
  =/  formula=hoon  (ream '!:  .^(* %cx /~nul/desk/~1234.5.6/foo/bar)')
  =/  subject-schematic=schematic:ford  [%$ %noun !>(.)]
  ::
  =^  moves  ford
    %-  call:ford
    :*  duct=~[/dead]  type=~  %make  ~nul
        [%ride formula subject-schematic]
    ==
  ::
  ?>  =(1 (lent moves))
  ?>  ?=(^ moves)
  ?>  ?=([* %give %made @da %complete %result %ride *] i.moves)
  ::
  %+  welp
    %-  expect-eq  !>
    ::  compare the move to the expected move, omitting type checking on vase
    ::
    ::    Types can't be compared using simple equality, so normalize the type
    ::    to check the rest of the move.
    ::
    :-  i.moves(&8 *type)
    :*  duct=~[/dead]  %give  %made  ~1234.5.6  %complete
        [%result [%ride *type 42]]
    ==
  ::
  %+  welp
    %-  expect-eq  !>
    :-  (~(nest ut &8:i.moves) | -:!>(*@))
    &
  ::
  =.  ford  (ford now=~1234.5.7 eny=0xbeef.dead scry=scry-is-forbidden)
  =^  moves2  ford
    (call:ford [duct=~[/dead] type=~ %kill ~nul])
  ::
  %+  welp
    %-  expect-eq  !>
    [moves2 ~]
  ::
  (expect-ford-empty ford ~nul)
::
++  test-ride-scry-fail
  ~&  %test-ride-scry-fail
  ::
  =/  scry-failed  (scry-fail ~1234.5.6)
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-failed)
  ::
  =/  formula=hoon  (ream '!:  .^(* %cx /~nul/desk/~1234.5.6/foo/bar)')
  =/  subject-schematic=schematic:ford  [%$ %noun !>(.)]
  ::
  =^  moves  ford
    %-  call:ford
    :*  duct=~[/dead]  type=~  %make  ~nul
        [%ride formula subject-schematic]
    ==
  ::
  ?>  =(1 (lent moves))
  ?>  ?=(^ moves)
  ?>  ?=([* %give %made @da %complete %error *] i.moves)
  ::
  %+  welp
    %-  expect-eq  !>
    ::  compare the move to the expected move, omitting check on stack trace
    ::
    :-  i.moves(|7 ~)
    :*  duct=~[/dead]  %give  %made  ~1234.5.6  %complete
        [%error [leaf+"ford: %ride failed:" ~]]
    ==
  ::
  =.  ford  (ford now=~1234.5.7 eny=0xbeef.dead scry=scry-is-forbidden)
  =^  moves2  ford
    (call:ford [duct=~[/dead] type=~ %kill ~nul])
  ::
  %+  welp
    %-  expect-eq  !>
    [moves2 ~]
  ::
  (expect-ford-empty ford ~nul)
::
++  test-ride-scry-block
  ~&  %test-ride-scry-block
  ::
  =/  scry-blocked  (scry-block ~1234.5.6)
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-blocked)
  ::
  =/  formula=hoon  (ream '!:  .^(* %cx /~nul/desk/~1234.5.6/foo/bar)')
  =/  subject-schematic=schematic:ford  [%$ %noun !>(.)]
  ::
  =^  moves  ford
    %-  call:ford
    :*  duct=~[/live]  type=~  %make  ~nul
        [%ride formula subject-schematic]
    ==
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~  %pass
            wire=/~nul/dependency/cx/~nul/desk/0/foo/bar
            %c  %warp  [~nul ~nul]  %desk
            ~  %sing  %x  [%da ~1234.5.6]  /bar/foo
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.7 eny=0xbeef.dead scry=scry-blocked)
  ::
  =^  moves2  ford
    %-  take:ford
    :*  wire=/~nul/dependency/cx/~nul/desk/0/foo/bar  duct=~
        ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type  ::  ^-  sign:ford
        [%c %writ ~ [%x [%da ~1234.5.6] %desk] /bar/foo %noun !>(42)]
    ==
  ::
  ?>  =(1 (lent moves2))
  ?>  ?=(^ moves2)
  ?>  ?=([* %give %made @da %complete %result %ride *] i.moves2)
  ::
  %+  welp
    %-  expect-eq  !>
    ::  compare the move to the expected move, omitting type checking on vase
    ::
    ::    Types can't be compared using simple equality, so normalize the type
    ::    to check the rest of the move.
    ::
    :-  i.moves2(&8 *type)
    :*  duct=~[/live]  %give  %made  ~1234.5.6  %complete
        [%result [%ride *type 42]]
    ==
  ::
  %+  welp
    %-  expect-eq  !>
    :-  (~(nest ut &8:i.moves2) | -:!>(*@))
    &
  ::
  =.  ford  (ford now=~1234.5.8 eny=0xbeef.dead scry=scry-is-forbidden)
  =^  moves3  ford
    (call:ford [duct=~[/live] type=~ %kill ~nul])
  ::
  %+  welp
    %-  expect-eq  !>
    [moves3 ~]
  ::
  (expect-ford-empty ford ~nul)
::
++  test-ride-scry-promote
  ~&  %test-ride-scry-promote
  ::
  =/  scry-type=type  [%atom %tas ~]
  ::
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %desk %da ~1234.5.6] /bar/foo]]
      [%noun scry-type %it-doesnt-matter]
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.7] /bar/foo]]
      [%noun scry-type %changed]
    ==
  ::
  =/  scry  (scry-with-results scry-results)
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry)
  ::
  =/  formula=hoon  (ream '`@tas`%constant')
  =/  subject-schematic=schematic:ford  [%scry %c %x [~nul %desk] /bar/foo]
  ::
  =/  ride=schematic:ford  [%ride formula subject-schematic]
  ::
  =^  moves  ford  (call:ford [duct=~[/ride] type=~ %make ~nul ride])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~[/ride]  %give  %made  ~1234.5.6  %complete
            [%result [%ride scry-type %constant]]
        ==
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.6] (sy [%x /foo/bar] ~)]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.7 eny=0xbeef.dead scry=scry)
  ::
  =^  moves2  ford
    %-  take:ford
    :*  wire=/~nul/clay-sub/~nul/desk  duct=~
        ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
        [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
    ==
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves2
    :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.7] (sy [%x /foo/bar] ~)]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.8 eny=0xbeef.dead scry=scry-is-forbidden)
  =^  moves3  ford
    (call:ford [duct=~[/ride] type=~ %kill ~nul])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves3
    :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==
  ::
  (expect-ford-empty ford ~nul)
::
++  test-five-oh-fora
  ~&  %test-five-oh-fora
  ::
  =/  scry-type=type
    [%cell [%face [~ %title] [%atom %tas ~]] [%face [~ %contents] -:!>("")]]
  ::
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %desk %da ~1234.5.6] /a/posts]]
      [%noun scry-type [title='post-a' contents="post-a-contents"]]
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.6] /b/posts]]
      [%noun scry-type [title='post-b' contents="post-b-contents"]]
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.8] /a/posts]]
      [%noun scry-type [title='post-a' contents="post-a-contents-changed"]]
    ==
  ::
  =/  scry  (scry-with-results scry-results)
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry)
  ::
  =/  post-a=schematic:ford  [%scry [%c %x [~nul %desk] /a/posts]]
  =/  title-a=schematic:ford  [%ride (ream '!:  title') post-a]
  ::
  =/  post-b=schematic:ford  [%scry [%c %x [~nul %desk] /b/posts]]
  =/  title-b=schematic:ford  [%ride (ream '!:  title') post-b]
  ::
  =/  sidebar=schematic:ford  [title-a title-b]
  ::
  =/  rendered-a=schematic:ford  [post-a sidebar]
  =/  rendered-b=schematic:ford  [post-b sidebar]
  ::  first, ask ford to build rendered-a
  ::
  =^  moves  ford  (call:ford [duct=~[/post-a] type=~ %make ~nul rendered-a])
  ::
  ?>  ?=([^ ^ ~] moves)
  %+  welp
    %-  check-post-made  :*
      move=i.moves
      duct=~[/post-a]
      type=scry-type
      date=~1234.5.6
      title='post-a'
      contents="post-a-contents"
    ==
  %+  welp
    %-  expect-eq  !>
    :-  i.t.moves
    :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
        %c  %warp  [~nul ~nul]  %desk
        `[%mult [%da ~1234.5.6] (sy [%x /posts/a] [%x /posts/b] ~)]
    ==
  ::
  =.  ford  (ford now=~1234.5.7 eny=0xbeef.dead scry=scry)
  ::
  =^  moves2  ford  (call:ford [duct=~[/post-b] type=~ %make ~nul rendered-b])
  ::
  ?>  ?=([^ ~] moves2)
  %+  welp
    %-  check-post-made  :*
      move=i.moves2
      duct=~[/post-b]
      type=scry-type
      date=~1234.5.7
      title='post-b'
      contents="post-b-contents"
    ==
  ::
  =.  ford  (ford now=~1234.5.8 eny=0xbeef.dead scry=scry)
  ::
  =^  moves3  ford
    %-  take:ford
    :*  wire=/~nul/clay-sub/~nul/desk  duct=~
        ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
        [%c %wris [%da ~1234.5.8] (sy [%x /posts/a]~)]
    ==
  ::
  ?>  ?=([^ ^ ~] moves3)
  %+  welp
    %-  check-post-made  :*
      move=i.moves3
      duct=~[/post-a]
      type=scry-type
      date=~1234.5.8
      title='post-a'
      contents="post-a-contents-changed"
    ==
  ::
  =.  ford  (ford now=~1234.5.9 eny=0xbeef.dead scry=scry-is-forbidden)
  ::
  =^  moves4  ford  (call:ford [duct=~[/post-b] type=~ %kill ~nul])
  ::
  %+  welp
    %-  expect-eq  !>
    [moves4 ~]
  ::
  =.  ford  (ford now=~1234.5.10 eny=0xbeef.dead scry=scry-is-forbidden)
  ::
  =^  moves5  ford  (call:ford [duct=~[/post-a] type=~ %kill ~nul])
  ::
  =/  state  (~(got by state-by-ship.+>+<.ford) ~nul)
  %+  welp
    %-  expect-eq  !>
    :-  moves5
    :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==
  ::
  (expect-ford-empty ford ~nul)
::
++  test-alts
  ~&  %test-alts
  ::
  =/  scry-type=type  [%atom %tas ~]
  ::
  =/  scry-results=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %first %da ~1234.5.6] /one/scry]]
      ~
    ::
      :-  [%cx [[~nul %second %da ~1234.5.6] /two/scry]]
      ~
    ::
      :-  [%cx [[~nul %first %da ~1234.5.7] /one/scry]]
      ~
    ::
      :-  [%cx [[~nul %second %da ~1234.5.7] /two/scry]]
      `[%noun scry-type 'scry-two']
    ::
      :-  [%cx [[~nul %first %da ~1234.5.8] /one/scry]]
      `[%noun scry-type 'scry-one']
    ==
  ::
  =/  scry  (scry-with-results-and-failures scry-results)
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry)
  ::
  =/  scry1=schematic:ford  [%scry [%c %x [~nul %first] /one/scry]]
  =/  scry2=schematic:ford  [%scry [%c %x [~nul %second] /two/scry]]
  =/  alts=schematic:ford   [%alts [scry1 scry2 ~]]
  ::
  =^  moves  ford  (call:ford [duct=~[/alts] type=~ %make ~nul alts])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~[/alts]  %give  %made  ~1234.5.6  %complete
            [%error [%leaf "%alts: all options failed"]~]
        ==
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/first
            %c  %warp  [~nul ~nul]  %first
            `[%mult [%da ~1234.5.6] (sy [%x /scry/one] ~)]
        ==
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/second
            %c  %warp  [~nul ~nul]  %second
            `[%mult [%da ~1234.5.6] (sy [%x /scry/two] ~)]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.7 eny=0xdead.beef scry=scry)
  ::
  =^  moves2  ford
    %-  take:ford
    :*  wire=/~nul/clay-sub/~nul/second  duct=~
        ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
        [%c %wris [%da ~1234.5.7] (sy [%x /scry/two]~)]
    ==
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves2
    :~  :*  duct=~[/alts]  %give  %made  ~1234.5.7  %complete
            %result  %alts  %result  %scry  %noun  scry-type  'scry-two'
        ==
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/second
            %c  %warp  [~nul ~nul]  %second
            `[%mult [%da ~1234.5.7] (sy [%x /scry/two] ~)]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.8 eny=0xdead.beef scry=scry)
  ::
  =^  moves3  ford
    %-  take:ford
    :*  wire=/~nul/clay-sub/~nul/first  duct=~
        ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
        [%c %wris [%da ~1234.5.8] (sy [%x /scry/one]~)]
    ==
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves3
    :~  :*  duct=~[/alts]  %give  %made  ~1234.5.8  %complete
            %result  %alts  %result  %scry  %noun  scry-type  'scry-one'
        ==
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/first
            %c  %warp  [~nul ~nul]  %first
            `[%mult [%da ~1234.5.8] (sy [%x /scry/one] ~)]
        ==
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/second
            %c  %warp  [~nul ~nul]  %second  ~
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.9 eny=0xbeef.dead scry=scry-is-forbidden)
  =^  moves4  ford
    (call:ford [duct=~[/alts] type=~ %kill ~nul])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves4
    :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/first
            %c  %warp  [~nul ~nul]  %first  ~
    ==  ==
  ::
  (expect-ford-empty ford ~nul)
::
++  test-alts-and-live
  ~&  %test-alts-and-live
  ::
  =/  scry-type=type  [%atom %tas ~]
  ::
  =/  scry-results=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %desk %da ~1234.5.6] /one/scry]]
      ~
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.6] /two/scry]]
      `[%noun scry-type 'scry-two']
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.7] /one/scry]]
      ~
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.7] /two/scry]]
      `[%noun scry-type 'scry-two']
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.8] /one/scry]]
      `[%noun scry-type 'scry-one']
    ==
  ::
  =/  scry  (scry-with-results-and-failures scry-results)
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry)
  ::
  =/  scry2=schematic:ford  [%scry [%c %x [~nul %desk] /two/scry]]
  =/  same=schematic:ford   [%same scry2]
  ::  depend on scry2 for the duration of the test
  ::
  =^  moves  ford  (call:ford [duct=~[/same] type=~ %make ~nul same])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~[/same]  %give  %made  ~1234.5.6  %complete
            %result  %same  %result  %scry  %noun  scry-type  'scry-two'
        ==
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.6] (sy [%x /scry/two] ~)]
    ==  ==
  ::
  =/  scry1=schematic:ford  [%scry [%c %x [~nul %desk] /one/scry]]
  =/  alts=schematic:ford   [%alts [scry1 scry2 ~]]
  ::
  =.  ford  (ford now=~1234.5.7 eny=0xdead.beef scry=scry)
  ::  call the alts schematic
  ::
  ::    The alts schematic should fail to read /scry/one, and should fallback
  ::    to /scry/two.
  ::
  =^  moves2  ford  (call:ford [duct=~[/alts] type=~ %make ~nul alts])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves2
    :~  :*  duct=~[/alts]  %give  %made  ~1234.5.7  %complete
            %result  %alts  %result  %scry  %noun  scry-type  'scry-two'
        ==
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.7] (sy [%x /scry/two] [%x /scry/one] ~)]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.8 eny=0xdead.beef scry=scry)
  ::  tell ford that /scry/one exists now
  ::
  =^  moves3  ford
    %-  take:ford
    :*  wire=/~nul/clay-sub/~nul/desk  duct=~
        ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
        [%c %wris [%da ~1234.5.8] (sy [%x /scry/one]~)]
    ==
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves3
    :~  :*  duct=~[/alts]  %give  %made  ~1234.5.8  %complete
            %result  %alts  %result  %scry  %noun  scry-type  'scry-one'
        ==
        ::  we subscribe to both paths because /same still exists.
        ::
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.8] (sy [%x /scry/one] [%x /scry/two] ~)]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.9 eny=0xdead.beef scry=scry)
  ::  kill the /same build
  ::
  ::    We should no longer subscribe to /scry/two in the resulting clay
  ::    subscription.
  ::
  =^  moves4  ford  (call:ford [duct=~[/same] type=~ %kill ~nul])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves4
    :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.8] (sy [%x /scry/one] ~)]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.10 eny=0xdead.beef scry=scry)
  ::
  =^  moves5  ford  (call:ford [duct=~[/alts] type=~ %kill ~nul])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves5
    :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==
  ::
  (expect-ford-empty ford ~nul)
::
++  test-double-alts
  ~&  %test-double-alts
  ::
  =/  scry-type=type  [%atom %tas ~]
  ::
  =/  scry-results=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %desk %da ~1234.5.6] /one/scry]]
      ~
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.6] /two/scry]]
      `[%noun scry-type 'scry-two']
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.7] /three/scry]]
      ~
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.7] /two/scry]]
      `[%noun scry-type 'scry-two']
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.8] /three/scry]]
      `[%noun scry-type 'scry-three']
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.8] /two/scry]]
      `[%noun scry-type 'scry-two']
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.9] /one/scry]]
      `[%noun scry-type 'scry-one']
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.9] /two/scry]]
      `[%noun scry-type 'scry-two-changed']
    ==
  ::
  =/  scry  (scry-with-results-and-failures scry-results)
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry)
  ::
  =/  scry1=schematic:ford  [%scry [%c %x [~nul %desk] /one/scry]]
  =/  scry2=schematic:ford  [%scry [%c %x [~nul %desk] /two/scry]]
  =/  scry3=schematic:ford  [%scry [%c %x [~nul %desk] /three/scry]]
  =/  alts1=schematic:ford  [%alts [scry1 scry2 ~]]
  =/  alts2=schematic:ford  [%alts [scry3 scry2 ~]]
  ::  alts1 will depend on both scry1 and scry2
  ::
  =^  moves  ford  (call:ford [duct=~[/first] type=~ %make ~nul alts1])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves
    :~  :*  duct=~[/first]  %give  %made  ~1234.5.6  %complete
            %result  %alts  %result  %scry  %noun  scry-type  'scry-two'
        ==
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.6] (sy [%x /scry/one] [%x /scry/two] ~)]
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.7 eny=0xdead.beef scry=scry)
  ::  alts2 will depend on both scry3 and scry2
  ::
  =^  moves2  ford  (call:ford [duct=~[/second] type=~ %make ~nul alts2])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves2
    :~  :*  duct=~[/second]  %give  %made  ~1234.5.7  %complete
            %result  %alts  %result  %scry  %noun  scry-type  'scry-two'
        ==
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk  ~  %mult  [%da ~1234.5.7]
            (sy [%x /scry/one] [%x /scry/two] [%x /scry/three] ~)
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.8 eny=0xdead.beef scry=scry)
  ::  alts2 should now just return 'scry-three'
  ::
  =^  moves3  ford
    %-  take:ford
    :*  wire=/~nul/clay-sub/~nul/desk  duct=~
        ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
        [%c %wris [%da ~1234.5.8] (sy [%x /scry/three]~)]
    ==
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves3
    :~  :*  duct=~[/second]  %give  %made  ~1234.5.8  %complete
            %result  %alts  %result  %scry  %noun  scry-type  'scry-three'
        ==
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk  ~  %mult  [%da ~1234.5.8]
            (sy [%x /scry/one] [%x /scry/two] [%x /scry/three] ~)
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.9 eny=0xdead.beef scry=scry)
  ::  alts1 should now just return 'scry-one'
  ::
  =^  moves4  ford
    %-  take:ford
    :*  wire=/~nul/clay-sub/~nul/desk  duct=~
        ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
        [%c %wris [%da ~1234.5.9] (sy [%x /scry/one] [%x /scry/two] ~)]
    ==
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves4
    :~  :*  duct=~[/first]  %give  %made  ~1234.5.9  %complete
            %result  %alts  %result  %scry  %noun  scry-type  'scry-one'
        ==
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk  ~  %mult  [%da ~1234.5.9]
            (sy [%x /scry/one] [%x /scry/three] ~)
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.10 eny=0xdead.beef scry=scry)
  ::
  =^  moves5  ford  (call:ford [duct=~[/first] type=~ %kill ~nul])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves5
    :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk  ~  %mult  [%da ~1234.5.9]
            (sy [%x /scry/three] ~)
    ==  ==
  ::
  =.  ford  (ford now=~1234.5.11 eny=0xdead.beef scry=scry)
  ::
  =^  moves6  ford  (call:ford [duct=~[/second] type=~ %kill ~nul])
  ::
  %+  welp
    %-  expect-eq  !>
    :-  moves6
    :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==
  ::
  (expect-ford-empty ford ~nul)
::
::  |utilities: helper arms
::
::+|  utilities
++  check-post-made
  |=  $:  move=move:ford-turbo
          =duct
          =type
          date=@da
          title=@tas
          contents=tape
      ==
  ^-  wall
  ::
  ?>  ?=([* %give %made @da %complete %result ^ *] move)
  =/  result  result.p.card.move
  ?>  ?=([%result %scry %noun type-a=* @tas *] head.result)
  ?>  ?=([%result ^ *] tail.result)
  ?>  ?=([%result %ride type-title-a=* %post-a] head.tail.result)
  ?>  ?=([%result %ride type-title-b=* %post-b] tail.tail.result)
  ::
  ;:  welp
    %-  expect-eq  !>
    [duct.move duct]
  ::
    %-  expect-eq  !>
    [date.p.card.move date]
  ::
    %-  expect-eq  !>
    :-  head.result(p.q.cage *^type)
    [%result %scry %noun *^type [title=title contents=contents]]
  ::
    %-  expect-eq  !>
    :-  (~(nest ut p.q.cage.head.result) | type)
    &
  ::
    %-  expect-eq  !>
    :-  head.tail.result(p.vase *^type)
    [%result %ride *^type 'post-a']
  ::
    %-  expect-eq  !>
    :-  (~(nest ut p.vase.head.tail.result) | -:!>(''))
    &
  ::
    %-  expect-eq  !>
    :-  tail.tail.result(p.vase *^type)
    [%result %ride *^type 'post-b']
  ::
    %-  expect-eq  !>
    :-  (~(nest ut p.vase.tail.tail.result) | -:!>(''))
    &
  ==
::
::  +scry-with-results
++  scry-with-results
  |=  results=(map [=term =beam] cage)
  |=  [* (unit (set monk)) =term =beam]
  ^-  (unit (unit cage))
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
::  +expect-ford-empty: assert that ford's state is one empty ship
::
::    At the end of every test, we want to assert that we have cleaned up all
::    state.
::
++  expect-ford-empty
  |=  [ford=_(ford-turbo) ship=@p]
  ^-  wall
  %-  expect-eq  !>
  :-  state-by-ship.+>+<.ford
  (my [ship *ford-state:ford-turbo]~)
--
