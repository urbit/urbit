/+  ford-turbo, tester
::
:-  %say
|=  [[now=@da eny=@ =beak] ~ ~]
:-  %noun
=+  tester:tester
|^
=-  ((slog (flop -)) ~)
^-  tang
;:  weld
  test-is-schematic-live
  test-date-from-schematic
  test-unify-jugs
  test-resource-wire-encoding
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
  test-cache-reclamation-trivial
  test-cache-reclamation-live-rebuild
  test-cache-reclamation-live-promote
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
  ^-  tang
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
++  test-resource-wire-encoding
  ~&  %test-resource-wire-encoding
  ::
  =/  ford  (ford-turbo now=~1234.5.6 eny=0xdead.beef scry=scry-is-forbidden)
  ::
  ;:  welp
    %-  expect-eq  !>
    :-  `path`(resource-to-path:ford [%c care=%x bel=[[~nul %desk] /foo/bar]])
    /cx/~nul/desk/0/bar/foo
  ::
    %-  expect-eq  !>
    :-  `resource:ford`[%c care=%x bel=[[~nul %desk] /foo/bar]]
    (need (path-to-resource:ford /cx/~nul/desk/0/bar/foo))
  ::
    %-  expect-eq  !>
    :-  ^-  path
      (resource-to-path:ford [%g care=%x bel=[[~nul %desk] /foo/bar]])
    /gx/~nul/desk/0/bar/foo
  ::
    %-  expect-eq  !>
    :-  ^-  resource:ford
      [%g care=%x bel=[[~nul %desk] /foo/bar]]
    (need (path-to-resource:ford /gx/~nul/desk/0/bar/foo))
  ==
::
++  test-literal
  ~&  %test-literal
  ::
  =/  ford  *ford-turbo
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry-is-forbidden
      ::  send a pinned literal, expects a %made response with pinned literal
      ::
      ^=  call-args
        [duct=~ type=~ %make ~nul [%pin ~1234.5.6 [%$ %noun !>(**)]]]
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.6
                %complete  %success  %pin  ~1234.5.6  %success  %$  %noun  !>(**)
        ==  ==
    ==
  ::
  %+  welp
    results1
  (expect-ford-empty ford ~nul)
::
++  test-autocons-same
  ~&  %test-autocons-same
  ::
  =/  ford  *ford-turbo
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry-is-forbidden
      ::  if we autocons the same schematic, we should get two of it as a result
      ::
      ^=  call-args
        :*  duct=~  type=~  %make  ~nul
          [%pin ~1234.5.6 [%$ %noun !>(**)] [%$ %noun !>(**)]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.6  %complete
                %success  %pin  ~1234.5.6  %success
               [%success %$ %noun !>(**)]
               [%success %$ %noun !>(**)]
    ==  ==  ==
  ::
  %+  welp
    results1
  (expect-ford-empty ford ~nul)
::
++  test-autocons-different
  ~&  %test-autocons-different
  ::
  =/  ford  *ford-turbo
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry-is-forbidden
      :: if we autocons different schematics, we get different values
      ::
      ^=  call-args
        :*  duct=~  type=~  %make  ~nul
          [%pin ~1234.5.6 [%$ %noun !>(42)] [%$ %noun !>(43)]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.6  %complete
                %success  %pin  ~1234.5.6  %success
                [%success %$ %noun !>(42)]
                [%success %$ %noun !>(43)]
    ==  ==  ==
  ::
  %+  welp
    results1
    (expect-ford-empty ford ~nul)
::
++  test-scry-clay-succeed
  ~&  %test-scry-clay-succeed
  ::
  =/  ford  *ford-turbo
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=(scry-succeed ~1234.5.6 [%noun !>(42)])
      ::  test a pinned scry which succeeds
      ::
      ^=  call-args
        :*  duct=~  type=~  %make  ~nul
            [%pin ~1234.5.6 [%scry %c ren=%x rail=[[~nul %desk] /bar/foo]]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.6  %complete  %success
                [%pin ~1234.5.6 %success [%scry %noun !>(42)]]
    ==  ==  ==
  ::
  %+  welp
    results1
  (expect-ford-empty ford ~nul)
::
++  test-scry-clay-fail
  ~&  %test-scry-clay-fail
  ::
  =/  ford  *ford-turbo
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=(scry-fail ~1234.5.6)
      ::  attempting to scry a path which fails should produce an error
      ::
      ^=  call-args
        :*  duct=~  type=~  %make  ~nul
            [%pin ~1234.5.6 [%scry %c ren=%x rail=[[~nul %desk] /bar/foo]]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.6  %complete
                %success  %pin  ~1234.5.6  %error
                :~  leaf+"scry failed for"
                    leaf+"%cx /~nul/desk/~1234.5.6/foo/bar"
    ==  ==  ==  ==
  ::
  %+  weld
    results1
    (expect-ford-empty ford ~nul)
::
++  test-scry-clay-block
  ~&  %test-scry-clay-block
  ::
  =/  ford  *ford-turbo
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=(scry-block ~1234.5.6)
      ::  when we scry on a blocked path, expect a subscription move
      ::
      ^=  call-args
        :*  duct=~  type=~  %make  ~nul
            [%pin ~1234.5.6 [%scry %c ren=%x rail=[[~nul %desk] /bar/foo]]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %pass
                wire=/~nul/resource/cx/~nul/desk/0/foo/bar
                %c  %warp  [~nul ~nul]  %desk
                ~  %sing  %x  [%da ~1234.5.6]  /bar/foo
    ==  ==  ==
  ::
  =^  results2  ford
    %-  test-ford-take  :*
      ford
      now=~1234.5.7
      scry=scry-is-forbidden
      ::  when clay responds, send a %made
      ::
      ^=  take-args
        :*  wire=/~nul/resource/cx/~nul/desk/0/foo/bar  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type  ::  ^-  sign:ford
            [%c %writ ~ [%x [%da ~1234.5.6] %desk] /bar/foo %noun !>(42)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.6  %complete  %success
                [%pin ~1234.5.6 %success [%scry %noun !>(42)]]
    ==  ==  ==
  ::
  ;:  welp
    results1
    results2
    (expect-ford-empty ford ~nul)
  ==
::
++  test-scry-clay-live
  ~&  %test-scry-clay-live
  ::
  =/  ford  *ford-turbo
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=(scry-succeed ~1234.5.6 [%noun !>(42)])
      ::
      ^=  call-args
        :*  duct=~  type=~  %make  ~nul
            [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.6  %complete  %success
                [%scry %noun !>(42)]
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results2  ford
    %-  test-ford-take  :*
      ford
      now=~1234.5.7
      scry=(scry-succeed ~1234.5.7 [%noun !>(43)])
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.7  %complete  %success
                [%scry %noun !>(43)]
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results3  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~ type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    (expect-ford-empty ford ~nul)
  ==
::
++  test-scry-clay-live-again
  ~&  %test-scry-clay-live-again
  =/  ford  *ford-turbo
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=(scry-succeed ~1234.5.6 [%noun !>(42)])
      ::  perform a live scry, we should get a %made and a clay subscription
      ::
      ^=  call-args
        :*  duct=~[/first]  type=~  %make  ~nul
            [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/first]  %give  %made  ~1234.5.6  %complete  %success
                [%scry %noun !>(42)]
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results2  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.7
      scry=scry-is-forbidden
      ::  a second scry from a different duct shouldn't resubscribe
      ::
      ^=  call-args
        :*  duct=~[/second]  type=~  %make  ~nul
            [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/second]  %give  %made  ~1234.5.7  %complete  %success
                [%scry %noun !>(42)]
    ==  ==  ==
  ::
  =^  results3  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/first] type=~ %kill ~nul]
      moves=~
    ==
  ::
  =^  results4  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.9
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/second] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    (expect-ford-empty ford ~nul)
  ==
::
++  test-pinned-in-live
  ~&  %test-pinned-in-live
  ::
  =/  ford  *ford-turbo
  ::
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  =/  scry-43  (scry-succeed ~1234.5.7 [%noun !>(43)])
  ::
  =/  schematic=schematic:ford
    :*  %same  %pin  ~1234.5.6
        [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
    ==
  ::
  =/  build=build:ford  [~1234.5.6 schematic]
  =/  result=build-result:ford
    [%success %same %success %pin ~1234.5.6 %success [%scry %noun !>(42)]]
  ::
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry-42
      ::
      call-args=[duct=~ type=~ %make ~nul schematic]
      moves=[duct=~ %give %made ~1234.5.6 %complete result]~
    ==
  ::
  =/  results2=tang
    %-  expect-eq  !>
    :-  results:(~(got by state-by-ship.+>+<.ford) ~nul)
    %-  my  :~
      :-  [~1234.5.6 [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]]
      [%value ~1234.5.6 %success %scry %noun !>(42)]
    ::
      :-  :*  ~1234.5.6  %pin  ~1234.5.6
              [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
          ==
      [%value ~1234.5.6 %success %pin ~1234.5.6 %success %scry %noun !>(42)]
    ::
      :-  :*  ~1234.5.6  %same  %pin  ~1234.5.6
              [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
          ==
      :*  %value  ~1234.5.6  %success  %same  %success  %pin  ~1234.5.6
          %success  %scry  %noun  !>(42)
      ==
    ==
  ::
  =^  results3  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~ type=~ %kill ~nul]
      moves=~
    ==
  ::
  ;:  weld
    results1
    results2
    results3
    (expect-ford-empty ford ~nul)
  ==
::
++  test-live-build-that-blocks
  ~&  %test-live-build-that-blocks
  ::
  =/  scry-blocked  (scry-block ~1234.5.6)
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  =/  scry-43  (scry-succeed ~1234.5.7 [%noun !>(43)])
  ::
  =/  ford  *ford-turbo
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry-blocked
      ::
      ^=  call-args
        :*  duct=~  type=~  %make  ~nul
            [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %pass
                wire=/~nul/resource/cx/~nul/desk/0/foo/bar
                %c  %warp  [~nul ~nul]  %desk
                ~  %sing  %x  [%da ~1234.5.6]  /bar/foo
            ==
        ::
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results2  ford
    %-  test-ford-take  :*
      ford
      now=~1234.5.7
      scry=scry-43
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results3  ford
    %-  test-ford-take  :*
      ford
      now=~1234.5.8
      scry=scry-42
      ::
      ^=  take-args
        :*  wire=/~nul/resource/cx/~nul/desk/0/foo/bar  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type  ::  ^-  sign:ford
            [%c %writ ~ [%x [%da ~1234.5.6] %desk] /bar/foo %noun !>(42)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.6  %complete  %success
                [%scry %noun !>(42)]
            ==
            :*  duct=~  %give  %made  ~1234.5.7  %complete  %success
                [%scry %noun !>(43)]
    ==  ==  ==
  ::
  =^  results4  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.9
      scry=scry-is-forbidden
      ::
      call-args=[duct=~ type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    (expect-ford-empty ford ~nul)
  ==
::
++  test-live-and-once
  ~&  %test-live-and-once
  ::
  =/  scry-blocked  (scry-block ~1234.5.6)
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  ::
  ::
  =/  ford  *ford-turbo
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry-blocked
      ::
      ^=  call-args
        :*  duct=~[/live]  type=~  %make  ~nul
            [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %pass
                wire=/~nul/resource/cx/~nul/desk/0/foo/bar
                %c  %warp  [~nul ~nul]  %desk
                ~  %sing  %x  [%da ~1234.5.6]  /bar/foo
            ==
        ::
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results2  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.7
      scry=scry-blocked
      ::
      ^=  call-args
        :*  duct=~[/once]  type=~  %make  ~nul
            [%pin ~1234.5.6 [%scry %c ren=%x rail=[[~nul %desk] /bar/foo]]]
        ==
      ::
      moves=~
    ==
  ::
  =^  results3  ford
    %-  test-ford-take  :*
      ford
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      ^=  take-args
        :*  wire=/~nul/resource/cx/~nul/desk/0/foo/bar  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type  ::  ^-  sign:ford
            [%c %writ ~ [%x [%da ~1234.5.6] %desk] /bar/foo %noun !>(42)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/live]  %give  %made  ~1234.5.6  %complete
                [%success [%scry %noun !>(42)]]
            ==
            :*  duct=~[/once]  %give  %made  ~1234.5.6  %complete
                [%success [%pin ~1234.5.6 %success [%scry %noun !>(42)]]]
    ==  ==  ==
  ::
  =^  results4  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.9
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/live] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    (expect-ford-empty ford ~nul)
  ==
::
++  test-live-two-deep
  ~&  %test-live-two-deep
  ::
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  =/  scry-43  (scry-succeed ~1234.5.7 [%noun !>(43)])
  ::
  =/  ford  *ford-turbo
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry-42
      ::
      ^=  call-args
        :*  duct=~  type=~  %make  ~nul
            [%same [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.6  %complete  %success
                %same  %success  [%scry %noun !>(42)]
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results2  ford
    %-  test-ford-take  :*
      ford
      now=~1234.5.7
      scry=scry-43
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.7  %complete  %success
                %same  %success  [%scry %noun !>(43)]
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results3  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~ type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    (expect-ford-empty ford ~nul)
  ==
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
  =/  ford  *ford-turbo
  ::
  =/  formula=hoon  (ream '`@tas`%constant')
  =/  subject-schematic=schematic:ford  [%scry %c %x [~nul %desk] /bar/foo]
  ::
  =/  ride=schematic:ford  [%ride formula subject-schematic]
  =/  same=schematic:ford  [%same ride]
  ::
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/ride] type=~ %make ~nul same]
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %give  %made  ~1234.5.6  %complete
                [%success [%same [%success [%ride scry-type %constant]]]]
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results2  ford
    %-  test-ford-take  :*
      ford
      now=~1234.5.7
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results3  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/ride] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    (expect-ford-empty ford ~nul)
  ==
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
  =/  ford  *ford-turbo
  ::
  =/  formula=hoon  (ream '`@tas`%constant')
  =/  subject-schematic=schematic:ford  [%scry %c %x [~nul %desk] /bar/foo]
  ::
  =/  ride-type=type  [%atom %tas ~]
  =/  ride=schematic:ford  [%ride formula subject-schematic]
  =/  autocons=schematic:ford  [ride subject-schematic]
  ::
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/ride] type=~ %make ~nul autocons]
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %give  %made  ~1234.5.6  %complete  %success
                [%success [%ride ride-type %constant]]
                [%success [%scry %noun !>(%it-does-in-fact-matter)]]
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results2  ford
    %-  test-ford-take  :*
      ford
      now=~1234.5.7
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %give  %made  ~1234.5.7  %complete  %success
                [%success [%ride ride-type %constant]]
                [%success [%scry %noun !>(%changed)]]
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results3  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/ride] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    (expect-ford-empty ford ~nul)
  ==
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
  =/  ford  *ford-turbo
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
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/static] type=~ %make ~nul static]
      ::
      ^=  moves
        :~  :*  duct=~[/static]  %give  %made  ~1234.5.6  %complete  %success
                %same  %success  %pin  ~1234.5.6  %success
                [%success [%ride ride-type %constant]]
                [%success [%scry %noun scry-type %it-does-in-fact-matter]]
    ==  ==  ==
  ::
  =^  results2  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.7
      scry=scry
      ::
      call-args=[duct=~[/autocons] type=~ %make ~nul autocons]
      ::
      ^=  moves
        :~  :*  duct=~[/autocons]  %give  %made  ~1234.5.7  %complete  %success
                [%success [%ride ride-type %constant]]
                [%success [%scry %noun scry-type %it-does-in-fact-matter]]
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results3  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/static] type=~ %kill ~nul]
      ::
      moves=~
    ==
  ::
  =^  results4  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/autocons] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    (expect-ford-empty ford ~nul)
  ==
::
++  test-slim
  ~&  %test-slim
  ::
  =/  formula=hoon  (ream '(add 2 2)')
  =/  subject-type=type  -:!>(.)
  ::
  =/  ford  *ford-turbo
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/dead] type=~ %make ~nul [%slim subject-type formula]]
      ::
      ^=  moves
        :~  :*  duct=~[/dead]  %give  %made  ~1234.5.6  %complete
                [%success [%slim (~(mint ut subject-type) [%noun formula])]]
    ==  ==  ==
  ::
  =^  results2  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.7
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/dead] type=~ %kill ~nul]
      ::
      moves=~
    ==
  ::
  ;:  weld
    results1
    results2
    (expect-ford-empty ford ~nul)
  ==
::
++  test-ride
  ~&  %test-ride
  ::
  =/  ford  *ford-turbo
  ::
  =/  fun  |=(a=@ (add 2 a))
  =/  formula=hoon  (ream '!:  (fun 3)')
  =/  subject-schematic=schematic:ford  [%$ %noun !>(.)]
  ::
  =^  results1  ford
    %-  test-ford-call-with-comparator  :*
      ford
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      ^=  call-args
        :*  duct=~[/dead]  type=~  %make  ~nul
            [%ride formula subject-schematic]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-turbo)
        ^-  tang
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete %success %ride *] i.moves)
        ::
        %+  weld
          %-  expect-eq  !>
          ::  compare the move to the expected move, omitting vase type checking
          ::
          ::    Types can't be compared using simple equality, so normalize the
          ::    type to check the rest of the move.
          ::
          :-  i.moves(&8 *type)
          :*  duct=~[/dead]  %give  %made  ~1234.5.6  %complete
              [%success [%ride *type 5]]
          ==
        ::  make sure the returned type nests
        ::
        %-  expect-eq  !>
        :-  (~(nest ut &8:i.moves) | -:!>(*@))
        &
    ==
  ::
  =^  results2  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.7
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/dead] type=~ %kill ~nul]
      moves=~
    ==
  ::
  ;:  weld
    results1
    results2
    (expect-ford-empty ford ~nul)
  ==
::
++  test-ride-scry-succeed
  ~&  %test-ride-scry-succeed
  ::
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  =/  ford  *ford-turbo
  ::
  =/  formula=hoon  (ream '!:  .^(* %cx /~nul/desk/~1234.5.6/foo/bar)')
  =/  subject-schematic=schematic:ford  [%$ %noun !>(.)]
  ::
  =^  results1  ford
    %-  test-ford-call-with-comparator  :*
      ford
      now=~1234.5.6
      scry=scry-42
      ::
      ^=  call-args
        :*  duct=~[/dead]  type=~  %make  ~nul
            [%ride formula subject-schematic]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-turbo)
        ^-  tang
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete %success %ride *] i.moves)
        ::
        %+  welp
          %-  expect-eq  !>
          ::  compare the move to the expected move, omitting vase type checking
          ::
          ::    Types can't be compared using simple equality, so normalize the
          ::    type to check the rest of the move.
          ::
          :-  i.moves(&8 *type)
          :*  duct=~[/dead]  %give  %made  ~1234.5.6  %complete
              [%success [%ride *type 42]]
          ==
        ::
        %-  expect-eq  !>
        :-  (~(nest ut &8:i.moves) | -:!>(*@))
        &
    ==
  ::
  =^  results2  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.7
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/dead] type=~ %kill ~nul]
      moves=~
    ==
  ::
  ;:  weld
    results1
    results2
    (expect-ford-empty ford ~nul)
  ==
::
++  test-ride-scry-fail
  ~&  %test-ride-scry-fail
  ::
  =/  scry-failed  (scry-fail ~1234.5.6)
  =/  ford  *ford-turbo
  ::
  =/  formula=hoon  (ream '!:  .^(* %cx /~nul/desk/~1234.5.6/foo/bar)')
  =/  subject-schematic=schematic:ford  [%$ %noun !>(.)]
  ::
  =^  results1  ford
    %-  test-ford-call-with-comparator  :*
      ford
      now=~1234.5.6
      scry=scry-failed
      ::
      ^=  call-args
        :*  duct=~[/dead]  type=~  %make  ~nul
            [%ride formula subject-schematic]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-turbo)
        ^-  tang
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete %error *] i.moves)
        ::
        %-  expect-eq  !>
        ::  compare the move to the expected move, omitting check on stack trace
        ::
        :-  i.moves(|7 ~)
        :*  duct=~[/dead]  %give  %made  ~1234.5.6  %complete
            [%error [leaf+"ford: %ride failed:" ~]]
    ==  ==
  ::
  =^  results2  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.7
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/dead] type=~ %kill ~nul]
      moves=~
    ==
  ::
  ;:  weld
    results1
    results2
    (expect-ford-empty ford ~nul)
  ==
::
++  test-ride-scry-block
  ~&  %test-ride-scry-block
  ::
  =/  scry-blocked  (scry-block ~1234.5.6)
  =/  ford  *ford-turbo
  ::
  =/  formula=hoon  (ream '!:  .^(* %cx /~nul/desk/~1234.5.6/foo/bar)')
  =/  subject-schematic=schematic:ford  [%$ %noun !>(.)]
  ::
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry-blocked
      ::
      ^=  call-args
        :*  duct=~[/live]  type=~  %make  ~nul
            [%ride formula subject-schematic]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %pass
                wire=/~nul/resource/cx/~nul/desk/0/foo/bar
                %c  %warp  [~nul ~nul]  %desk
                ~  %sing  %x  [%da ~1234.5.6]  /bar/foo
    ==  ==  ==
  ::
  =^  results2  ford
    %-  test-ford-take-with-comparator  :*
      ford
      now=~1234.5.7
      scry=scry-blocked
      ::
      ^=  call-args
        :*  wire=/~nul/resource/cx/~nul/desk/0/foo/bar  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type  ::  ^-  sign:ford
            [%c %writ ~ [%x [%da ~1234.5.6] %desk] /bar/foo %noun !>(42)]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-turbo)
        ^-  tang
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete %success %ride *] i.moves)
        ::
        %+  welp
          %-  expect-eq  !>
          ::  compare the move to the expected move, omitting vase type checking
          ::
          ::    Types can't be compared using simple equality, so normalize the
          ::    type to check the rest of the move.
          ::
          :-  i.moves(&8 *type)
          :*  duct=~[/live]  %give  %made  ~1234.5.6  %complete
              [%success [%ride *type 42]]
          ==
        ::  make sure the types nest
        ::
        %-  expect-eq  !>
        :-  (~(nest ut &8:i.moves) | -:!>(*@))
        &
    ==
  ::
  =^  results3  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/live] type=~ %kill ~nul]
      moves=~
    ==
  ::
  ;:  weld
    results1
    results2
    results3
    (expect-ford-empty ford ~nul)
  ==
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
  =/  ford  *ford-turbo
  ::
  =/  formula=hoon  (ream '`@tas`%constant')
  =/  subject-schematic=schematic:ford  [%scry %c %x [~nul %desk] /bar/foo]
  ::
  =/  ride=schematic:ford  [%ride formula subject-schematic]
  ::
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/ride] type=~ %make ~nul ride]
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %give  %made  ~1234.5.6  %complete
                [%success [%ride scry-type %constant]]
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results2  ford
    %-  test-ford-take  :*
      ford
      now=~1234.5.7
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results3  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/ride] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    (expect-ford-empty ford ~nul)
  ==
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
  =/  ford  *ford-turbo
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
  =^  results1  ford
    %-  test-ford-call-with-comparator  :*
      ford
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/post-a] type=~ %make ~nul rendered-a]
      ::
      ^=  comparator
        |=  moves=(list move:ford-turbo)
        ^-  tang
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
        %-  expect-eq  !>
        :-  i.t.moves
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.6] (sy [%x /posts/a] [%x /posts/b] ~)]
    ==  ==
  ::
  =^  results2  ford
    %-  test-ford-call-with-comparator  :*
      ford
      now=~1234.5.7
      scry=scry
      ::
      call-args=[duct=~[/post-b] type=~ %make ~nul rendered-b]
      ::
      ^=  comparator
        |=  moves=(list move:ford-turbo)
        ^-  tang
        ::
        ?>  ?=([^ ~] moves)
        %-  check-post-made  :*
          move=i.moves
          duct=~[/post-b]
          type=scry-type
          date=~1234.5.7
          title='post-b'
          contents="post-b-contents"
    ==  ==
  ::
  =^  results3  ford
    %-  test-ford-take-with-comparator  :*
      ford
      now=~1234.5.8
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
            [%c %wris [%da ~1234.5.8] (sy [%x /posts/a]~)]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-turbo)
        ^-  tang
        ::
        ?>  ?=([^ ^ ~] moves)
        %-  check-post-made  :*
          move=i.moves
          duct=~[/post-a]
          type=scry-type
          date=~1234.5.8
          title='post-a'
          contents="post-a-contents-changed"
    ==  ==
  ::
  =^  results4  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.9
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/post-b] type=~ %kill ~nul]
      moves=~
    ==
  ::
  =^  results5  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.10
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/post-a] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    results5
    (expect-ford-empty ford ~nul)
  ==
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
  =/  ford  *ford-turbo
  ::
  =/  scry1=schematic:ford  [%scry [%c %x [~nul %first] /one/scry]]
  =/  scry2=schematic:ford  [%scry [%c %x [~nul %second] /two/scry]]
  =/  alts=schematic:ford   [%alts [scry1 scry2 ~]]
  ::
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/alts] type=~ %make ~nul alts]
      ::
      ^=  moves
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
    ==  ==  ==
  ::
  =^  results2  ford
    %-  test-ford-take  :*
      ford
      now=~1234.5.7
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/second  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /scry/two]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/alts]  %give  %made  ~1234.5.7  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-two'
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/second
                %c  %warp  [~nul ~nul]  %second
                `[%mult [%da ~1234.5.7] (sy [%x /scry/two] ~)]
    ==  ==  ==
  ::
  =^  results3  ford
    %-  test-ford-take  :*
      ford
      now=~1234.5.8
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/first  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
            [%c %wris [%da ~1234.5.8] (sy [%x /scry/one]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/alts]  %give  %made  ~1234.5.8  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-one'
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/first
                %c  %warp  [~nul ~nul]  %first
                `[%mult [%da ~1234.5.8] (sy [%x /scry/one] ~)]
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/second
                %c  %warp  [~nul ~nul]  %second  ~
    ==  ==  ==
  ::
  =^  results4  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.9
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/alts] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/first
                %c  %warp  [~nul ~nul]  %first  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    (expect-ford-empty ford ~nul)
  ==
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
  =/  ford  *ford-turbo
  ::
  =/  scry2=schematic:ford  [%scry [%c %x [~nul %desk] /two/scry]]
  =/  same=schematic:ford   [%same scry2]
  ::  depend on scry2 for the duration of the test
  ::
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/same] type=~ %make ~nul same]
      ::
      ^=  moves
        :~  :*  duct=~[/same]  %give  %made  ~1234.5.6  %complete
                %success  %same  %success  %scry  %noun  scry-type  'scry-two'
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /scry/two] ~)]
    ==  ==  ==
  ::
  =/  scry1=schematic:ford  [%scry [%c %x [~nul %desk] /one/scry]]
  =/  alts=schematic:ford   [%alts [scry1 scry2 ~]]
  ::  call the alts schematic
  ::
  ::    The alts schematic should fail to read /scry/one, and should fallback
  ::    to /scry/two.
  ::
  =^  results2  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.7
      scry=scry
      ::
      call-args=[duct=~[/alts] type=~ %make ~nul alts]
      ::
      ^=  moves
        :~  :*  duct=~[/alts]  %give  %made  ~1234.5.7  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-two'
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /scry/two] [%x /scry/one] ~)]
    ==  ==  ==
  ::
  ::  tell ford that /scry/one exists now
  ::
  =^  results3  ford
    %-  test-ford-take  :*
      ford
      now=~1234.5.8
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
            [%c %wris [%da ~1234.5.8] (sy [%x /scry/one]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/alts]  %give  %made  ~1234.5.8  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-one'
            ==
            ::  we subscribe to both paths because /same still exists.
            ::
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.8] (sy [%x /scry/one] [%x /scry/two] ~)]
    ==  ==  ==
  ::
  ::  kill the /same build
  ::
  ::    We should no longer subscribe to /scry/two in the resulting clay
  ::    subscription.
  ::
  =^  results4  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.9
      scry=scry
      ::
      call-args=[duct=~[/same] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.8] (sy [%x /scry/one] ~)]
    ==  ==  ==
  ::
  =^  results5  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.10
      scry=scry
      ::
      call-args=[duct=~[/alts] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    results5
    (expect-ford-empty ford ~nul)
  ==
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
  =/  ford  *ford-turbo
  ::
  =/  scry1=schematic:ford  [%scry [%c %x [~nul %desk] /one/scry]]
  =/  scry2=schematic:ford  [%scry [%c %x [~nul %desk] /two/scry]]
  =/  scry3=schematic:ford  [%scry [%c %x [~nul %desk] /three/scry]]
  =/  alts1=schematic:ford  [%alts [scry1 scry2 ~]]
  =/  alts2=schematic:ford  [%alts [scry3 scry2 ~]]
  ::  alts1 will depend on both scry1 and scry2
  ::
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/first] type=~ %make ~nul alts1]
      ::
      ^=  moves
        :~  :*  duct=~[/first]  %give  %made  ~1234.5.6  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-two'
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /scry/one] [%x /scry/two] ~)]
    ==  ==  ==
  ::  alts2 will depend on both scry3 and scry2
  ::
  =^  results2  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.7
      scry=scry
      ::
      call-args=[duct=~[/second] type=~ %make ~nul alts2]
      ::
      ^=  moves
        :~  :*  duct=~[/second]  %give  %made  ~1234.5.7  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-two'
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~  %mult  [%da ~1234.5.7]
                (sy [%x /scry/one] [%x /scry/two] [%x /scry/three] ~)
    ==  ==  ==
  ::
  ::  alts2 should now just return 'scry-three'
  ::
  =^  results3  ford
    %-  test-ford-take  :*
      ford
      now=~1234.5.8
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
            [%c %wris [%da ~1234.5.8] (sy [%x /scry/three]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/second]  %give  %made  ~1234.5.8  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-three'
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~  %mult  [%da ~1234.5.8]
                (sy [%x /scry/one] [%x /scry/two] [%x /scry/three] ~)
    ==  ==  ==
  ::
  ::  alts1 should now just return 'scry-one'
  ::
  =^  results4  ford
    %-  test-ford-take  :*
      ford
      now=~1234.5.9
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
            [%c %wris [%da ~1234.5.9] (sy [%x /scry/one] [%x /scry/two] ~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/first]  %give  %made  ~1234.5.9  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-one'
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~  %mult  [%da ~1234.5.9]
                (sy [%x /scry/one] [%x /scry/three] ~)
    ==  ==  ==
  ::
  =^  results5  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.10
      scry=scry
      ::
      call-args=[duct=~[/first] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~  %mult  [%da ~1234.5.9]
                (sy [%x /scry/three] ~)
    ==  ==  ==
  ::
  =^  results6  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.11
      scry=scry
      ::
      call-args=[duct=~[/second] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    results5
    results6
    (expect-ford-empty ford ~nul)
  ==
::  +test-cache-reclamation-trivial: reclaim cache on a blank slate ford
::
++  test-cache-reclamation-trivial
  ~&  %test-cache-reclamation-trivial
  ::
  =/  ford  *ford-turbo
  ::
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry-is-forbidden
      ::  send a pinned literal, expects a %made response with pinned literal
      ::
      ^=  call-args
        [duct=~ type=~ %make ~nul [%pin ~1234.5.6 [%$ %noun !>(**)]]]
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.6
                %complete  %success  %pin  ~1234.5.6  %success  %$  %noun  !>(**)
        ==  ==
    ==
  ::
  =^  results2  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.7
      scry=scry-is-forbidden
      ::  ask ford to wipe its cache
      ::
      call-args=[duct=~ type=~ %wipe ~]
      ::  cache wiping should never produce any moves
      ::
      moves=~
    ==
  ::
  ;:  welp
    results1
    results2
    (expect-ford-empty ford ~nul)
  ==
::
++  test-cache-reclamation-live-rebuild
  ~&  %test-cache-reclamation-live-rebuild
  ::
  =/  ford  *ford-turbo
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=(scry-succeed ~1234.5.6 [%noun !>(42)])
      ::
      ^=  call-args
        :*  duct=~  type=~  %make  ~nul
            [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.6  %complete  %success
                [%scry %noun !>(42)]
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results2  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.7
      scry=scry-is-forbidden
      ::  ask ford to wipe its cache
      ::
      call-args=[duct=~ type=~ %wipe ~]
      ::  cache wiping should never produce any moves
      ::
      moves=~
    ==
  ::
  =^  results3  ford
    %-  test-ford-take  :*
      ford
      now=~1234.5.7
      scry=(scry-succeed ~1234.5.7 [%noun !>(43)])
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.7  %complete  %success
                [%scry %noun !>(43)]
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results4  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~ type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    (expect-ford-empty ford ~nul)
  ==
::
++  test-cache-reclamation-live-promote
  ~&  %test-cache-reclamation-live-promote
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
  =/  ford  *ford-turbo
  ::
  =/  formula=hoon  (ream '`@tas`%constant')
  =/  subject-schematic=schematic:ford  [%scry %c %x [~nul %desk] /bar/foo]
  ::
  =/  ride=schematic:ford  [%ride formula subject-schematic]
  =/  same=schematic:ford  [%same ride]
  ::
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/ride] type=~ %make ~nul same]
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %give  %made  ~1234.5.6  %complete
                [%success [%same [%success [%ride scry-type %constant]]]]
            ==
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results2  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.7
      scry=scry-is-forbidden
      ::  ask ford to wipe its cache
      ::
      call-args=[duct=~ type=~ %wipe ~]
      ::  cache wiping should never produce any moves
      ::
      moves=~
    ==
  ::
  =^  results3  ford
    %-  test-ford-take  :*
      ford
      now=~1234.5.7
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results4  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/ride] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    (expect-ford-empty ford ~nul)
  ==

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
  ^-  tang
  ::
  ?>  ?=([* %give %made @da %complete %success ^ *] move)
  =/  result  result.p.card.move
  ?>  ?=([%success %scry %noun type-a=* @tas *] head.result)
  ?>  ?=([%success ^ *] tail.result)
  ?>  ?=([%success %ride type-title-a=* %post-a] head.tail.result)
  ?>  ?=([%success %ride type-title-b=* %post-b] tail.tail.result)
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
    [%success %scry %noun *^type [title=title contents=contents]]
  ::
    %-  expect-eq  !>
    :-  (~(nest ut p.q.cage.head.result) | type)
    &
  ::
    %-  expect-eq  !>
    :-  head.tail.result(p.vase *^type)
    [%success %ride *^type 'post-a']
  ::
    %-  expect-eq  !>
    :-  (~(nest ut p.vase.head.tail.result) | -:!>(''))
    &
  ::
    %-  expect-eq  !>
    :-  tail.tail.result(p.vase *^type)
    [%success %ride *^type 'post-b']
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
::
++  test-ford-call
  |=  $:  ford=_(ford-turbo)
          now=@da
          scry=sley
          call-args=[=duct type=* wrapped-task=(hobo task:able:ford-turbo)]
          expected-moves=(list move:ford-turbo)
      ==
  ^-  [tang _(ford-turbo)]
  ::
  =.  ford  (ford now=now eny=0xdead.beef scry=scry)
  ::
  =^  moves  ford
    %-  call:ford  call-args
  ::
  =/  output=tang
    %-  expect-eq  !>
    :-  moves
    expected-moves
  ::
  [output ford]
::
++  test-ford-take
  |=  $:  ford=_(ford-turbo)
          now=@da
          scry=sley
          take-args=[=wire =duct wrapped-sign=(hypo sign:ford-turbo)]
          expected-moves=(list move:ford-turbo)
      ==
  ^-  [tang _(ford-turbo)]
  ::
  =.  ford  (ford now=now eny=0xdead.beef scry=scry)
  ::
  =^  moves  ford
    %-  take:ford  take-args
  ::
  =/  output=tang
    %-  expect-eq  !>
    :-  moves
    expected-moves
  ::
  [output ford]
::  +test-ford-call-with-comparator
::
::    Sometimes we can't just do simple comparisons between the moves statements and
::    must instead specify a gate that performs the comparisons.
::
++  test-ford-call-with-comparator
  |=  $:  ford=_(ford-turbo)
          now=@da
          scry=sley
          call-args=[=duct type=* wrapped-task=(hobo task:able:ford-turbo)]
          move-comparator=$-((list move:ford-turbo) tang)
      ==
  ^-  [tang _(ford-turbo)]
  ::
  =.  ford  (ford now=now eny=0xdead.beef scry=scry)
  ::
  =^  moves  ford
    %-  call:ford  call-args
  ::
  =/  output=tang  (move-comparator moves)
  ::
  [output ford]
::  +test-ford-take-with-comparator
::
++  test-ford-take-with-comparator
  |=  $:  ford=_(ford-turbo)
          now=@da
          scry=sley
          take-args=[=wire =duct wrapped-sign=(hypo sign:ford-turbo)]
          move-comparator=$-((list move:ford-turbo) tang)
      ==
  ^-  [tang _(ford-turbo)]
  ::
  =.  ford  (ford now=now eny=0xdead.beef scry=scry)
  ::
  =^  moves  ford
    %-  take:ford  take-args
  ::
  =/  output=tang  (move-comparator moves)
  ::
  [output ford]
::  +expect-ford-empty: assert that ford's state is one empty ship
::
::    At the end of every test, we want to assert that we have cleaned up all
::    state.
::
++  expect-ford-empty
  |=  [ford=_(ford-turbo) ship=@p]
  ^-  tang
  %-  expect-eq  !>
  :-  state-by-ship.+>+<.ford
  (my [ship *ford-state:ford-turbo]~)
--
