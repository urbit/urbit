/+  ford-turbo, tester
::
:-  %say
|=  [[now=@da eny=@ bek=beak] ~ ~]
:-  %noun
=+  tester:tester
::
=/  test-pit=vase  !>(.)
=/  ford-gate  (ford-turbo test-pit)
::
|^
=-  ((slog -) ~)
^-  tang
;:  weld
  test-tear
  test-is-schematic-live
  test-date-from-schematic
  test-unify-jugs
  test-resource-wire-encoding
  test-parse-scaffold-direct
  test-parse-scaffold-indirect
  test-parse-scaffold-sur-lib
  test-parse-scaffold-zuse-version
  test-parse-scaffold-crane-fssg
  test-parse-scaffold-crane-fsbc
  test-parse-scaffold-crane-fsbr
  test-parse-scaffold-crane-fsts
  test-parse-scaffold-crane-fsdt
  test-parse-scaffold-crane-fscm
  test-parse-scaffold-crane-fscn
  test-parse-scaffold-crane-fspm
  test-parse-scaffold-crane-fscb
  test-parse-scaffold-crane-fssm
  test-parse-scaffold-crane-fscl
  test-parse-scaffold-crane-fskt
  test-parse-scaffold-crane-fszp
  test-parse-scaffold-crane-fszy
  test-literal
  test-autocons-same
  test-autocons-different
  test-scry-clay-succeed
  test-scry-clay-fail
  test-scry-clay-block
  test-scry-clay-live
  test-scry-clay-live-again
  test-scry-clay-same-path
  test-pinned-in-past
  test-pinned-in-future
  test-pinned-in-pin
  test-pinned-in-live
  test-live-build-that-blocks
  test-live-and-once
  test-live-two-deep
  test-live-three-deep
  test-live-triangle
  test-live-and-pinned-triangle
  test-call
  test-call-scry-succeed
  test-call-scry-fail
  test-call-scry-block
  test-call-scry-varies
  test-dude
  test-dude-error
  test-hood
  test-slim
  test-slit
  test-slit-error
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
  test-five-oh-cache-reclamation
::  test-reef  ::  very slow
  test-reef-short-circuit
  test-path
  test-plan-direct-hoon
  test-core
  test-core-linker
  test-core-fsts-fssg
  test-core-fsdt-fskt
  test-core-fskt-nest-fail
  test-core-fssm
==
++  test-tear
  ~&  %test-tear
  ::
  ;:  welp
    %-  expect-eq  !>
    :-  ~['a' 'bc' 'de']
    (tear:ford-gate 'a-bc-de')
  ::
    %-  expect-eq  !>
    :-  ~['abc']
    (tear:ford-gate 'abc')
  ::
    %-  expect-eq  !>
    :-  ~['ab/c']
    (tear:ford-gate 'ab/c')
  ==
::
++  test-is-schematic-live
  ~&  %test-is-schematic-live
  ::
  =/  ford  (ford-gate now=~1234.5.6 eny=0xdead.beef scry=scry-is-forbidden)
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
  =/  ford  (ford-gate now=~1234.5.6 eny=0xdead.beef scry=scry-is-forbidden)
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
  =/  ford  (ford-gate now=~1234.5.6 eny=0xdead.beef scry=scry-is-forbidden)
  ::
  %-  expect-eq  !>
  :-  ^-  (jug @tas @ud)
      (my ~[[%a (sy 1 2 ~)] [%b (sy 3 4 5 6 ~)] [%c (sy 7 8 ~)]])
  ::
  %+  unify-jugs:ford
    `(jug @tas @ud)`(my ~[[%a (sy 1 2 ~)] [%b (sy 3 4 ~)]])
  `(jug @tas @ud)`(my ~[[%b (sy 5 6 ~)] [%c (sy 7 8 ~)]])
::
++  test-resource-wire-encoding
  ~&  %test-resource-wire-encoding
  ::
  =/  ford  (ford-gate now=~1234.5.6 eny=0xdead.beef scry=scry-is-forbidden)
  ::
  ;:  welp
    %-  expect-eq  !>
    :-  /cx/~nul/desk/~1234.5.6/bar/foo
    ^-  path
    %-  scry-request-to-path:ford
    [%c care=%x [[~nul %desk [%da ~1234.5.6]] /foo/bar]]
  ::
    %-  expect-eq  !>
    :-  (need (path-to-scry-request:ford /cx/~nul/desk/~1234.5.6/bar/foo))
    `scry-request:ford`[%c care=%x [[~nul %desk [%da ~1234.5.6]] /foo/bar]]
  ::
    %-  expect-eq  !>
    :-  /gx/~nul/desk/~1234.5.6/bar/foo
    ^-  path
    %-  scry-request-to-path:ford
    [%g care=%x [[~nul %desk [%da ~1234.5.6]] /foo/bar]]
  ::
    %-  expect-eq  !>
    :-  (need (path-to-scry-request:ford /gx/~nul/desk/~1234.5.6/bar/foo))
    ^-  scry-request:ford
    [%g care=%x [[~nul %desk [%da ~1234.5.6]] /foo/bar]]
  ==
::
++  test-parse-scaffold-direct
  ~&  %test-parse-scaffold-direct
  ::
  %-  expect-eq  !>
  :-  :-  [1 19]
      :-  ~
      :_  [[1 19] ""]
      :*  %309  ~  ~  ~
        :*  %direct  %dbug  [/~nul/desk/~1234.5.6/bar/foo [[1 1] [1 19]]]
            (ream '!.  |=(a=@ud +(a))')
        ==
        ~
      ==
  %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /foo/bar]))
    [1 1]
  "!.  |=(a=@ud +(a))"
::
++  test-parse-scaffold-indirect
  ~&  %test-parse-scaffold-indirect
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /foo/bar]))
      [1 1]
    """
    //  /%/child/hoon
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  zuse-version=309
      structures=~
      libraries=~
      cranes=~
      ^=  sources
        :~  :*  %indirect
                [[~nul %desk [%da ~1234.5.6]] /hoon/child/foo/bar]
  ==    ==  ==
::
++  test-parse-scaffold-sur-lib
  ~&  %test-parse-scaffold-sur-lib
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /foo/bar]))
      [1 1]
    """
    /-  struct, face=other
    /+  library, *thing
    !.
    |=(a a)
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  zuse-version=309
      structures=~[[`%struct %struct] [`%face %other]]
      libraries=~[[`%library %library] [~ %thing]]
      cranes=~
      ^=  sources
        :~  :*  %direct
                %dbug
                [/~nul/desk/~1234.5.6/bar/foo [3 1] [4 8]]
                (ream '|=(a a)')
  ==    ==  ==
::
++  test-parse-scaffold-zuse-version
  ~&  %test-parse-scaffold-zuse-version
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /foo/bar]))
      [1 1]
    """
    /?  400
    !.
    |=(a a)
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  zuse-version=400
      structures=~
      libraries=~
      cranes=~
      ^=  sources
        :~  :*  %direct
                %dbug
                [/~nul/desk/~1234.5.6/bar/foo [2 1] [3 8]]
                (ream '|=(a a)')
  ==    ==  ==
::
++  test-parse-scaffold-crane-fssg
  ~&  %test-parse-scaffold-crane-fssg
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /foo/bar]))
      [1 1]
    """
    /~  !.  [a=1 b=3]
    !.
    |=(a b)
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fssg
                %dbug
                [/~nul/desk/~1234.5.6/bar/foo [1 5] [1 18]]
                (ream '[a=1 b=3]')
        ==  ==
      ^=  sources
        :~  :*  %direct
                %dbug
                [/~nul/desk/~1234.5.6/bar/foo [2 1] [3 8]]
                (ream '|=(a b)')
  ==    ==  ==
::
++  test-parse-scaffold-crane-fsbc
  ~&  %test-parse-scaffold-crane-fsbc
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /foo/bar]))
      [1 1]
    """
    /$  !.  |=(a a)
    !.
    |=(a b)
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fsbc
                %dbug
                [/~nul/desk/~1234.5.6/bar/foo [1 5] [1 16]]
                (ream '|=(a a)')
        ==  ==
      ^=  sources
        :~  :*  %direct
                %dbug
                [/~nul/desk/~1234.5.6/bar/foo [2 1] [3 8]]
                (ream '|=(a b)')
  ==    ==  ==
::
++  test-parse-scaffold-crane-fsbr
  ~&  %test-parse-scaffold-crane-fsbr
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /foo/bar]))
      [1 1]
    """
    /|  /~  ~
        /~  ~
        ==
    //  /%/child/hoon
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fsbr
                :~  :*  %fssg  %dbug
                        [/~nul/desk/~1234.5.6/bar/foo [1 9] [1 10]]
                        [%bust %null]
                    ==
                    :*  %fssg  %dbug
                        [/~nul/desk/~1234.5.6/bar/foo [2 9] [2 10]]
                        [%bust %null]
        ==  ==  ==  ==
      ^=  sources
        :~  :*  %indirect
                [[~nul %desk [%da ~1234.5.6]] /hoon/child/foo/bar]
  ==    ==  ==
::
++  test-parse-scaffold-crane-fsts
  ~&  %test-parse-scaffold-crane-fsts
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /foo/bar]))
      [1 1]
    """
    /=  a  /~  ~
    //  /%/child/hoon
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fsts  %a
                %fssg  %dbug  [/~nul/desk/~1234.5.6/bar/foo [1 12] [1 13]]
                [%bust %null]
        ==  ==
      ^=  sources
        :~  :*  %indirect
                [[~nul %desk [%da ~1234.5.6]] /hoon/child/foo/bar]
  ==    ==  ==
::
++  test-parse-scaffold-crane-fsdt
  ~&  %test-parse-scaffold-crane-fsdt
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /foo/bar]))
      [1 1]
    """
    /.  /~  !.  a=5
        /~  !.  b=6
        ==
    //  /%/child/hoon
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fsdt
                :~  :*  %fssg  %dbug
                        [/~nul/desk/~1234.5.6/bar/foo [1 9] [1 16]]
                        (ream 'a=5')
                    ==
                    :*  %fssg  %dbug
                        [/~nul/desk/~1234.5.6/bar/foo [2 9] [2 16]]
                        (ream 'b=6')
        ==  ==  ==  ==
      ^=  sources
        :~  :*  %indirect
                [[~nul %desk [%da ~1234.5.6]] /hoon/child/foo/bar]
  ==    ==  ==
::
++  test-parse-scaffold-crane-fscm
  ~&  %test-parse-scaffold-crane-fscm
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /foo/bar]))
      [1 1]
    """
    /,
        /path/to/a
      /~  !.  a=5
    ::
        /path/to/b
      /~  !.  b=6
    ==
    //  /%/child/hoon
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fscm
                :~  :-  /path/to/a
                    :*  %fssg  %dbug
                        [/~nul/desk/~1234.5.6/bar/foo [3 7] [3 14]]
                        (ream 'a=5')
                    ==
                    :-  /path/to/b
                    :*  %fssg  %dbug
                        [/~nul/desk/~1234.5.6/bar/foo [6 7] [6 14]]
                        (ream 'b=6')
        ==  ==  ==  ==
      ^=  sources
        :~  :*  %indirect
                [[~nul %desk [%da ~1234.5.6]] /hoon/child/foo/bar]
  ==    ==  ==
::
++  test-parse-scaffold-crane-fscn
  ~&  %test-parse-scaffold-crane-fscn
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /foo/bar]))
      [1 1]
    """
    /%  /$  !.  |=(a a)
    //  /%/child/hoon
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fscn  %fsbc  %dbug
                [/~nul/desk/~1234.5.6/bar/foo [1 9] [1 20]]
                (ream '|=(a a)')
        ==  ==
      ^=  sources
        :~  :*  %indirect
                [[~nul %desk [%da ~1234.5.6]] /hoon/child/foo/bar]
  ==    ==  ==
::
++  test-parse-scaffold-crane-fspm
  ~&  %test-parse-scaffold-crane-fspm
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /foo/bar]))
      [1 1]
    """
    /=  data  /&  mark  /~  !.  a=1
    //  /%/child/hoon
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fsts  %data
                %fspm  [%mark ~]
                %fssg  %dbug
                [/~nul/desk/~1234.5.6/bar/foo [1 25] [1 32]]
                (ream 'a=1')
        ==  ==
      ^=  sources
        :~  :*  %indirect
                [[~nul %desk [%da ~1234.5.6]] /hoon/child/foo/bar]
  ==    ==  ==
::
++  test-parse-scaffold-crane-fscb
  ~&  %test-parse-scaffold-crane-fscb
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /foo/bar]))
      [1 1]
    """
    /_  /mark/
    //  /%/child/hoon
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fscb  %fszy  %mark
        ==  ==
      ^=  sources
        :~  :*  %indirect
                [[~nul %desk [%da ~1234.5.6]] /hoon/child/foo/bar]
  ==    ==  ==
::
++  test-parse-scaffold-crane-fssm
  ~&  %test-parse-scaffold-crane-fssm
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /foo/bar]))
      [1 1]
    """
    /=  data
      /;  !.  |=(a=@u +(a))
      /~  !.  5
    //  /%/child/hoon
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fsts  %data
                %fssm
                :*  %dbug
                    [/~nul/desk/~1234.5.6/bar/foo [2 7] [2 24]]
                    (ream '|=(a=@u +(a))')
                ==
                %fssg
                :*  %dbug
                    [/~nul/desk/~1234.5.6/bar/foo [3 7] [3 12]]
                    (ream '5')
        ==  ==  ==
      ^=  sources
        :~  :*  %indirect
                [[~nul %desk [%da ~1234.5.6]] /hoon/child/foo/bar]
  ==    ==  ==
::
++  test-parse-scaffold-crane-fscl
  ~&  %test-parse-scaffold-crane-fscl
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /foo/bar]))
      [1 1]
    """
    /=  tests
      /:  /===/tests
      /_  /mark/
    //  /%/child/hoon
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fsts  %tests
                %fscl  [[~ ~[~ ~ ~ [~ [%sand %tas 495.874.958.708]]]] ~]
                %fscb  %fszy  %mark
        ==  ==
      ^=  sources
        :~  :*  %indirect
                [[~nul %desk [%da ~1234.5.6]] /hoon/child/foo/bar]
  ==    ==  ==
::
++  test-parse-scaffold-crane-fskt
  ~&  %test-parse-scaffold-crane-fskt
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /foo/bar]))
      [1 1]
    """
    /=  data
      /^  !.  (list @ud)
      /.  /~  !.  1
          /~  !.  2
          ==
    //  /%/child/hoon
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fsts  %data
                %fskt
                :*  %dbug
                    [/~nul/desk/~1234.5.6/bar/foo [2 7] [2 21]]
                    (ream '(list @ud)')
                ==
                %fsdt
                :~  :*  %fssg  %dbug
                        [/~nul/desk/~1234.5.6/bar/foo [3 11] [3 16]]
                        (ream '1')
                    ==
                    :*  %fssg  %dbug
                        [/~nul/desk/~1234.5.6/bar/foo [4 11] [4 16]]
                        (ream '2')
        ==  ==  ==  ==
      ^=  sources
        :~  :*  %indirect
                [[~nul %desk [%da ~1234.5.6]] /hoon/child/foo/bar]
  ==    ==  ==
::
++  test-parse-scaffold-crane-fszp
  ~&  %test-parse-scaffold-crane-fszp
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /foo/bar]))
      [1 1]
    """
    /=  data  /!mark/
    //  /%/child/hoon
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fsts  %data
                %fszp  %mark
        ==  ==
      ^=  sources
        :~  :*  %indirect
                [[~nul %desk [%da ~1234.5.6]] /hoon/child/foo/bar]
  ==    ==  ==
::
++  test-parse-scaffold-crane-fszy
  ~&  %test-parse-scaffold-crane-fszy
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /foo/bar]))
      [1 1]
    """
    /=  data  /mark/
    //  /%/child/hoon
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fsts  %data
                %fszy  %mark
        ==  ==
      ^=  sources
        :~  :*  %indirect
                [[~nul %desk [%da ~1234.5.6]] /hoon/child/foo/bar]
  ==    ==  ==
::
++  test-literal
  ~&  %test-literal
  ::
  =/  ford  *ford-gate
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
  =/  ford  *ford-gate
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
  =/  ford  *ford-gate
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
  =/  ford  *ford-gate
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
  =/  ford  *ford-gate
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
  =/  ford  *ford-gate
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
                wire=/~nul/scry-request/cx/~nul/desk/~1234.5.6/foo/bar
                %c  %warp  [~nul ~nul]  %desk
                ~  %sing  %x  [%da ~1234.5.6]  /foo/bar
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
        :*  wire=/~nul/scry-request/cx/~nul/desk/~1234.5.6/foo/bar  duct=~
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
  =/  ford  *ford-gate
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
  =/  ford  *ford-gate
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
::  tests multiple subscriptions on the same resource at different times
::
::    We can depend on the same paths but at different times. Make sure we can
::    block on /~nul/desk/~1234.5.7/... and /~nul/desk/~1234.5.8/... at the
::    same time.
::
++  test-scry-clay-same-path
  ~&  %test-scry-clay-same-path
  ::
  =/  scry-type=type  [%atom %tas ~]
  ::
  =/  blocks=(set @da)  (sy ~1234.5.7 ~1234.5.8 ~)
  ::
  =/  ford  *ford-gate
  ::
  =/  scry-schematic=schematic:ford
    [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
  =/  autocons=schematic:ford
    [[%pin ~1234.5.7 scry-schematic] [%pin ~1234.5.8 scry-schematic]]
  ::
  =^  results1  ford
    %-  test-ford-call-with-comparator  :*
      ford
      now=~1234.5.6
      scry=(scry-blocks blocks)
      ::
      call-args=[duct=~[/first] type=~ %make ~nul autocons]
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        %-  expect-eq  !>
        :-  (sy moves)
        %-  sy
        :~  :*  duct=~  %pass
                wire=/~nul/scry-request/cx/~nul/desk/~1234.5.7/foo/bar
                %c  %warp  [~nul ~nul]  %desk
                `[%sing %x [%da ~1234.5.7] /foo/bar]
            ==
            :*  duct=~  %pass
                wire=/~nul/scry-request/cx/~nul/desk/~1234.5.8/foo/bar
                %c  %warp  [~nul ~nul]  %desk
                `[%sing %x [%da ~1234.5.8] /foo/bar]
    ==  ==  ==
  ::
  =^  results2  ford
    %-  test-ford-take  :*
      ford
      now=~1234.5.7
      scry=(scry-blocks blocks)
      ::
      ^=  take-args
        :*  wire=/~nul/scry-request/cx/~nul/desk/~1234.5.7/foo/bar  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type  ::  ^-  sign:ford
            [%c %writ ~ [%x [%da ~1234.5.7] %desk] /bar/foo %noun scry-type %seven]
        ==
      ::
      expected-moves=~
    ==
  ::
  =^  results3  ford
    %-  test-ford-take  :*
      ford
      now=~1234.5.8
      scry=(scry-blocks blocks)
      ::
      ^=  take-args
        :*  wire=/~nul/scry-request/cx/~nul/desk/~1234.5.8/foo/bar  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type  ::  ^-  sign:ford
            [%c %writ ~ [%x [%da ~1234.5.8] %desk] /bar/foo %noun scry-type %eight]
        ==
      ::
      ^=  expected-moves
        ^-  (list move:ford-gate)
        :~  :*  duct=~[/first]  %give  %made  ~1234.5.6  %complete  %success
                [%success %pin ~1234.5.7 %success %scry %noun scry-type %seven]
                [%success %pin ~1234.5.8 %success %scry %noun scry-type %eight]
    ==  ==  ==
  ::
  =^  results4  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.9
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/first] type=~ %kill ~nul]
      ::
      moves=~
    ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    (expect-ford-empty ford ~nul)
  ==
::
++  test-pinned-in-past
  ~&  %test-pinned-in-past
  ::
  =/  ford  *ford-gate
  =/  schematic  [%pin ~1234.5.5 [%$ %noun !>(42)]]
  ::
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/pin] type=~ %make ~nul schematic]
      ::
      ^=  expected-moves
        :~  :*  duct=~[/pin]  %give  %made  ~1234.5.5  %complete
                %success  %pin  ~1234.5.5  %success  %$  %noun  !>(42)
    ==  ==  ==
  results1
::
++  test-pinned-in-future
  ~&  %test-pinned-in-future
  ::
  =/  ford  *ford-gate
  =/  schematic  [%pin ~1234.5.7 [%$ %noun !>(42)]]
  ::
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/pin] type=~ %make ~nul schematic]
      ::
      ^=  expected-moves
        :~  :*  duct=~[/pin]  %give  %made  ~1234.5.7  %complete
                %success  %pin  ~1234.5.7  %success  %$  %noun  !>(42)
    ==  ==  ==
  results1
::
++  test-pinned-in-pin
  ~&  %test-pinned-in-pin
  ::
  =/  ford  *ford-gate
  ::
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=(scry-succeed ~1234.5.8 [%noun !>(42)])
      ::
      ^=  call-args
        :*  duct=~[/pinned-in-future]  type=~  %make  ~nul
            %pin  ~1234.5.7
            %pin  ~1234.5.8
            [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/pinned-in-future]
                %give  %made  ~1234.5.8  %complete
                %success  %pin  ~1234.5.7
                %success  %pin  ~1234.5.8
                [%success %scry %noun !>(42)]
    ==  ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford ~nul)
  ==
::
++  test-pinned-in-live
  ~&  %test-pinned-in-live
  ::
  =/  ford  *ford-gate
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
    :_  results:(~(got by state-by-ship.+>+<.ford) ~nul)
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
  =/  ford  *ford-gate
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
                wire=/~nul/scry-request/cx/~nul/desk/~1234.5.6/foo/bar
                %c  %warp  [~nul ~nul]  %desk
                ~  %sing  %x  [%da ~1234.5.6]  /foo/bar
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
        :*  wire=/~nul/scry-request/cx/~nul/desk/~1234.5.6/foo/bar  duct=~
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
  =/  ford  *ford-gate
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
                wire=/~nul/scry-request/cx/~nul/desk/~1234.5.6/foo/bar
                %c  %warp  [~nul ~nul]  %desk
                ~  %sing  %x  [%da ~1234.5.6]  /foo/bar
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
        :*  wire=/~nul/scry-request/cx/~nul/desk/~1234.5.6/foo/bar  duct=~
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
  =/  ford  *ford-gate
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
  =/  ford  *ford-gate
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
  =/  ford  *ford-gate
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
      :-  [%cx [[~nul %home %da ~1234.5.6] /bar/foo]]
      [%noun scry-type %it-does-in-fact-matter]
    ::
      :-  [%cx [[~nul %home %da ~1234.5.7] /bar/foo]]
      [%noun scry-type %it-does-in-fact-matter]
    ==
  ::
  =/  scry  (scry-with-results scry-results)
  =/  ford  *ford-gate
  ::
  =/  formula=hoon  (ream '`@tas`%constant')
  =/  subject-schematic=schematic:ford  [%scry %c %x [~nul %home] /bar/foo]
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
            :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/home
                %c  %warp  [~nul ~nul]  %home
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
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/home
                %c  %warp  [~nul ~nul]  %home  ~
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
++  test-call
  ~&  %test-call
  =/  ford  *ford-gate
  ::
  =/  sample-schematic=schematic:ford  [%$ %noun !>(5)]
  =/  gate-schematic=schematic:ford  [%$ %noun !>(|=(a=@ud +(a)))]
  =/  call-schematic=schematic:ford  [%call gate-schematic sample-schematic]
  ::
  =^  results1  ford
    %-  test-ford-call-with-comparator  :*
      ford
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/call] type=~ %make ~nul call-schematic]
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete %success %call *] i.moves)
        ::
        =/  result=vase  |7:i.moves
        ::
        %+  weld
          %-  expect-eq  !>
          [6 q.result]
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.result) | -:!>(*@ud))
    ==
  ::
  =^  results2  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.7
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/call] type=~ %kill ~nul]
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
++  test-call-scry-succeed
  ~&  %test-call-scry-succeed
  ::
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  =/  ford  *ford-gate
  ::
  =/  sample-schematic=schematic:ford  [%$ %noun !>(24)]
  =/  gate-schematic=schematic:ford
    [%$ %noun !>(|=(a=@ud .^(@ud %cx /~nul/desk/~1234.5.6/foo/bar)))]
  =/  call-schematic=schematic:ford  [%call gate-schematic sample-schematic]
  ::
  =^  results1  ford
    %-  test-ford-call-with-comparator  :*
      ford
      now=~1234.5.6
      scry=scry-42
      ::
      call-args=[duct=~[/call] type=~ %make ~nul call-schematic]
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete %success %call *] i.moves)
        ::
        =/  result=vase  |7:i.moves
        ::
        %+  weld
          %-  expect-eq  !>
          [42 q.result]
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.result) | -:!>(*@ud))
    ==
  ::
  =^  results2  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.7
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/call] type=~ %kill ~nul]
      ::
      moves=~
    ==
  ::
  ;:  weld
    results1
    results2
    (expect-ford-empty ford ~nul)
  ==
++  test-call-scry-fail
  ~&  %test-call-scry-fail
  ::
  =/  scry-failed  (scry-fail ~1234.5.6)
  =/  ford  *ford-gate
  ::
  =/  sample-schematic=schematic:ford  [%$ %noun !>(24)]
  =/  gate-schematic=schematic:ford
    [%$ %noun !>(|=(a=@ud .^(@ud %cx /~nul/desk/~1234.5.6/foo/bar)))]
  =/  call-schematic=schematic:ford  [%call gate-schematic sample-schematic]
  ::
  =^  results1  ford
    %-  test-ford-call-with-comparator  :*
      ford
      now=~1234.5.6
      scry=scry-failed
      ::
      call-args=[duct=~[/dead] type=~ %make ~nul call-schematic]
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete %error *] i.moves)
        ::
        %-  expect-eq  !>
        ::  compare the move to the expected move, omitting check on stack trace
        ::
        :_  i.moves(|7 ~)
        :*  duct=~[/dead]  %give  %made  ~1234.5.6  %complete
            [%error [leaf+"ford: %call failed:" ~]]
    ==  ==
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
::
++  test-call-scry-block
  ~&  %test-call-scry-block
  ::
  =/  scry-blocked  (scry-block ~1234.5.6)
  =/  ford  *ford-gate
  ::
  =/  sample-schematic=schematic:ford  [%$ %noun !>(24)]
  =/  gate-schematic=schematic:ford
    [%$ %noun !>(|=(a=@ud .^(@ud %cx /~nul/desk/~1234.5.6/foo/bar)))]
  =/  call-schematic=schematic:ford  [%call gate-schematic sample-schematic]
  ::
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry-blocked
      ::
      call-args=[duct=~[/live] type=~ %make ~nul call-schematic]
      ::
      ^=  moves
        :~  :*  duct=~  %pass
                wire=/~nul/scry-request/cx/~nul/desk/~1234.5.6/foo/bar
                %c  %warp  [~nul ~nul]  %desk
                ~  %sing  %x  [%da ~1234.5.6]  /foo/bar
    ==  ==  ==
  ::
  =^  results2  ford
    %-  test-ford-take-with-comparator  :*
      ford
      now=~1234.5.7
      scry=scry-blocked
      ::
      ^=  call-args
        :*  wire=/~nul/scry-request/cx/~nul/desk/~1234.5.6/foo/bar  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type  ::  ^-  sign:ford
            [%c %writ ~ [%x [%da ~1234.5.6] %desk] /bar/foo %noun !>(42)]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete %success %call *] i.moves)
        ::
        %+  welp
          %-  expect-eq  !>
          ::  compare the move to the expected move, omitting vase type checking
          ::
          ::    Types can't be compared using simple equality, so normalize the
          ::    type to check the rest of the move.
          ::
          :_  i.moves(&8 *type)
          :*  duct=~[/live]  %give  %made  ~1234.5.6  %complete
              [%success [%call *type 42]]
          ==
        ::  make sure the types nest
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut &8:i.moves) | -:!>(*@))
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
::  +test-call-scry-varies: call with an argument which varies
::
::    This test reads the sample for a %call schematic from clay. This sample
::    is a date. Inside of the gate called, we scry on a path based on the
::    passed in sample date.
::
++  test-call-scry-varies
  ~&  %test-call-scry-varies
  ::
  =/  date-type=type  [%atom %da ~]
  =/  term-type=type   [%atom %tas ~]
  ::
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %desk %da ~1234.5.6] /timer]]
      [%noun date-type ~1234.5.6]
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.6] /result]]
      [%noun term-type %first]
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.7] /timer]]
      [%noun date-type ~1234.5.7]
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.7] /result]]
      [%noun term-type %second]
    ==
  ::
  =/  scry  (scry-with-results scry-results)
  =/  ford  *ford-gate
  ::
  =/  sample-schematic=schematic:ford
    [%scry [%c care=%x bel=[[~nul %desk] /timer]]]
  =/  gate-schematic=schematic:ford
    [%$ %noun !>(|=(a=@da .^(@tas %cx /~nul/desk/(scot %da a)/result)))]
  =/  call-schematic=schematic:ford  [%call gate-schematic sample-schematic]
  ::
  =^  results1  ford
    %-  test-ford-call-with-comparator  :*
      ford
      now=~1234.5.6
      scry=scry
      ::
      ^=  call-args
        [duct=~[/call] type=~ %make ~nul call-schematic]
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  =(2 (lent moves))
        ?>  ?=([^ ^ ~] moves)
        ?>  ?=([* %give %made @da %complete %success %call *] i.moves)
        ::  compare the move to the expected move, omitting vase type checking
        ::
        %+  weld
          %-  expect-eq  !>
          :_  i.moves(&8 *type)
          :*  duct=~[/call]  %give  %made  ~1234.5.6  %complete  %success
              %call  *type  %first
          ==
        ::  make sure the types nest
        %+  weld
          %-  expect-eq  !>
          :-  &
          (~(nest ut &8:i.moves) | -:!>(*@tas))
        ::  make sure the other move is a subscription
        ::
        %-  expect-eq  !>
        :_  i.t.moves
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.6] (sy [%x /timer] ~)]
    ==  ==
  ::
  =^  results2  ford
    %-  test-ford-take-with-comparator  :*
      ford
      now=~1234.5.7
      scry=scry
      ::
      ^=  call-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /timer]~)]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  =(2 (lent moves))
        ?>  ?=([^ ^ ~] moves)
        ?>  ?=([* %give %made @da %complete %success %call *] i.moves)
        ::  compare the move to the expected move, omitting vase type checking
        ::
        %+  weld
          %-  expect-eq  !>
          :_  i.moves(&8 *type)
          :*  duct=~[/call]  %give  %made  ~1234.5.7  %complete  %success
              %call  *type  %second
          ==
        ::  make sure the types nest
        %+  weld
          %-  expect-eq  !>
          :-  &
          (~(nest ut &8:i.moves) | -:!>(*@tas))
        ::  make sure the other move is a subscription
        ::
        %-  expect-eq  !>
        :_  i.t.moves
        :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.7] (sy [%x /timer] ~)]
    ==  ==
  ::
  =^  results3  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/call] type=~ %kill ~nul]
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
++  test-dude
  ~&  %test-dude
  ::
  =/  ford  *ford-gate
  ::
  =/  schematic=schematic:ford
    :*  %pin  ~1234.5.6
        %dude  |.(>%test-no-error<)
        [%scry [%c care=%x bel=[[~nul %desk] /bar/foo]]]
    ==
  ::
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  ::
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry-42
      ::
      call-args=[duct=~[/once] type=~ %make ~nul schematic]
      ::
      ^=  moves
        :~  :*  duct=~[/once]  %give  %made  ~1234.5.6  %complete  %success
                [%pin ~1234.5.6 %success %dude %success [%scry %noun !>(42)]]
    ==  ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford ~nul)
  ==
::
++  test-dude-error
  ~&  %test-dude-error
  ::
  =/  ford  *ford-gate
  ::
  =/  schematic=schematic:ford
    :*  %pin  ~1234.5.6
        %dude  |.(>%in-the-error-message<)
        [%scry [%c care=%x bel=[[~nul %desk] /bar/foo]]]
    ==
  ::
  =/  scry-42  (scry-fail ~1234.5.6)
  ::
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry-42
      ::
      call-args=[duct=~[/once] type=~ %make ~nul schematic]
      ::
      ^=  moves
        :~  :*  duct=~[/once]  %give  %made  ~1234.5.6  %complete
                %success  %pin  ~1234.5.6  %error
                :~  leaf+"%in-the-error-message"
                    leaf+"scry failed for"
                    leaf+"%cx /~nul/desk/~1234.5.6/foo/bar"
    ==  ==  ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford ~nul)
  ==
::
++  test-hood
  ~&  %test-hood
  ::
  =/  ford  *ford-gate
  ::
  =/  scry-type=type  [%atom %tas ~]
  =/  scry
    %-  scry-with-results
    ^-  (map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %desk %da ~1234.5.6] /hoon/bar/foo]]
      :*  %noun  scry-type
          '!.  |=(a=@ud +(a))'
      ==
    ==
  ::
  =/  schematic=schematic:ford  [%hood [[~nul %desk] /hoon/bar/foo]]
  ::
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/hood] type=~ %make ~nul schematic]
      ::
      ^=  moves
        :~  :*  duct=~[/hood]  %give  %made  ~1234.5.6  %complete  %success  %hood
                :*  zuse-version=309
                    structures=~
                    libraries=~
                    cranes=~
                    ^=  sources
                      :~  :*  %direct
                              %dbug
                              [/~nul/desk/0/foo/bar/hoon [1 1] [1 19]]
                              (ream '|=(a=@ud +(a))')
            ==  ==    ==  ==
            :*  duct=~  %pass  /~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar/hoon] ~)]
    ==  ==  ==
  ::
  results1
::
++  test-slim
  ~&  %test-slim
  ::
  =/  formula=hoon  (ream '(add 2 2)')
  =/  subject-type=type  -:!>(.)
  ::
  =/  ford  *ford-gate
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
++  test-slit
  ~&  %test-slit
  ::
  =/  gate=vase    (ride %noun '|=(a=@ud ["one" a])')
  =/  sample=vase  !>(42)
  ::
  =/  ford  *ford-gate
  =^  results1  ford
    %-  test-ford-call-with-comparator  :*
      ford
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/slit] type=~ %make ~nul [%slit gate sample]]
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete %success %slit *] i.moves)
        ::  we are expecting a type, and all we can do is ensure it nests in
        ::  the right type
        ::
        =/  expected-type=type  -:!>([*tape *@ud])
        =/  actual-type=type    |7:i.moves
        %-  expect-eq  !>
        :-  &
        (~(nest ut actual-type) | expected-type)
    ==
  ::
  =^  results2  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.7
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/slit] type=~ %kill ~nul]
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
++  test-slit-error
  ~&  %test-slit-error
  ::
  =/  gate=vase    (ride %noun '|=(a=@ud ["one" a])')
  =/  sample=vase  !>("a tape instead of @ud")
  ::
  =/  ford  *ford-gate
  =^  results1  ford
    %-  test-ford-call-with-comparator  :*
      ford
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/slit] type=~ %make ~nul [%slit gate sample]]
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete %error *] i.moves)
        ::  ignore last message; contains source positions in the stack trace
        ::
        =/  messages=tang  (scag 4 `tang`|6:i.moves)
        ::
        %-  expect-eq  !>
        :_  messages
        :~  [%palm ["." "-" "" ""] [%leaf "have"] [%leaf "\"\""] ~]
            :~  %palm  ["." "-" "" ""]
                [%leaf "want"]
                [%palm ["/" "" "" ""] [%leaf "a"] [%leaf "@ud"] ~]
            ==
            [%leaf "%slit failed: "]
            [%leaf "nest-fail"]
        ==
    ==
  ::
  =^  results2  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.7
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/slit] type=~ %kill ~nul]
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
  =/  ford  *ford-gate
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
        |=  moves=(list move:ford-gate)
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
          :_  i.moves(&8 *type)
          :*  duct=~[/dead]  %give  %made  ~1234.5.6  %complete
              [%success [%ride *type 5]]
          ==
        ::  make sure the returned type nests
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut &8:i.moves) | -:!>(*@))
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
  =/  ford  *ford-gate
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
        |=  moves=(list move:ford-gate)
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
          :_  i.moves(&8 *type)
          :*  duct=~[/dead]  %give  %made  ~1234.5.6  %complete
              [%success [%ride *type 42]]
          ==
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut &8:i.moves) | -:!>(*@))
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
  =/  ford  *ford-gate
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
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete %error *] i.moves)
        ::
        %-  expect-eq  !>
        ::  compare the move to the expected move, omitting check on stack trace
        ::
        :_  i.moves(|7 ~)
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
  =/  ford  *ford-gate
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
                wire=/~nul/scry-request/cx/~nul/desk/~1234.5.6/foo/bar
                %c  %warp  [~nul ~nul]  %desk
                ~  %sing  %x  [%da ~1234.5.6]  /foo/bar
    ==  ==  ==
  ::
  =^  results2  ford
    %-  test-ford-take-with-comparator  :*
      ford
      now=~1234.5.7
      scry=scry-blocked
      ::
      ^=  take-args
        :*  wire=/~nul/scry-request/cx/~nul/desk/~1234.5.6/foo/bar  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type  ::  ^-  sign:ford
            [%c %writ ~ [%x [%da ~1234.5.6] %desk] /bar/foo %noun !>(42)]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
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
          :_  i.moves(&8 *type)
          :*  duct=~[/live]  %give  %made  ~1234.5.6  %complete
              [%success [%ride *type 42]]
          ==
        ::  make sure the types nest
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut &8:i.moves) | -:!>(*@))
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
  =/  ford  *ford-gate
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
  =/  ford  *ford-gate
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
        |=  moves=(list move:ford-gate)
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
        :_  i.t.moves
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
        |=  moves=(list move:ford-gate)
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
        |=  moves=(list move:ford-gate)
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
  =/  ford  *ford-gate
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
  =/  ford  *ford-gate
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
  =/  ford  *ford-gate
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
  =/  ford  *ford-gate
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
  =/  ford  *ford-gate
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
  =/  ford  *ford-gate
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
::  tests that doing a cache reclamation during the five-oh-fora rebuild works
::
++  test-five-oh-cache-reclamation
  ~&  %test-five-oh-cache-reclamation
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
      :-  [%cx [[~nul %desk %da ~1234.5.9] /a/posts]]
      [%noun scry-type [title='post-a' contents="post-a-contents-changed"]]
    ::
      ::  unchanged, but might be requested if cache entry gets wiped
      ::
      :-  [%cx [[~nul %desk %da ~1234.5.9] /b/posts]]
      [%noun scry-type [title='post-b' contents="post-b-contents"]]
    ==
  ::
  =/  scry  (scry-with-results scry-results)
  =/  ford  *ford-gate
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
        |=  moves=(list move:ford-gate)
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
        :_  i.t.moves
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
        |=  moves=(list move:ford-gate)
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
    %-  test-ford-call  :*
      ford
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~ type=~ %wipe ~]
      moves=~
    ==
  ::
  =^  results4  ford
    %-  test-ford-take-with-comparator  :*
      ford
      now=~1234.5.9
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford)  :-  *type
            [%c %wris [%da ~1234.5.9] (sy [%x /posts/a]~)]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        =+  length=(lent moves)
        ::  deal with mug ordering
        ::
        ::    This test depends on the mugs of types stored in ford, which is
        ::    dependent on the parse tree of this file. There are two valid
        ::    responses, one where we send a spurious move about %post-b
        ::    because it was the entry which was evicted from the cache and one
        ::    where we don't because it wasn't. Check both cases.
        ::
        ?:  =(length 2)
          ::  the simple case where we don't send a spurious post-b %made
          ::
          ?>  ?=([^ ^ ~] moves)
          ::
          %-  check-post-made  :*
            move=i.moves
            duct=~[/post-a]
            type=scry-type
            date=~1234.5.9
            title='post-a'
            contents="post-a-contents-changed"
          ==
        :: the complex case
        ::
        ::   Not only do we send a spurious %made, we don't have a set order
        ::   that these events happen in, so check both ways.
        ::
        ?>  ?=([^ ^ ^ ~] moves)
        ::
        =/  post-a-first=tang
          %+  welp
            %-  check-post-made  :*
              move=i.moves
              duct=~[/post-a]
              type=scry-type
              date=~1234.5.9
              title='post-a'
              contents="post-a-contents-changed"
            ==
          %-  check-post-made  :*
            move=i.t.moves
            duct=~[/post-b]
            type=scry-type
            date=~1234.5.9
            title='post-b'
            contents="post-b-contents"
          ==
        ::  if we got a ~ for post-a-first, everything is fine
        ::
        ?~  post-a-first
          ~
        ::  otherwise, its either post-b first or an error.
        ::
        ::    Either way, return post-b first check.
        ::
        %+  welp
          %-  check-post-made  :*
            move=i.moves
            duct=~[/post-b]
            type=scry-type
            date=~1234.5.9
            title='post-b'
            contents="post-b"
          ==
        ::
        %-  check-post-made  :*
          move=i.t.moves
          duct=~[/post-a]
          type=scry-type
          date=~1234.5.9
          title='post-a'
          contents="post-a-contents-changed"
    ==  ==
  ::
  =^  results5  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.10
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/post-b] type=~ %kill ~nul]
      moves=~
    ==
  ::
  =^  results6  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.11
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
    results6
    (expect-ford-empty ford ~nul)
  ==
::
++  test-reef
  ~&  %test-reef
  ::
  =/  ford  *ford-gate
  =/  hoon-beam-path=path  (en-beam:format [bek /hoon/hoon/sys])
  =/  hoon-txt=@t  .^(@t %cx hoon-beam-path)
  =/  hoon-parsed=hoon  (rain hoon-beam-path hoon-txt)
  ~&  %parsed-hoon
  ::
  =/  arvo-beam-path=path  (en-beam:format [bek /hoon/arvo/sys])
  =/  arvo-txt=@t  .^(@t %cx arvo-beam-path)
  =/  arvo-parsed=hoon  (rain arvo-beam-path arvo-txt)
  ~&  %parsed-arvo
  ::
  =/  zuse-beam-path=path  (en-beam:format [bek /hoon/zuse/sys])
  =/  zuse-txt=@t  .^(@t %cx zuse-beam-path)
  =/  zuse-parsed=hoon  (rain zuse-beam-path zuse-txt)
  ~&  %parsed-zuse
  ::
  =/  pit=vase  !>(~)
  =/  hoon-compiled=vase  (slap pit hoon-parsed)
  ~&  %hoon-compiled
  =/  arvo-compiled=vase  (slap hoon-compiled arvo-parsed)
  ~&  %arvo-compiled
  =/  zuse-compiled=vase  (slap arvo-compiled zuse-parsed)
  ~&  %zuse-compiled
  ::
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %base %da ~1234.5.6] /hoon/hoon/sys]]
      [%noun !>(hoon-txt)]
    ::
      :-  [%cx [[~nul %base %da ~1234.5.6] /hoon/arvo/sys]]
      [%noun !>(arvo-txt)]
    ::
      :-  [%cx [[~nul %base %da ~1234.5.6] /hoon/zuse/sys]]
      [%noun !>(zuse-txt)]
    ==
  ::
  =^  results1  ford
    %-  test-ford-call-with-comparator  :*
      ford
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/reef]  type=~  %make  ~nul
            [%pin ~1234.5.6 [%reef [~nul %base]]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=(%complete -.result)
        ?>  ?=([%success %pin @da %success %reef *] +.result)
        ::
        =/  kernel=vase  |5:+.result
        ::
        %+  weld
          %-  expect-eq  !>
          :-  (en-beam:format *beam)
          q:(slym (slap (slap kernel [%limb %format]) [%limb %en-beam]) *beam)
        ::
        %-  expect-eq  !>
        :-  &
        (slab %format p.kernel)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford ~nul)
  ==
::
++  test-reef-short-circuit
  ~&  %test-reef-short-circuit
  ::
  =/  ford  *ford-gate
  ::
  =^  results1  ford
    %-  test-ford-call-with-comparator  :*
      ford
      now=~1234.5.6
      ^=  scry
        |=  [* * =term =beam]
        ^-  (unit (unit cage))
        ::
        ~|  [term=term beam=beam]
        ?>  =(%cw term)
        ?>  =([[~nul %home [%da ~1234.5.6]] /hoon/hoon/sys] beam)
        ::
        ``[%cass !>(`cass:clay`[ud=1 da=~1234.5.6])]
      ::
      ^=  call-args
        :*  duct=~[/reef]  type=~  %make  ~nul
            [%pin ~1234.5.6 [%reef [~nul %home]]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=(%complete -.result)
        ?>  ?=([%success %pin @da %success %reef *] +.result)
        ::
        =/  kernel=vase  |5:+.result
        ::
        %+  weld
          %-  expect-eq  !>
          :-  (en-beam:format *beam)
          q:(slym (slap (slap kernel [%limb %format]) [%limb %en-beam]) *beam)
        ::
        %-  expect-eq  !>
        :-  &
        (slab %format p.kernel)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford ~nul)
  ==
 
::
++  test-path
  ~&  %test-path
  ::
  =/  ford  *ford-gate
  ::
  =/  scry-results=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %desk %da ~1234.5.6] /hoon/bar/foo/lib]]
      `[%hoon !>(*hoon)]
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.6] /hoon/foo-bar/lib]]
      ~
    ==
  ::
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %make  ~nul
            %pin  ~1234.5.6
            [%path disc=[~nul %desk] prefix='lib' raw-path='foo-bar']
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/path]  %give  %made  ~1234.5.6  %complete
                %success  %pin  ~1234.5.6
                %success  %path  [[~nul %desk] /hoon/bar/foo/lib]
    ==  ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford ~nul)
  ==
::
++  test-plan-direct-hoon
  ~&  %test-plan-direct-hoon
  ::
  =/  ford  *ford-gate
  ::
  =/  =hoon  (ream '`@tas`%constant')
  ::
  =^  results1  ford
    %-  test-ford-call-with-comparator  :*
      ford
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      ^=  call-args
        :*  duct=~[/plan]  type=~  %make  ~nul
            %pin  ~1234.5.6
            %plan
            source-path=[[~nul %home] /bar/foo]
            query-string=`coin`[%$ *dime]
            zuse-version=309
            structures=~
            libraries=~
            cranes=~
            sources=[%direct hoon]~
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=(%complete -.result)
        ?>  ?=([%success %pin @da %success %plan *] +.result)
        ::
        =/  =vase  |5:+.result
        ::
        %+  weld
          %-  expect-eq  !>
          :-  &
          (~(nest ut p.vase) | -:!>(*@tas))
        ::
        %-  expect-eq  !>
        [%constant q.vase]
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford ~nul)
  ==
::
++  test-core
  ~&  %test-core
  ::
  =/  ford  *ford-gate
  ::
  =/  hoon-src  '`@tas`%constant'
  =/  hoon-src-type=type  [%atom %$ ~]
  ::
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/foo-bar/lib]]
      [%hoon hoon-src-type hoon-src]
    ==
  ::
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %make  ~nul
            %pin  ~1234.5.6
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/foo-bar/lib]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/path]  %give  %made  ~1234.5.6  %complete
                %success  %pin  ~1234.5.6
                %success  %core  [%atom %tas ~]  %constant
    ==  ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford ~nul)
  ==
::
++  test-core-linker
  ~&  %test-core-linker
  ::
  =/  ford  *ford-gate
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/data/sur]]
      :-  %hoon
      :-  hoon-src-type
      '''
      |%
      +=  data-type
        [msg=tape count=@ud]
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/data/lib]]
      :-  %hoon
      :-  hoon-src-type
      '''
      /-  data
      |%
      ++  do
        |=  [a=data-type:data b=data-type:data]
        ^-  data-type:data
        [(weld msg.a msg.b) (add count.a count.b)]
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/program/gen]]
      :-  %hoon
      :-  hoon-src-type
      '''
      /-  *data
      /+  combiner=data
      (do:combiner `data-type`["one" 1] `data-type`["two" 2])
      '''
    ==
  ::
  =^  results1  ford
    %-  test-ford-call-with-comparator  :*
      ford
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %make  ~nul
            %pin  ~1234.5.6
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/program/gen]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete %success %pin *] i.moves)
        =/  result  result.p.card.i.moves
        =/  pin-result  build-result.result
        ?>  ?=([%success %core *] build-result.pin-result)
        ::
        =/  =vase  vase.build-result.pin-result
        ::
        %+  weld
          %-  expect-eq  !>
          :-  ["onetwo" 3]
          q.vase
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.vase) | -:!>(["onetwo" 3]))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford ~nul)
  ==
::
++  test-core-fsts-fssg
  ~&  %test-core-fsts-fssg
  ::
  =/  ford  *ford-gate
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/program/gen]]
      :-  %hoon
      :-  hoon-src-type
      '''
      /=  one  /~  `@u`1
      /=  two  /~  `@u`2
      (add one two)
      '''
    ==
  ::
  =^  results1  ford
    %-  test-ford-call-with-comparator  :*
      ford
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %make  ~nul
            %pin  ~1234.5.6
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/program/gen]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete %success %pin *] i.moves)
        =/  result  result.p.card.i.moves
        =/  pin-result  build-result.result
        ?>  ?=([%success %core *] build-result.pin-result)
        ::
        =/  =vase  vase.build-result.pin-result
        ::
        %+  weld
          %-  expect-eq  !>
          :-  3
          q.vase
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.vase) | -:!>([3]))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford ~nul)
  ==
::
++  test-core-fsdt-fskt
  ~&  %test-core-fsdt-fskt
  ::
  =/  ford  *ford-gate
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/program/gen]]
      :-  %hoon
      :-  hoon-src-type
      '''
      /=  data  /^  (list @ud)
                /.  /~  1
                    /~  2
                    /~  3
                    ==
      (weld data [4 5 ~])
      '''
    ==
  ::
  =^  results1  ford
    %-  test-ford-call-with-comparator  :*
      ford
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %make  ~nul
            %pin  ~1234.5.6
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/program/gen]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete %success %pin *] i.moves)
        =/  result  result.p.card.i.moves
        =/  pin-result  build-result.result
        ?>  ?=([%success %core *] build-result.pin-result)
        ::
        =/  =vase  vase.build-result.pin-result
        ::
        %+  weld
          %-  expect-eq  !>
          :-  [1 2 3 4 5 ~]
          q.vase
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.vase) | -:!>([1 2 3 4 5 ~]))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford ~nul)
  ==
::
++  test-core-fskt-nest-fail
  ~&  %test-core-fskt-nest-fail
  ::
  =/  ford  *ford-gate
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/program/gen]]
      :-  %hoon
      :-  hoon-src-type
      '''
      /=  data  /^  (list @u)
                /~  5
      data
      '''
    ==
  ::
  =^  results1  ford
    %-  test-ford-call  :*
      ford
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %make  ~nul
            %pin  ~1234.5.6
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/program/gen]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/path]  %give  %made  ~1234.5.6  %complete  %success
                %pin  ~1234.5.6  %error
                :~  [%leaf "ford: %core failed: "]
                    [%leaf "/^ failed: nest-fail"]
    ==  ==  ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford ~nul)
  ==
::
++  test-core-fssm
  ~&  %test-core-fssm
  ::
  =/  ford  *ford-gate
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/program/gen]]
      :-  %hoon
      :-  hoon-src-type
      '''
      /=  data  /;  |=(a=@u [a a ~])
                /~  5
      data
      '''
    ==
  ::
  =^  results1  ford
    %-  test-ford-call-with-comparator  :*
      ford
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %make  ~nul
            %pin  ~1234.5.6
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/program/gen]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete %success %pin *] i.moves)
        =/  result  result.p.card.i.moves
        =/  pin-result  build-result.result
        ?>  ?=([%success %core *] build-result.pin-result)
        ::
        =/  =vase  vase.build-result.pin-result
        ::
        %+  weld
          %-  expect-eq  !>
          :-  [5 5 ~]
          q.vase
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.vase) | -:!>([5 5 ~]))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford ~nul)
  ==
::
::  |utilities: helper arms
::
::+|  utilities
++  check-post-made
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
    %-  expect-eq  !>
    [duct duct.move]
  ::
    %-  expect-eq  !>
    [date date.p.card.move]
  ::
    %-  expect-eq  !>
    :_  head.result(p.q.cage *^type)
    [%success %scry %noun *^type [title=title contents=contents]]
  ::
    %-  expect-eq  !>
    :-  &
    (~(nest ut p.q.cage.head.result) | type)
  ::
    %-  expect-eq  !>
    :-  [%success %ride *^type 'post-a']
    head.tail.result(p.vase *^type)
  ::
    %-  expect-eq  !>
    :-  &
    (~(nest ut p.vase.head.tail.result) | -:!>(''))
  ::
    %-  expect-eq  !>
    :-  [%success %ride *^type 'post-b']
    tail.tail.result(p.vase *^type)
  ::
    %-  expect-eq  !>
    :-  &
    (~(nest ut p.vase.tail.tail.result) | -:!>(''))
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
  ~|  scry-is-forbidden+[beam+beam term+term]
  !!
::
++  test-ford-call
  |=  $:  ford=_(ford-gate)
          now=@da
          scry=sley
          call-args=[=duct type=* wrapped-task=(hobo task:able:ford-gate)]
          expected-moves=(list move:ford-gate)
      ==
  ^-  [tang _(ford-gate)]
  ::
  =.  ford  (ford now=now eny=0xdead.beef scry=scry)
  ::
  =^  moves  ford
    %-  call:ford  call-args
  ::
  =/  output=tang
    %-  expect-eq  !>
    :-  expected-moves
    moves
  ::
  [output ford]
::
++  test-ford-take
  |=  $:  ford=_(ford-gate)
          now=@da
          scry=sley
          take-args=[=wire =duct wrapped-sign=(hypo sign:ford-gate)]
          expected-moves=(list move:ford-gate)
      ==
  ^-  [tang _(ford-gate)]
  ::
  =.  ford  (ford now=now eny=0xdead.beef scry=scry)
  ::
  =^  moves  ford
    %-  take:ford  take-args
  ::
  =/  output=tang
    %-  expect-eq  !>
    :-  expected-moves
    moves
  ::
  [output ford]
::  +test-ford-call-with-comparator
::
::    Sometimes we can't just do simple comparisons between the moves statements and
::    must instead specify a gate that performs the comparisons.
::
++  test-ford-call-with-comparator
  |=  $:  ford=_(ford-gate)
          now=@da
          scry=sley
          call-args=[=duct type=* wrapped-task=(hobo task:able:ford-gate)]
          move-comparator=$-((list move:ford-gate) tang)
      ==
  ^-  [tang _(ford-gate)]
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
  |=  $:  ford=_(ford-gate)
          now=@da
          scry=sley
          take-args=[=wire =duct wrapped-sign=(hypo sign:ford-gate)]
          move-comparator=$-((list move:ford-gate) tang)
      ==
  ^-  [tang _(ford-gate)]
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
  |=  [ford=_(ford-gate) ship=@p]
  ^-  tang
  %-  expect-eq  !>
  :-  (my [ship *ford-state:ford-gate]~)
  state-by-ship.+>+<.ford
--
