/+  tester
::
/=  ford-turbo  /:  /===/sys/vane/turbo
                /!noun/
::
=,  ford-api
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
  test-parse-scaffold
  test-parse-scaffold-sur-lib
  test-parse-scaffold-zuse-version
  test-parse-scaffold-crane-fssg
  test-parse-scaffold-crane-fsbc
  test-parse-scaffold-crane-fsbr
  test-parse-scaffold-crane-fsts
  test-parse-scaffold-crane-fsdt
  test-parse-scaffold-crane-fscm
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
  test-plan-hoon
  test-core
  test-core-linker
  test-core-multi-hoon
  test-core-fsts-fssg
  test-core-fsdt-fskt
  test-core-fskt-nest-fail
  test-core-fssm
  test-core-fsbr
  test-core-fsbr-out-of-options
  test-plan-fszp-as-noun
  test-core-fszp-as-mark
  test-core-fscl-fszp
  test-core-fscm
  test-plan-fsbc
  test-core-fscb
  test-core-fspm
  test-core-fszy-renderer
  test-bunt
  test-volt
  test-vale
  test-vale-error
  test-cast
  test-cast-grow
  test-mute
  test-bake-renderer
  test-bake-mark
  test-diff
  test-diff-form
  test-pact
  test-pact-mark
  test-join
  test-list
  test-mash
  test-multi-core-same-dependency
  test-walk-prefer-grab
  test-walk-large-graph
==
++  test-tear
  :-  `tank`leaf+"test-tear"
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
  :-  `tank`leaf+"test-is-schematic-live"
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
  :-  `tank`leaf+"test-date-from-schematic"
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
  :-  `tank`leaf+"test-unify-jugs"
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
  :-  `tank`leaf+"test-resource-wire-encoding"
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
++  test-parse-scaffold
  :-  `tank`leaf+"test-parse-scaffold"
  ::
  %-  expect-eq  !>
  :-  :-  [1 19]
      :-  ~
      :_  [[1 19] ""]
      ^-  scaffold:ford-gate
      :*  source-rail=[[~nul %desk] /bar/foo]
          zuse-version=%309
          structures=~
          libraries=~
          cranes=~
          ^=  sources
            :*  %dbug  [/~nul/desk/~1234.5.6/foo/bar [[1 1] [1 19]]]
                (ream '!.  |=(a=@ud +(a))')
            ==
          ~
      ==
  %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /bar/foo]))
    [1 1]
  "!.  |=(a=@ud +(a))"
::
++  test-parse-scaffold-sur-lib
  :-  `tank`leaf+"test-parse-scaffold-sur-lib"
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /bar/foo]))
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
  :*  source-rail=[[~nul %desk] /bar/foo]
      zuse-version=309
      structures=~[[`%struct %struct] [`%face %other]]
      libraries=~[[`%library %library] [~ %thing]]
      cranes=~
      ^=  sources
        :~  :*  %dbug
                [/~nul/desk/~1234.5.6/foo/bar [3 1] [4 8]]
                (ream '|=(a a)')
  ==    ==  ==
::
++  test-parse-scaffold-zuse-version
  :-  `tank`leaf+"test-parse-scaffold-zuse-version"
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /bar/foo]))
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
  :*  source-rail=[[~nul %desk] /bar/foo]
      zuse-version=400
      structures=~
      libraries=~
      cranes=~
      ^=  sources
        :~  :*  %dbug
                [/~nul/desk/~1234.5.6/foo/bar [2 1] [3 8]]
                (ream '|=(a a)')
  ==    ==  ==
::
++  test-parse-scaffold-crane-fssg
  :-  `tank`leaf+"test-parse-scaffold-crane-fssg"
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /bar/foo]))
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
  :*  source-rail=[[~nul %desk] /bar/foo]
      zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fssg
                %dbug
                [/~nul/desk/~1234.5.6/foo/bar [1 5] [1 18]]
                (ream '[a=1 b=3]')
        ==  ==
      ^=  sources
        :~  :*  %dbug
                [/~nul/desk/~1234.5.6/foo/bar [2 1] [3 8]]
                (ream '|=(a b)')
  ==    ==  ==
::
++  test-parse-scaffold-crane-fsbc
  :-  `tank`leaf+"test-parse-scaffold-crane-fsbc"
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /bar/foo]))
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
  :*  source-rail=[[~nul %desk] /bar/foo]
      zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fsbc
                %dbug
                [/~nul/desk/~1234.5.6/foo/bar [1 5] [1 16]]
                (ream '|=(a a)')
        ==  ==
      ^=  sources
        :~  :*  %dbug
                [/~nul/desk/~1234.5.6/foo/bar [2 1] [3 8]]
                (ream '|=(a b)')
  ==    ==  ==
::
++  test-parse-scaffold-crane-fsbr
  :-  `tank`leaf+"test-parse-scaffold-crane-fsbr"
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /bar/foo]))
      [1 1]
    """
    /|  /~  ~
        /~  ~
        ==
    5
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  source-rail=[[~nul %desk] /bar/foo]
      zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fsbr
                :~  :*  %fssg  %dbug
                        [/~nul/desk/~1234.5.6/foo/bar [1 9] [1 10]]
                        [%bust %null]
                    ==
                    :*  %fssg  %dbug
                        [/~nul/desk/~1234.5.6/foo/bar [2 9] [2 10]]
                        [%bust %null]
        ==  ==  ==  ==
      ^=  sources
        :~  [%dbug [/~nul/desk/~1234.5.6/foo/bar [4 1] [4 2]] [%sand %ud 5]]
  ==    ==
::
++  test-parse-scaffold-crane-fsts
  :-  `tank`leaf+"test-parse-scaffold-crane-fsts"
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /bar/foo]))
      [1 1]
    """
    /=  a  /~  ~
    5
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  source-rail=[[~nul %desk] /bar/foo]
      zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fsts  %a
                %fssg  %dbug  [/~nul/desk/~1234.5.6/foo/bar [1 12] [1 13]]
                [%bust %null]
        ==  ==
      ^=  sources
        :~  [%dbug [/~nul/desk/~1234.5.6/foo/bar [2 1] [2 2]] [%sand %ud 5]]
  ==    ==
::
++  test-parse-scaffold-crane-fsdt
  :-  `tank`leaf+"test-parse-scaffold-crane-fsdt"
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /bar/foo]))
      [1 1]
    """
    /.  /~  !.  a=5
        /~  !.  b=6
        ==
    5
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  source-rail=[[~nul %desk] /bar/foo]
      zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fsdt
                :~  :*  %fssg  %dbug
                        [/~nul/desk/~1234.5.6/foo/bar [1 9] [1 16]]
                        (ream 'a=5')
                    ==
                    :*  %fssg  %dbug
                        [/~nul/desk/~1234.5.6/foo/bar [2 9] [2 16]]
                        (ream 'b=6')
        ==  ==  ==  ==
      ^=  sources
        :~  [%dbug [/~nul/desk/~1234.5.6/foo/bar [4 1] [4 2]] [%sand %ud 5]]
  ==    ==
::
++  test-parse-scaffold-crane-fscm
  :-  `tank`leaf+"test-parse-scaffold-crane-fscm"
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /bar/foo]))
      [1 1]
    """
    /,
        /path/to/a
      /~  !.  a=5
    ::
        /path/to/b
      /~  !.  b=6
    ==
    1
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  source-rail=[[~nul %desk] /bar/foo]
      zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fscm
                :~  :-  /path/to/a
                    :*  %fssg  %dbug
                        [/~nul/desk/~1234.5.6/foo/bar [3 7] [3 14]]
                        (ream 'a=5')
                    ==
                    :-  /path/to/b
                    :*  %fssg  %dbug
                        [/~nul/desk/~1234.5.6/foo/bar [6 7] [6 14]]
                        (ream 'b=6')
        ==  ==  ==  ==
      ^=  sources
        :~  [%dbug [/~nul/desk/~1234.5.6/foo/bar [8 1] [8 2]] [%sand %ud 1]]
  ==    ==
::
++  test-parse-scaffold-crane-fspm
  :-  `tank`leaf+"test-parse-scaffold-crane-fspm"
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /bar/foo]))
      [1 1]
    """
    /=  data  /&  mark  /~  !.  a=1
    1
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  source-rail=[[~nul %desk] /bar/foo]
      zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fsts  %data
                %fspm  [%mark ~]
                %fssg  %dbug
                [/~nul/desk/~1234.5.6/foo/bar [1 25] [1 32]]
                (ream 'a=1')
        ==  ==
      ^=  sources
        :~  [%dbug [/~nul/desk/~1234.5.6/foo/bar [2 1] [2 2]] [%sand %ud 1]]
  ==    ==
::
++  test-parse-scaffold-crane-fscb
  :-  `tank`leaf+"test-parse-scaffold-crane-fscb"
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /bar/foo]))
      [1 1]
    """
    /_  /mark/
    8
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  source-rail=[[~nul %desk] /bar/foo]
      zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fscb  %fszy  %mark
        ==  ==
      ^=  sources
        :~  [%dbug [/~nul/desk/~1234.5.6/foo/bar [2 1] [2 2]] [%sand %ud 8]]
  ==    ==
::
++  test-parse-scaffold-crane-fssm
  :-  `tank`leaf+"test-parse-scaffold-crane-fssm"
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /bar/foo]))
      [1 1]
    """
    /=  data
      /;  !.  |=(a=@u +(a))
      /~  !.  5
    7
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  source-rail=[[~nul %desk] /bar/foo]
      zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fsts  %data
                %fssm
                :*  %dbug
                    [/~nul/desk/~1234.5.6/foo/bar [2 7] [2 24]]
                    (ream '|=(a=@u +(a))')
                ==
                %fssg
                :*  %dbug
                    [/~nul/desk/~1234.5.6/foo/bar [3 7] [3 12]]
                    (ream '5')
        ==  ==  ==
      ^=  sources
        :~  [%dbug [/~nul/desk/~1234.5.6/foo/bar [4 1] [4 2]] [%sand %ud 7]]
  ==    ==
::
++  test-parse-scaffold-crane-fscl
  :-  `tank`leaf+"test-parse-scaffold-crane-fscl"
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /bar/foo]))
      [1 1]
    """
    /=  tests
      /:  /===/tests
      /_  /mark/
    3
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  source-rail=[[~nul %desk] /bar/foo]
      zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fsts  %tests
                %fscl  [[~ ~[~ ~ ~ [~ [%sand %tas 495.874.958.708]]]] ~]
                %fscb  %fszy  %mark
        ==  ==
      ^=  sources
        :~  [%dbug [/~nul/desk/~1234.5.6/foo/bar [4 1] [4 2]] [%sand %ud 3]]
  ==    ==
::
++  test-parse-scaffold-crane-fskt
  :-  `tank`leaf+"test-parse-scaffold-crane-fskt"
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /bar/foo]))
      [1 1]
    """
    /=  data
      /^  !.  (list @ud)
      /.  /~  !.  1
          /~  !.  2
          ==
    6
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  source-rail=[[~nul %desk] /bar/foo]
      zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fsts  %data
                %fskt
                :*  %dbug
                    [/~nul/desk/~1234.5.6/foo/bar [2 7] [2 21]]
                    (ream '(list @ud)')
                ==
                %fsdt
                :~  :*  %fssg  %dbug
                        [/~nul/desk/~1234.5.6/foo/bar [3 11] [3 16]]
                        (ream '1')
                    ==
                    :*  %fssg  %dbug
                        [/~nul/desk/~1234.5.6/foo/bar [4 11] [4 16]]
                        (ream '2')
        ==  ==  ==  ==
      ^=  sources
        :~  [%dbug [/~nul/desk/~1234.5.6/foo/bar [6 1] [6 2]] [%sand %ud 6]]
  ==    ==
::
++  test-parse-scaffold-crane-fszp
  :-  `tank`leaf+"test-parse-scaffold-crane-fszp"
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /bar/foo]))
      [1 1]
    """
    /=  data  /!mark/
    2
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  source-rail=[[~nul %desk] /bar/foo]
      zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fsts  %data
                %fszp  %mark
        ==  ==
      ^=  sources
        :~  [%dbug [/~nul/desk/~1234.5.6/foo/bar [2 1] [2 2]] [%sand %ud 2]]
  ==    ==
::
++  test-parse-scaffold-crane-fszy
  :-  `tank`leaf+"test-parse-scaffold-crane-fszy"
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /bar/foo]))
      [1 1]
    """
    /=  data  /mark/
    9
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %-  expect-eq  !>
  :_  p.u.q.parsed
  :*  source-rail=[[~nul %desk] /bar/foo]
      zuse-version=309
      structures=~
      libraries=~
      ^=  crane
        :~  :*  %fsts  %data
                %fszy  %mark
        ==  ==
      ^=  sources
        :~  [%dbug [/~nul/desk/~1234.5.6/foo/bar [2 1] [2 2]] [%sand %ud 9]]
  ==    ==
::
++  test-literal
  :-  `tank`leaf+"test-literal"
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::  send a pinned literal, expects a %made response with pinned literal
      ::
      ^=  call-args
        [duct=~ type=~ %build ~nul [%pin ~1234.5.6 [%$ %noun !>(**)]]]
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.6
                %complete  %success  %pin  ~1234.5.6  %success  %$  %noun  !>(**)
        ==  ==
    ==
  ::
  %+  welp
    results1
  (expect-ford-empty ford-gate ~nul)
::
++  test-autocons-same
  :-  `tank`leaf+"test-autocons-same"
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::  if we autocons the same schematic, we should get two of it as a result
      ::
      ^=  call-args
        :*  duct=~  type=~  %build  ~nul
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
  (expect-ford-empty ford-gate ~nul)
::
++  test-autocons-different
  :-  `tank`leaf+"test-autocons-different"
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      :: if we autocons different schematics, we get different values
      ::
      ^=  call-args
        :*  duct=~  type=~  %build  ~nul
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
    (expect-ford-empty ford-gate ~nul)
::
++  test-scry-clay-succeed
  :-  `tank`leaf+"test-scry-clay-succeed"
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-succeed ~1234.5.6 [%noun !>(42)])
      ::  test a pinned scry which succeeds
      ::
      ^=  call-args
        :*  duct=~  type=~  %build  ~nul
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
  (expect-ford-empty ford-gate ~nul)
::
++  test-scry-clay-fail
  :-  `tank`leaf+"test-scry-clay-fail"
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-fail ~1234.5.6)
      ::  attempting to scry a path which fails should produce an error
      ::
      ^=  call-args
        :*  duct=~  type=~  %build  ~nul
            [%pin ~1234.5.6 [%scry %c ren=%x rail=[[~nul %desk] /bar/foo]]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.6  %complete
                %error
                :~  leaf+"scry failed for"
                    leaf+"%cx /~nul/desk/~1234.5.6/foo/bar"
    ==  ==  ==  ==
  ::
  %+  weld
    results1
    (expect-ford-empty ford-gate ~nul)
::
++  test-scry-clay-block
  :-  `tank`leaf+"test-scry-clay-block"
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-block ~1234.5.6)
      ::  when we scry on a blocked path, expect a subscription move
      ::
      ^=  call-args
        :*  duct=~  type=~  %build  ~nul
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
  =^  results2  ford-gate
    %-  test-ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=scry-is-forbidden
      ::  when clay responds, send a %made
      ::
      ^=  take-args
        :*  wire=/~nul/scry-request/cx/~nul/desk/~1234.5.6/foo/bar  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-scry-clay-live
  :-  `tank`leaf+"test-scry-clay-live"
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-succeed ~1234.5.6 [%noun !>(42)])
      ::
      ^=  call-args
        :*  duct=~  type=~  %build  ~nul
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
  =^  results2  ford-gate
    %-  test-ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=(scry-succeed ~1234.5.7 [%noun !>(43)])
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
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
  =^  results3  ford-gate
    %-  test-ford-call  :*
      ford-gate
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-scry-clay-live-again
  :-  `tank`leaf+"test-scry-clay-live-again"
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-succeed ~1234.5.6 [%noun !>(42)])
      ::  perform a live scry, we should get a %made and a clay subscription
      ::
      ^=  call-args
        :*  duct=~[/first]  type=~  %build  ~nul
            [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/first]  %give  %made  ~1234.5.6  %complete  %success
                [%scry %noun !>(42)]
            ==
            :*  duct=~[/first]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry-is-forbidden
      ::  a second scry from a different duct shouldn't resubscribe
      ::
      ^=  call-args
        :*  duct=~[/second]  type=~  %build  ~nul
            [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/second]  %give  %made  ~1234.5.7  %complete  %success
                [%scry %noun !>(42)]
    ==  ==  ==
  ::
  =^  results3  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/first] type=~ %kill ~nul]
      moves=~
    ==
  ::
  =^  results4  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.9
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/second] type=~ %kill ~nul]
      ::
      ^=  moves
        ::  the cancellation should go on the duct that made it: /first
        ::
        :~  :*  duct=~[/first]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    (expect-ford-empty ford-gate ~nul)
  ==
::  tests multiple subscriptions on the same resource at different times
::
::    We can depend on the same paths but at different times. Make sure we can
::    block on /~nul/desk/~1234.5.7/... and /~nul/desk/~1234.5.8/... at the
::    same time.
::
++  test-scry-clay-same-path
  :-  `tank`leaf+"test-scry-clay-same-path"
  ::
  =/  scry-type=type  [%atom %tas ~]
  ::
  =/  blocks=(set @da)  (sy ~1234.5.7 ~1234.5.8 ~)
  ::
  =/  scry-schematic=schematic:ford-gate
    [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
  =/  autocons=schematic:ford-gate
    [[%pin ~1234.5.7 scry-schematic] [%pin ~1234.5.8 scry-schematic]]
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-blocks blocks)
      ::
      call-args=[duct=~[/first] type=~ %build ~nul autocons]
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        %-  expect-eq  !>
        :-  (sy moves)
        %-  sy
        :~  :*  duct=~[/first]  %pass
                wire=/~nul/scry-request/cx/~nul/desk/~1234.5.7/foo/bar
                %c  %warp  [~nul ~nul]  %desk
                `[%sing %x [%da ~1234.5.7] /foo/bar]
            ==
            :*  duct=~[/first]  %pass
                wire=/~nul/scry-request/cx/~nul/desk/~1234.5.8/foo/bar
                %c  %warp  [~nul ~nul]  %desk
                `[%sing %x [%da ~1234.5.8] /foo/bar]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  test-ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=(scry-blocks blocks)
      ::
      ^=  take-args
        :*  wire=/~nul/scry-request/cx/~nul/desk/~1234.5.7/foo/bar  duct=~[/first]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            :*  %c  %writ  ~  [%x [%da ~1234.5.7] %desk]
                /bar/foo  %noun  scry-type  %seven
            ==
        ==
      ::
      expected-moves=~
    ==
  ::
  =^  results3  ford-gate
    %-  test-ford-take  :*
      ford-gate
      now=~1234.5.8
      scry=(scry-blocks blocks)
      ::
      ^=  take-args
        :*  wire=/~nul/scry-request/cx/~nul/desk/~1234.5.8/foo/bar  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            :*  %c  %writ  ~  [%x [%da ~1234.5.8] %desk]
                /bar/foo  %noun  scry-type  %eight
            ==
        ==
      ::
      ^=  expected-moves
        ^-  (list move:ford-gate)
        :~  :*  duct=~[/first]  %give  %made  ~1234.5.6  %complete  %success
                [%success %pin ~1234.5.7 %success %scry %noun scry-type %seven]
                [%success %pin ~1234.5.8 %success %scry %noun scry-type %eight]
    ==  ==  ==
  ::
  =^  results4  ford-gate
    %-  test-ford-call  :*
      ford-gate
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-pinned-in-past
  :-  `tank`leaf+"test-pinned-in-past"
  ::
  =/  schematic  [%pin ~1234.5.5 [%$ %noun !>(42)]]
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/pin] type=~ %build ~nul schematic]
      ::
      ^=  expected-moves
        :~  :*  duct=~[/pin]  %give  %made  ~1234.5.5  %complete
                %success  %pin  ~1234.5.5  %success  %$  %noun  !>(42)
    ==  ==  ==
  results1
::
++  test-pinned-in-future
  :-  `tank`leaf+"test-pinned-in-future"
  ::
  =/  schematic  [%pin ~1234.5.7 [%$ %noun !>(42)]]
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/pin] type=~ %build ~nul schematic]
      ::
      ^=  expected-moves
        :~  :*  duct=~[/pin]  %give  %made  ~1234.5.7  %complete
                %success  %pin  ~1234.5.7  %success  %$  %noun  !>(42)
    ==  ==  ==
  results1
::
++  test-pinned-in-pin
  :-  `tank`leaf+"test-pinned-in-pin"
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-succeed ~1234.5.8 [%noun !>(42)])
      ::
      ^=  call-args
        :*  duct=~[/pinned-in-future]  type=~  %build  ~nul
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-pinned-in-live
  :-  `tank`leaf+"test-pinned-in-live"
  ::
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  =/  scry-43  (scry-succeed ~1234.5.7 [%noun !>(43)])
  ::
  =/  schematic=schematic:ford-gate
    :*  %same  %pin  ~1234.5.6
        [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
    ==
  ::
  =/  build=build:ford-gate  [~1234.5.6 schematic]
  =/  result=build-result:ford-gate
    [%success %same %success %pin ~1234.5.6 %success [%scry %noun !>(42)]]
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-42
      ::
      call-args=[duct=~ type=~ %build ~nul schematic]
      moves=[duct=~ %give %made ~1234.5.6 %complete result]~
    ==
  ::
  =/  results2=tang
    %-  expect-eq  !>
    =/  ford  *ford-gate
    :_  results:(~(got by state-by-ship.ax.+>+<.ford) ~nul)
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
  =^  results3  ford-gate
    %-  test-ford-call  :*
      ford-gate
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-live-build-that-blocks
  :-  `tank`leaf+"test-live-build-that-blocks"
  ::
  =/  scry-blocked  (scry-block ~1234.5.6)
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  =/  scry-43  (scry-succeed ~1234.5.7 [%noun !>(43)])
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-blocked
      ::
      ^=  call-args
        :*  duct=~  type=~  %build  ~nul
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
  =^  results2  ford-gate
    %-  test-ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=scry-43
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results3  ford-gate
    %-  test-ford-take  :*
      ford-gate
      now=~1234.5.8
      scry=scry-42
      ::
      ^=  take-args
        :*  wire=/~nul/scry-request/cx/~nul/desk/~1234.5.6/foo/bar  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
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
  =^  results4  ford-gate
    %-  test-ford-call  :*
      ford-gate
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-live-and-once
  :-  `tank`leaf+"test-live-and-once"
  ::
  =/  scry-blocked  (scry-block ~1234.5.6)
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-blocked
      ::
      ^=  call-args
        :*  duct=~[/live]  type=~  %build  ~nul
            [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/live]  %pass
                wire=/~nul/scry-request/cx/~nul/desk/~1234.5.6/foo/bar
                %c  %warp  [~nul ~nul]  %desk
                ~  %sing  %x  [%da ~1234.5.6]  /foo/bar
            ==
        ::
            :*  duct=~[/live]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry-blocked
      ::
      ^=  call-args
        :*  duct=~[/once]  type=~  %build  ~nul
            [%pin ~1234.5.6 [%scry %c ren=%x rail=[[~nul %desk] /bar/foo]]]
        ==
      ::
      moves=~
    ==
  ::
  =^  results3  ford-gate
    %-  test-ford-take  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      ^=  take-args
        :*  wire=/~nul/scry-request/cx/~nul/desk/~1234.5.6/foo/bar  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
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
  =^  results4  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.9
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/live] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~[/live]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-live-two-deep
  :-  `tank`leaf+"test-live-two-deep"
  ::
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  =/  scry-43  (scry-succeed ~1234.5.7 [%noun !>(43)])
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-42
      ::
      ^=  call-args
        :*  duct=~  type=~  %build  ~nul
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
  =^  results2  ford-gate
    %-  test-ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=scry-43
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
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
  =^  results3  ford-gate
    %-  test-ford-call  :*
      ford-gate
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-live-three-deep
  :-  `tank`leaf+"test-live-three-deep"
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
  ::
  =/  formula=hoon  (ream '`@tas`%constant')
  =/  subject-schematic=schematic:ford-gate
    [%scry %c %x [~nul %desk] /bar/foo]
  ::
  =/  ride=schematic:ford-gate  [%ride formula subject-schematic]
  =/  same=schematic:ford-gate  [%same ride]
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/ride] type=~ %build ~nul same]
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %give  %made  ~1234.5.6  %complete
                [%success [%same [%success [%ride scry-type %constant]]]]
            ==
            :*  duct=~[/ride]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  test-ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~[/ride]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results3  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/ride] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-live-triangle
  :-  `tank`leaf+"test-live-triangle"
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
  ::
  =/  formula=hoon  (ream '`@tas`%constant')
  =/  subject-schematic=schematic:ford-gate
    [%scry %c %x [~nul %desk] /bar/foo]
  ::
  =/  ride-type=type  [%atom %tas ~]
  =/  ride=schematic:ford-gate  [%ride formula subject-schematic]
  =/  autocons=schematic:ford-gate  [ride subject-schematic]
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/ride] type=~ %build ~nul autocons]
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %give  %made  ~1234.5.6  %complete  %success
                [%success [%ride ride-type %constant]]
                [%success [%scry %noun !>(%it-does-in-fact-matter)]]
            ==
            :*  duct=~[/ride]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  test-ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~[/ride]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %give  %made  ~1234.5.7  %complete  %success
                [%success [%ride ride-type %constant]]
                [%success [%scry %noun !>(%changed)]]
            ==
            :*  duct=~[/ride]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results3  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/ride] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    (expect-ford-empty ford-gate ~nul)
  ==
::  like +test-live-triangle, but with another pinned build
::
::    Ensures that we deal with various issues with live builds which
::    were partially pinned and their interaction with other live builds.
::
++  test-live-and-pinned-triangle
  :-  `tank`leaf+"test-live-and-pinned-triangle"
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
  ::
  =/  formula=hoon  (ream '`@tas`%constant')
  =/  subject-schematic=schematic:ford-gate  [%scry %c %x [~nul %home] /bar/foo]
  ::
  =/  ride-type=type  [%atom %tas ~]
  =/  ride=schematic:ford-gate  [%ride formula subject-schematic]
  =/  autocons=schematic:ford-gate  [ride subject-schematic]
  ::
  =/  static=schematic:ford-gate  [%same [%pin ~1234.5.6 autocons]]
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/static] type=~ %build ~nul static]
      ::
      ^=  moves
        :~  :*  duct=~[/static]  %give  %made  ~1234.5.6  %complete  %success
                %same  %success  %pin  ~1234.5.6  %success
                [%success [%ride ride-type %constant]]
                [%success [%scry %noun scry-type %it-does-in-fact-matter]]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      call-args=[duct=~[/autocons] type=~ %build ~nul autocons]
      ::
      ^=  moves
        :~  :*  duct=~[/autocons]  %give  %made  ~1234.5.7  %complete  %success
                [%success [%ride ride-type %constant]]
                [%success [%scry %noun scry-type %it-does-in-fact-matter]]
            ==
            :*  duct=~[/autocons]  %pass  wire=/~nul/clay-sub/~nul/home
                %c  %warp  [~nul ~nul]  %home
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results3  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/static] type=~ %kill ~nul]
      ::
      moves=~
    ==
  ::
  =^  results4  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/autocons] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~[/autocons]  %pass  wire=/~nul/clay-sub/~nul/home
                %c  %warp  [~nul ~nul]  %home  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-call
  :-  `tank`leaf+"test-call"
  ::
  =/  sample-schematic=schematic:ford-gate  [%$ %noun !>(5)]
  =/  gate-schematic=schematic:ford-gate  [%$ %noun !>(|=(a=@ud +(a)))]
  =/  call-schematic=schematic:ford-gate
    [%call gate-schematic sample-schematic]
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/call] type=~ %build ~nul call-schematic]
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
  =^  results2  ford-gate
    %-  test-ford-call  :*
      ford-gate
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-call-scry-succeed
  :-  `tank`leaf+"test-call-scry-succeed"
  ::
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  ::
  =/  sample-schematic=schematic:ford-gate  [%$ %noun !>(24)]
  =/  gate-schematic=schematic:ford-gate
    [%$ %noun !>(|=(a=@ud .^(@ud %cx /~nul/desk/~1234.5.6/foo/bar)))]
  =/  call-schematic=schematic:ford-gate
    [%call gate-schematic sample-schematic]
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry-42
      ::
      call-args=[duct=~[/call] type=~ %build ~nul call-schematic]
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ::
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
  =^  results2  ford-gate
    %-  test-ford-call  :*
      ford-gate
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
    (expect-ford-empty ford-gate ~nul)
  ==
++  test-call-scry-fail
  :-  `tank`leaf+"test-call-scry-fail"
  ::
  =/  scry-failed  (scry-fail ~1234.5.6)
  ::
  =/  sample-schematic=schematic:ford-gate  [%$ %noun !>(24)]
  =/  gate-schematic=schematic:ford-gate
    [%$ %noun !>(|=(a=@ud .^(@ud %cx /~nul/desk/~1234.5.6/foo/bar)))]
  =/  call-schematic=schematic:ford-gate
    [%call gate-schematic sample-schematic]
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry-failed
      ::
      call-args=[duct=~[/dead] type=~ %build ~nul call-schematic]
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
  =^  results2  ford-gate
    %-  test-ford-call  :*
      ford-gate
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
::
++  test-call-scry-block
  :-  `tank`leaf+"test-call-scry-block"
  ::
  =/  scry-blocked  (scry-block ~1234.5.6)
  ::
  =/  sample-schematic=schematic:ford-gate  [%$ %noun !>(24)]
  =/  gate-schematic=schematic:ford-gate
    [%$ %noun !>(|=(a=@ud .^(@ud %cx /~nul/desk/~1234.5.6/foo/bar)))]
  =/  call-schematic=schematic:ford-gate
    [%call gate-schematic sample-schematic]
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-blocked
      ::
      call-args=[duct=~[/live] type=~ %build ~nul call-schematic]
      ::
      ^=  moves
        :~  :*  duct=~[/live]  %pass
                wire=/~nul/scry-request/cx/~nul/desk/~1234.5.6/foo/bar
                %c  %warp  [~nul ~nul]  %desk
                ~  %sing  %x  [%da ~1234.5.6]  /foo/bar
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  test-ford-take-with-comparator  :*
      ford-gate
      now=~1234.5.7
      scry=scry-blocked
      ::
      ^=  call-args
        :*  wire=/~nul/scry-request/cx/~nul/desk/~1234.5.6/foo/bar  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
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
  =^  results3  ford-gate
    %-  test-ford-call  :*
      ford-gate
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
    (expect-ford-empty ford-gate ~nul)
  ==
::  +test-call-scry-varies: call with an argument which varies
::
::    This test reads the sample for a %call schematic from clay. This sample
::    is a date. Inside of the gate called, we scry on a path based on the
::    passed in sample date.
::
++  test-call-scry-varies
  :-  `tank`leaf+"test-call-scry-varies"
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
  ::
  =/  sample-schematic=schematic:ford-gate
    [%scry [%c care=%x bel=[[~nul %desk] /timer]]]
  =/  gate-schematic=schematic:ford-gate
    [%$ %noun !>(|=(a=@da .^(@tas %cx /~nul/desk/(scot %da a)/result)))]
  =/  call-schematic=schematic:ford-gate
    [%call gate-schematic sample-schematic]
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      ^=  call-args
        [duct=~[/call] type=~ %build ~nul call-schematic]
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
        :*  duct=~[/call]  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.6] (sy [%x /timer] ~)]
    ==  ==
  ::
  =^  results2  ford-gate
    %-  test-ford-take-with-comparator  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      ^=  call-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~[/call]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
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
        :*  duct=~[/call]  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.7] (sy [%x /timer] ~)]
    ==  ==
  ::
  =^  results3  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/call] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~[/call]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-dude
  :-  `tank`leaf+"test-dude"
  ::
  =/  schematic=schematic:ford-gate
    :*  %pin  ~1234.5.6
        %dude  |.(>%test-no-error<)
        [%scry [%c care=%x bel=[[~nul %desk] /bar/foo]]]
    ==
  ::
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-42
      ::
      call-args=[duct=~[/once] type=~ %build ~nul schematic]
      ::
      ^=  moves
        :~  :*  duct=~[/once]  %give  %made  ~1234.5.6  %complete  %success
                [%pin ~1234.5.6 %success %dude %success [%scry %noun !>(42)]]
    ==  ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-dude-error
  :-  `tank`leaf+"test-dude-error"
  ::
  =/  schematic=schematic:ford-gate
    :*  %pin  ~1234.5.6
        %dude  |.(>%in-the-error-message<)
        [%scry [%c care=%x bel=[[~nul %desk] /bar/foo]]]
    ==
  ::
  =/  scry-42  (scry-fail ~1234.5.6)
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-42
      ::
      call-args=[duct=~[/once] type=~ %build ~nul schematic]
      ::
      ^=  moves
        :~  :*  duct=~[/once]  %give  %made  ~1234.5.6  %complete
                %error
                :~  leaf+"%in-the-error-message"
                    leaf+"scry failed for"
                    leaf+"%cx /~nul/desk/~1234.5.6/foo/bar"
    ==  ==  ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-hood
  :-  `tank`leaf+"test-hood"
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
  =/  schematic=schematic:ford-gate  [%hood [[~nul %desk] /hoon/bar/foo]]
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/hood] type=~ %build ~nul schematic]
      ::
      ^=  moves
        :~  :*  duct=~[/hood]  %give  %made  ~1234.5.6
                %complete  %success  %hood
                :*  source-rail=[[~nul %desk] /hoon/bar/foo]
                    zuse-version=309
                    structures=~
                    libraries=~
                    cranes=~
                    ^=  sources
                      :~  :*  %dbug
                              [/~nul/desk/0/foo/bar/hoon [1 1] [1 19]]
                              (ream '|=(a=@ud +(a))')
            ==  ==    ==  ==
            :*  duct=~[/hood]  %pass  /~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar/hoon] ~)]
    ==  ==  ==
  ::
  results1
::
++  test-slim
  :-  `tank`leaf+"test-slim"
  ::
  =/  formula=hoon  (ream '(add 2 2)')
  =/  subject-type=type  -:!>(.)
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/dead] type=~ %build ~nul [%slim subject-type formula]]
      ::
      ^=  moves
        :~  :*  duct=~[/dead]  %give  %made  ~1234.5.6  %complete
                [%success [%slim (~(mint ut subject-type) [%noun formula])]]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  test-ford-call  :*
      ford-gate
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-slit
  :-  `tank`leaf+"test-slit"
  ::
  =/  gate=vase    (ride %noun '|=(a=@ud ["one" a])')
  =/  sample=vase  !>(42)
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/slit] type=~ %build ~nul [%slit gate sample]]
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
  =^  results2  ford-gate
    %-  test-ford-call  :*
      ford-gate
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-slit-error
  :-  `tank`leaf+"test-slit-error"
  ::
  =/  gate=vase    (ride %noun '|=(a=@ud ["one" a])')
  =/  sample=vase  !>("a tape instead of @ud")
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/slit] type=~ %build ~nul [%slit gate sample]]
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
  =^  results2  ford-gate
    %-  test-ford-call  :*
      ford-gate
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-ride
  :-  `tank`leaf+"test-ride"
  ::
  =/  fun  |=(a=@ (add 2 a))
  =/  formula=hoon  (ream '!:  (fun 3)')
  =/  subject-schematic=schematic:ford-gate  [%$ %noun !>(.)]
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      ^=  call-args
        :*  duct=~[/dead]  type=~  %build  ~nul
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
  =^  results2  ford-gate
    %-  test-ford-call  :*
      ford-gate
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-ride-scry-succeed
  :-  `tank`leaf+"test-ride-scry-succeed"
  ::
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  ::
  =/  formula=hoon  (ream '!:  .^(* %cx /~nul/desk/~1234.5.6/foo/bar)')
  =/  subject-schematic=schematic:ford-gate  [%$ %noun !>(.)]
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry-42
      ::
      ^=  call-args
        :*  duct=~[/dead]  type=~  %build  ~nul
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
  =^  results2  ford-gate
    %-  test-ford-call  :*
      ford-gate
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-ride-scry-fail
  :-  `tank`leaf+"test-ride-scry-fail"
  ::
  =/  scry-failed  (scry-fail ~1234.5.6)
  ::
  =/  formula=hoon  (ream '!:  .^(* %cx /~nul/desk/~1234.5.6/foo/bar)')
  =/  subject-schematic=schematic:ford-gate  [%$ %noun !>(.)]
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry-failed
      ::
      ^=  call-args
        :*  duct=~[/dead]  type=~  %build  ~nul
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
  =^  results2  ford-gate
    %-  test-ford-call  :*
      ford-gate
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-ride-scry-block
  :-  `tank`leaf+"test-ride-scry-block"
  ::
  =/  scry-blocked  (scry-block ~1234.5.6)
  ::
  =/  formula=hoon  (ream '!:  .^(* %cx /~nul/desk/~1234.5.6/foo/bar)')
  =/  subject-schematic=schematic:ford-gate  [%$ %noun !>(.)]
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-blocked
      ::
      ^=  call-args
        :*  duct=~[/live]  type=~  %build  ~nul
            [%ride formula subject-schematic]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/live]  %pass
                wire=/~nul/scry-request/cx/~nul/desk/~1234.5.6/foo/bar
                %c  %warp  [~nul ~nul]  %desk
                ~  %sing  %x  [%da ~1234.5.6]  /foo/bar
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  test-ford-take-with-comparator  :*
      ford-gate
      now=~1234.5.7
      scry=scry-blocked
      ::
      ^=  take-args
        :*  wire=/~nul/scry-request/cx/~nul/desk/~1234.5.6/foo/bar  duct=~[/live]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
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
  =^  results3  ford-gate
    %-  test-ford-call  :*
      ford-gate
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-ride-scry-promote
  :-  `tank`leaf+"test-ride-scry-promote"
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
  ::
  =/  formula=hoon  (ream '`@tas`%constant')
  =/  subject-schematic=schematic:ford-gate  [%scry %c %x [~nul %desk] /bar/foo]
  ::
  =/  ride=schematic:ford-gate  [%ride formula subject-schematic]
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/ride] type=~ %build ~nul ride]
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %give  %made  ~1234.5.6  %complete
                [%success [%ride scry-type %constant]]
            ==
            :*  duct=~[/ride]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  test-ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~[/ride]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results3  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/ride] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-five-oh-fora
  :-  `tank`leaf+"test-five-oh-fora"
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
  ::
  =/  post-a=schematic:ford-gate  [%scry [%c %x [~nul %desk] /a/posts]]
  =/  title-a=schematic:ford-gate  [%ride (ream '!:  title') post-a]
  ::
  =/  post-b=schematic:ford-gate  [%scry [%c %x [~nul %desk] /b/posts]]
  =/  title-b=schematic:ford-gate  [%ride (ream '!:  title') post-b]
  ::
  =/  sidebar=schematic:ford-gate  [title-a title-b]
  ::
  =/  rendered-a=schematic:ford-gate  [post-a sidebar]
  =/  rendered-b=schematic:ford-gate  [post-b sidebar]
  ::  first, ask ford to build rendered-a
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/post-a] type=~ %build ~nul rendered-a]
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
        :*  duct=~[/post-a]  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.6] (sy [%x /posts/a] [%x /posts/b] ~)]
    ==  ==
  ::
  =^  results2  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      call-args=[duct=~[/post-b] type=~ %build ~nul rendered-b]
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
  =^  results3  ford-gate
    %-  test-ford-take-with-comparator  :*
      ford-gate
      now=~1234.5.8
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~[/post-a]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
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
  =^  results4  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.9
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/post-b] type=~ %kill ~nul]
      moves=~
    ==
  ::
  =^  results5  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.10
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/post-a] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~[/post-a]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    results5
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-alts
  :-  `tank`leaf+"test-alts"
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
  ::
  =/  scry1=schematic:ford-gate  [%scry [%c %x [~nul %first] /one/scry]]
  =/  scry2=schematic:ford-gate  [%scry [%c %x [~nul %second] /two/scry]]
  =/  alts=schematic:ford-gate   [%alts [scry1 scry2 ~]]
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/alts] type=~ %build ~nul alts]
      ::
      ^=  moves
        :~  :*  duct=~[/alts]  %give  %made  ~1234.5.6  %complete
                [%error [%leaf "%alts: all options failed"]~]
            ==
            :*  duct=~[/alts]  %pass  wire=/~nul/clay-sub/~nul/first
                %c  %warp  [~nul ~nul]  %first
                `[%mult [%da ~1234.5.6] (sy [%x /scry/one] ~)]
            ==
            :*  duct=~[/alts]  %pass  wire=/~nul/clay-sub/~nul/second
                %c  %warp  [~nul ~nul]  %second
                `[%mult [%da ~1234.5.6] (sy [%x /scry/two] ~)]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  test-ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/second  duct=~[/alts]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /scry/two]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/alts]  %give  %made  ~1234.5.7  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-two'
            ==
            :*  duct=~[/alts]  %pass  wire=/~nul/clay-sub/~nul/second
                %c  %warp  [~nul ~nul]  %second
                `[%mult [%da ~1234.5.7] (sy [%x /scry/two] ~)]
    ==  ==  ==
  ::
  =^  results3  ford-gate
    %-  test-ford-take  :*
      ford-gate
      now=~1234.5.8
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/first  duct=~[/alts]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.8] (sy [%x /scry/one]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/alts]  %give  %made  ~1234.5.8  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-one'
            ==
            :*  duct=~[/alts]  %pass  wire=/~nul/clay-sub/~nul/first
                %c  %warp  [~nul ~nul]  %first
                `[%mult [%da ~1234.5.8] (sy [%x /scry/one] ~)]
            ==
            :*  duct=~[/alts]  %pass  wire=/~nul/clay-sub/~nul/second
                %c  %warp  [~nul ~nul]  %second  ~
    ==  ==  ==
  ::
  =^  results4  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.9
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/alts] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~[/alts]  %pass  wire=/~nul/clay-sub/~nul/first
                %c  %warp  [~nul ~nul]  %first  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-alts-and-live
  :-  `tank`leaf+"test-alts-and-live"
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
  ::
  =/  scry2=schematic:ford-gate  [%scry [%c %x [~nul %desk] /two/scry]]
  =/  same=schematic:ford-gate   [%same scry2]
  ::  depend on scry2 for the duration of the test
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/same] type=~ %build ~nul same]
      ::
      ^=  moves
        :~  :*  duct=~[/same]  %give  %made  ~1234.5.6  %complete
                %success  %same  %success  %scry  %noun  scry-type  'scry-two'
            ==
            :*  duct=~[/same]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /scry/two] ~)]
    ==  ==  ==
  ::
  =/  scry1=schematic:ford-gate  [%scry [%c %x [~nul %desk] /one/scry]]
  =/  alts=schematic:ford-gate   [%alts [scry1 scry2 ~]]
  ::  call the alts schematic
  ::
  ::    The alts schematic should fail to read /scry/one, and should fallback
  ::    to /scry/two.
  ::
  =^  results2  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      call-args=[duct=~[/alts] type=~ %build ~nul alts]
      ::
      ^=  moves
        :~  :*  duct=~[/alts]  %give  %made  ~1234.5.7  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-two'
            ==
            :*  duct=~[/same]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
            ==
            :*  duct=~[/alts]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /scry/two] [%x /scry/one] ~)]
    ==  ==  ==
  ::
  ::  tell ford that /scry/one exists now
  ::
  =^  results3  ford-gate
    %-  test-ford-take  :*
      ford-gate
      now=~1234.5.8
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~[/alts]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.8] (sy [%x /scry/one]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/alts]  %give  %made  ~1234.5.8  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-one'
            ==
            ::  we subscribe to both paths because /same still exists.
            ::
            :*  duct=~[/alts]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.8] (sy [%x /scry/one] [%x /scry/two] ~)]
    ==  ==  ==
  ::
  ::  kill the /same build
  ::
  ::    We should no longer subscribe to /scry/two in the resulting clay
  ::    subscription.
  ::
  =^  results4  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.9
      scry=scry
      ::
      call-args=[duct=~[/same] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~[/alts]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
            ==
            :*  duct=~[/same]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.8] (sy [%x /scry/one] ~)]
    ==  ==  ==
  ::
  =^  results5  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.10
      scry=scry
      ::
      call-args=[duct=~[/alts] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~[/same]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    results5
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-double-alts
  :-  `tank`leaf+"test-double-alts"
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
  ::
  =/  scry1=schematic:ford-gate  [%scry [%c %x [~nul %desk] /one/scry]]
  =/  scry2=schematic:ford-gate  [%scry [%c %x [~nul %desk] /two/scry]]
  =/  scry3=schematic:ford-gate  [%scry [%c %x [~nul %desk] /three/scry]]
  =/  alts1=schematic:ford-gate  [%alts [scry1 scry2 ~]]
  =/  alts2=schematic:ford-gate  [%alts [scry3 scry2 ~]]
  ::  alts1 will depend on both scry1 and scry2
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/first] type=~ %build ~nul alts1]
      ::
      ^=  moves
        :~  :*  duct=~[/first]  %give  %made  ~1234.5.6  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-two'
            ==
            :*  duct=~[/first]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /scry/one] [%x /scry/two] ~)]
    ==  ==  ==
  ::  alts2 will depend on both scry3 and scry2
  ::
  =^  results2  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      call-args=[duct=~[/second] type=~ %build ~nul alts2]
      ::
      ^=  moves
        :~  :*  duct=~[/second]  %give  %made  ~1234.5.7  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-two'
            ==
            :*  duct=~[/first]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
            ==
            :*  duct=~[/second]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~  %mult  [%da ~1234.5.7]
                (sy [%x /scry/one] [%x /scry/two] [%x /scry/three] ~)
    ==  ==  ==
  ::
  ::  alts2 should now just return 'scry-three'
  ::
  =^  results3  ford-gate
    %-  test-ford-take  :*
      ford-gate
      now=~1234.5.8
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~[/second]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.8] (sy [%x /scry/three]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/second]  %give  %made  ~1234.5.8  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-three'
            ==
            :*  duct=~[/second]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~  %mult  [%da ~1234.5.8]
                (sy [%x /scry/one] [%x /scry/two] [%x /scry/three] ~)
    ==  ==  ==
  ::
  ::  alts1 should now just return 'scry-one'
  ::
  =^  results4  ford-gate
    %-  test-ford-take  :*
      ford-gate
      now=~1234.5.9
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~[/second]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.9] (sy [%x /scry/one] [%x /scry/two] ~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/first]  %give  %made  ~1234.5.9  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-one'
            ==
            :*  duct=~[/second]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~  %mult  [%da ~1234.5.9]
                (sy [%x /scry/one] [%x /scry/three] ~)
    ==  ==  ==
  ::
  =^  results5  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.10
      scry=scry
      ::
      call-args=[duct=~[/first] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~[/second]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
            ==
            :*  duct=~[/first]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~  %mult  [%da ~1234.5.9]
                (sy [%x /scry/three] ~)
    ==  ==  ==
  ::
  =^  results6  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.11
      scry=scry
      ::
      call-args=[duct=~[/second] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~[/first]  %pass  wire=/~nul/clay-sub/~nul/desk
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
    (expect-ford-empty ford-gate ~nul)
  ==
::  +test-cache-reclamation-trivial: reclaim cache on a blank slate ford
::
++  test-cache-reclamation-trivial
  :-  `tank`leaf+"test-cache-reclamation-trivial"
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::  send a pinned literal, expects a %made response with pinned literal
      ::
      ^=  call-args
        [duct=~[/trivial] type=~ %build ~nul [%pin ~1234.5.6 [%$ %noun !>(**)]]]
      ::
      ^=  moves
        :~  :*  duct=~[/trivial]  %give  %made  ~1234.5.6
                %complete  %success  %pin  ~1234.5.6  %success  %$  %noun  !>(**)
        ==  ==
    ==
  ::
  =^  results2  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry-is-forbidden
      ::  ask ford to wipe its cache
      ::
      call-args=[duct=~[/trivial] type=~ %wipe ~]
      ::  cache wiping should never produce any moves
      ::
      moves=~
    ==
  ::
  ;:  welp
    results1
    results2
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-cache-reclamation-live-rebuild
  :-  `tank`leaf+"test-cache-reclamation-live-rebuild"
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-succeed ~1234.5.6 [%noun !>(42)])
      ::
      ^=  call-args
        :*  duct=~[/build]  type=~  %build  ~nul
            [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/build]  %give  %made  ~1234.5.6  %complete  %success
                [%scry %noun !>(42)]
            ==
            :*  duct=~[/build]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry-is-forbidden
      ::  ask ford to wipe its cache
      ::
      call-args=[duct=~[/build] type=~ %wipe ~]
      ::  cache wiping should never produce any moves
      ::
      moves=~
    ==
  ::
  =^  results3  ford-gate
    %-  test-ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=(scry-succeed ~1234.5.7 [%noun !>(43)])
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~[/build]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/build]  %give  %made  ~1234.5.7  %complete  %success
                [%scry %noun !>(43)]
            ==
            :*  duct=~[/build]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results4  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/build] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~[/build]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-cache-reclamation-live-promote
  :-  `tank`leaf+"test-cache-reclamation-live-promote"
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
  ::
  =/  formula=hoon  (ream '`@tas`%constant')
  =/  subject-schematic=schematic:ford-gate
    [%scry %c %x [~nul %desk] /bar/foo]
  ::
  =/  ride=schematic:ford-gate  [%ride formula subject-schematic]
  =/  same=schematic:ford-gate  [%same ride]
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/ride] type=~ %build ~nul same]
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %give  %made  ~1234.5.6  %complete
                [%success [%same [%success [%ride scry-type %constant]]]]
            ==
            :*  duct=~[/ride]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry-is-forbidden
      ::  ask ford to wipe its cache
      ::
      call-args=[duct=~[/ride] type=~ %wipe ~]
      ::  cache wiping should never produce any moves
      ::
      moves=~
    ==
  ::
  =^  results3  ford-gate
    %-  test-ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~[/ride]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results4  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/ride] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %pass  wire=/~nul/clay-sub/~nul/desk
                %c  %warp  [~nul ~nul]  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    (expect-ford-empty ford-gate ~nul)
  ==
::  tests that doing a cache reclamation during the five-oh-fora rebuild works
::
++  test-five-oh-cache-reclamation
  :-  `tank`leaf+"test-five-oh-cache-reclamation"
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
  ::
  =/  post-a=schematic:ford-gate  [%scry [%c %x [~nul %desk] /a/posts]]
  =/  title-a=schematic:ford-gate  [%ride (ream '!:  title') post-a]
  ::
  =/  post-b=schematic:ford-gate  [%scry [%c %x [~nul %desk] /b/posts]]
  =/  title-b=schematic:ford-gate  [%ride (ream '!:  title') post-b]
  ::
  =/  sidebar=schematic:ford-gate  [title-a title-b]
  ::
  =/  rendered-a=schematic:ford-gate  [post-a sidebar]
  =/  rendered-b=schematic:ford-gate  [post-b sidebar]
  ::  first, ask ford to build rendered-a
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/post-a] type=~ %build ~nul rendered-a]
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
        :*  duct=~[/post-a]  %pass  wire=/~nul/clay-sub/~nul/desk
            %c  %warp  [~nul ~nul]  %desk
            `[%mult [%da ~1234.5.6] (sy [%x /posts/a] [%x /posts/b] ~)]
    ==  ==
  ::
  =^  results2  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      call-args=[duct=~[/post-b] type=~ %build ~nul rendered-b]
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
  =^  results3  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/wipe] type=~ %wipe ~]
      moves=~
    ==
  ::
  =^  results4  ford-gate
    %-  test-ford-take-with-comparator  :*
      ford-gate
      now=~1234.5.9
      scry=scry
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/desk  duct=~[/post-a]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
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
  =^  results5  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.10
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/post-b] type=~ %kill ~nul]
      moves=~
    ==
  ::
  =^  results6  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.11
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/post-a] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~[/post-a]  %pass  wire=/~nul/clay-sub/~nul/desk
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-reef
  :-  `tank`leaf+"test-reef"
  ::
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
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/reef]  type=~  %build  ~nul
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-reef-short-circuit
  :-  `tank`leaf+"test-reef-short-circuit"
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
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
        :*  duct=~[/reef]  type=~  %build  ~nul
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
    (expect-ford-empty ford-gate ~nul)
  ==
 
::
++  test-path
  :-  `tank`leaf+"test-path"
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
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-plan-hoon
  :-  `tank`leaf+"test-plan-hoon"
  ::
  =/  =hoon  (ream '`@tas`%constant')
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      ^=  call-args
        :*  duct=~[/plan]  type=~  %build  ~nul
            %pin  ~1234.5.6
            %plan
            source-path=[[~nul %home] /bar/foo]
            query-string=`coin`[%$ *dime]
            source-rail=[[~nul %desk] /bar/foo]
            zuse-version=309
            structures=~
            libraries=~
            cranes=~
            sources=[hoon]~
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core
  :-  `tank`leaf+"test-core"
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
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-linker
  :-  `tank`leaf+"test-core-linker"
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
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-multi-hoon
  :-  `tank`leaf+"test-core-multi-hoon"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/program/gen]]
      :-  %hoon
      :-  hoon-src-type
      '''
      |%
      +=  data-type
        [msg=tape count=@ud]
      --
      |%
      ++  data
        ^-  data-type
        ["one" 1]
      --
      data
      '''
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
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
          :-  ["one" 1]
          q.vase
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.vase) | -:!>(["one" 1]))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fsts-fssg
  :-  `tank`leaf+"test-core-fsts-fssg"
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
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fsdt-fskt
  :-  `tank`leaf+"test-core-fsdt-fskt"
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
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fskt-nest-fail
  :-  `tank`leaf+"test-core-fskt-nest-fail"
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
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/program/gen]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/path]  %give  %made  ~1234.5.6  %complete  %error
                :~  [%leaf "ford: %core failed: "]
                    [%leaf "/^ failed: nest-fail"]
    ==  ==  ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==

::
++  test-core-fssm
  :-  `tank`leaf+"test-core-fssm"
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
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
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
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fsbr
  :-  `tank`leaf+"test-core-fsbr"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/program/gen]]
      :-  %hoon
      :-  hoon-src-type
      '''
      /=  data  /|  /^  (list @u)
                    /~  5
                ::
                    /^  (list @u)
                    /~  [6 6 ~]
                ==
      data
      '''
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
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
          :-  [6 6 ~]
          q.vase
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.vase) | -:!>([6 6 ~]))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fsbr-out-of-options
  :-  `tank`leaf+"test-core-fsbr-out-of-options"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/program/gen]]
      :-  %hoon
      :-  hoon-src-type
      '''
      /=  data  /|  /^  (list @u)
                    /~  5
                ::
                    /^  @u
                    /~  [6 6 ~]
                ==
      data
      '''
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/program/gen]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/path]  %give  %made  ~1234.5.6  %complete  %error
                :~  [%leaf "ford: %core failed: "]
                    [%leaf "/| failed: out of options"]
    ==  ==  ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-plan-fszp-as-noun
  :-  `tank`leaf+"test-plan-fszp-as-noun"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/program/gen]]
      :-  %hoon
      :-  hoon-src-type
      '''
      /=  data  /!noun/
      data
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/other/lib]]
      :-  %hoon
      :-  hoon-src-type
      '''
      [1 2 3 ~]
      '''
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            :*  %plan  [[~nul %home] /other/lib]  *coin
                :*  source-rail=[[~nul %desk] /bar/foo]
                    zuse-version=309
                    structures=~
                    libraries=~
                    cranes=[%fsts %data [%fszp %noun]]~
                    sources=[%wing [%data]~]~
        ==  ==  ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete %success %pin *] i.moves)
        =/  result  result.p.card.i.moves
        =/  pin-result  build-result.result
        ?>  ?=([%success %plan *] build-result.pin-result)
        ::
        =/  =vase  vase.build-result.pin-result
        ::
        %+  weld
          %-  expect-eq  !>
          :-  [1 2 3 ~]
          q.vase
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.vase) | -:!>([1 2 3 ~]))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fszp-as-mark
  :-  `tank`leaf+"test-core-fszp-as-mark"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/program/gen]]
      :-  %hoon
      :-  hoon-src-type
      '''
      /=  data  /:  /===/lib/other
                /!somemark/
      data
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/somemark/mar]]
      :-  %hoon
      :-  hoon-src-type
      '''
      |_  [word=tape num=@]
      ++  grab
        |%
        +=  noun  [tape @]
        --
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/other/lib]]
      :-  %hoon
      :-  hoon-src-type
      '''
      ["five" 5]
      '''
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            %core  [[~nul %home] /hoon/program/gen]
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
          :-  ["five" 5]
          q.vase
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.vase) | -:!>(["five" 5]))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fscl-fszp
  :-  `tank`leaf+"test-core-fscl-fszp"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/program/gen]]
      :-  %hoon
      :-  hoon-src-type
      '''
      /=  data  /:  /===/data
                /!noun/
      data
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/data]]
      :-  %hoon
      :-  hoon-src-type
      '''
      [1 2 3 ~]
      '''
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
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
          :-  [1 2 3 ~]
          q.vase
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.vase) | -:!>([1 2 3 ~]))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fscm
  :-  `tank`leaf+"test-core-fscm"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/program/gen]]
      :-  %hoon
      :-  hoon-src-type
      '''
      /=  data  /:  /===/data
                /!noun/
      data
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/data]]
      :-  %hoon
      :-  hoon-src-type
      '''
      /,    /other
         /~  a=[3 2 1 ~]
      ::
            /data
         /~  a=[1 2 3 ~]
      ==
      a
      '''
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
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
          :-  [1 2 3 ~]
          q.vase
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.vase) | -:!>([1 2 3 ~]))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-plan-fsbc
  :-  `tank`leaf+"test-plan-fsbc"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/other/lib]]
      :-  %hoon
      :-  hoon-src-type
      '''
      /=  data  /$  %+  cork  fuel:html
                    |=  gas/epic:eyre
                    [bem.gas (~(got by qix.gas) 'key')]
      data
      '''
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            :*  %plan  [[~nul %home] /other/lib]
                :~  %many
                    [%blob *cred:eyre]
                    [%$ [%t %key]]
                    [%$ [%t %value]]
                    [%$ %n ~]
                ==
                :*  source-rail=[[~nul %home] /hoon/other/lib]
                    zuse-version=309
                    structures=~
                    libraries=~
                    cranes=[%fsts %data [%fszp %noun]]~
                    sources=`(list hoon)`[[%wing [%data]~] ~]
        ==  ==  ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete %success %pin *] i.moves)
        =/  result  result.p.card.i.moves
        =/  pin-result  build-result.result
        ?>  ?=([%success %plan *] build-result.pin-result)
        ::
        =/  =vase  vase.build-result.pin-result
        ::
        %+  weld
          %-  expect-eq  !>
          :-  [[[~nul %home [%da ~1234.5.6]] /other/lib] %value]
          q.vase
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.vase) | -:!>([*beam *@tas]))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fscb
  :-  `tank`leaf+"test-core-fscb"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  arch-type=type  -:!>(*arch)
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/program/gen]]
      :-  %hoon
      :-  hoon-src-type
      '''
      /=  data  /:  /===/data
                /_  /!noun/
      data
      '''
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /data]]
      :-  %arch
      :-  arch-type
      :-  ~
      (my ~[[~.one ~] [~.two ~] [~.hoon ~]])
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /one/data]]
      :-  %arch
      :-  arch-type
      :-  ~
      (my ~[[~.hoon ~]])
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /two/data]]
      :-  %arch
      :-  arch-type
      :-  ~
      (my ~[[~.hoon ~]])
    ::
      ::  this "hoon" file should be filtered out
      ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/data]]
      :-  %arch
      :-  arch-type
      :-  fil=[~ u=0v6]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/one/data]]
      :-  %hoon
      :-  hoon-src-type
      '''
      1
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/two/data]]
      :-  %hoon
      :-  hoon-src-type
      '''
      2
      '''
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
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
          :-  (my [[%one 1] [%two 2] ~])
          q.vase
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.vase) | -:!>(*[[@ta @ud] [[@ta @ud] ~ ~] ~]))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fspm
  :-  `tank`leaf+"test-core-fspm"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  arch-type=type  -:!>(*arch)
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/program/gen]]
      :-  %hoon
      :-  hoon-src-type
      '''
      /=  data  /&second&first&/~["four" 5 "six"]
      data
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/first/mar]]
      :-  %hoon
      :-  hoon-src-type
      '''
      |_  [word=tape num=@]
      ++  grab
        |%
        ++  noun  |=([a=tape b=@ c=tape] [a b])
        --
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/second/mar]]
      :-  %hoon
      :-  hoon-src-type
      '''
      |_  num=@
      ++  grab
        |%
        ++  first  |=([a=tape b=@] b)
        --
      --
      '''
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /mar]]
      :^  %arch  arch-type  ~
      (my ~[[~.first ~] [~.second ~]])
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /first/mar]]
      [%arch arch-type ~ (my ~[[~.hoon ~]])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /second/mar]]
      [%arch arch-type ~ (my ~[[~.hoon ~]])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/first/mar]]
      [%arch arch-type fil=[~ u=0v1] ~]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/second/mar]]
      [%arch arch-type fil=[~ u=0v2] ~]
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            %core  [[~nul %home] /hoon/program/gen]
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
          :-  5
          q.vase
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.vase) | -:!>(5))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fszy-renderer
  :-  `tank`leaf+"test-core-fszy-renderer"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/program/gen]]
      :-  %hoon
      :-  hoon-src-type
      '''
      /=  data  /:  /===/data
                /foo/
      data
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/foo/ren]]
      :-  %hoon
      :-  hoon-src-type
      '''
      /=  data  /!noun/
      data
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/data]]
      :-  %hoon
      :-  hoon-src-type
      '''
      [1 2 3 ~]
      '''
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            %core  [[~nul %home] /hoon/program/gen]
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
          :-  [1 2 3 ~]
          q.vase
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.vase) | -:!>([1 2 3 ~]))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-bunt
  :-  `tank`leaf+"test-bunt"
  ::
  =/  hoon-src=@ta
    '''
    |_  cell=^
    ++  grab
      |%
      ++  noun  ^
      --
    --
    '''
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/foo/mar]]
      [%hoon !>(hoon-src)]
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            [%bunt [~nul %home] %foo]
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
        ?>  ?=([%success %bunt *] build-result.pin-result)
        ::
        =/  =vase  q.cage.build-result.pin-result
        ::
        %+  weld
          %-  expect-eq  !>
          :-  [0 0]
          q.vase
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.vase) | -:!>(*^))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-volt
  :-  `tank`leaf+"test-volt"
  ::
  =/  hoon-src=@ta
    '''
    |_  cell=^
    ++  grab
      |%
      ++  noun  ^
      --
    --
    '''
  ::
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/foo/mar]]
      [%hoon !>(hoon-src)]
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            [%volt [~nul %home] %foo [12 13]]
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
        ?>  ?=([%success %volt *] build-result.pin-result)
        ::
        =/  =vase  q.cage.build-result.pin-result
        ::
        %+  weld
          %-  expect-eq  !>
          :-  [12 13]
          q.vase
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.vase) | -:!>(*^))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-vale
  :-  `tank`leaf+"test-vale"
  ::
  =/  hoon-src=@ta
    '''
    |_  cell=^
    ++  grab
      |%
      ++  noun  ^
      --
    --
    '''
  ::
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/foo/mar]]
      [%hoon !>(hoon-src)]
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            [%vale [~nul %home] %foo [12 13]]
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
        ?>  ?=([%success %vale *] build-result.pin-result)
        ::
        =/  =vase  q.cage.build-result.pin-result
        ::
        %+  weld
          %-  expect-eq  !>
          :-  [12 13]
          q.vase
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.vase) | -:!>(*^))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-vale-error
  :-  `tank`leaf+"test-vale-error"
  ::
  =/  hoon-src=@ta
    '''
    |_  cell=^
    ++  grab
      |%
      ++  noun  ^
      --
    --
    '''
  ::
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/foo/mar]]
      [%hoon !>(hoon-src)]
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            [%vale [~nul %home] %foo 42]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/path]  %give  %made  ~1234.5.6  %complete  %error
                :-  %leaf
                %+  weld
                  "ford: %vale failed: invalid input for mark: "
                "/~nul/home/~1234.5.6/mar/foo/hoon"
                ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-cast
  :-  `tank`leaf+"test-cast"
  ::
  =/  foo-mark-src=@ta
    '''
    |_  cell=^
    ++  grab
      |%
      ++  bar  ^
      --
    --
    '''
  ::
  =/  bar-mark-src=@ta
    '''
    |_  sample=[@ @]
    ++  grab
      |%
      +=  noun  [@ @]
      --
    --
    '''
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  arch-type=type  -:!>(*arch)
  ::
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/foo/mar]]
      [%hoon hoon-src-type foo-mark-src]
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/bar/mar]]
      [%hoon hoon-src-type bar-mark-src]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /mar]]
      :^  %arch  arch-type  ~
      (my ~[[~.foo ~] [~.bar ~]])
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /foo/mar]]
      [%arch arch-type ~ (my ~[[~.hoon ~]])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /bar/mar]]
      [%arch arch-type ~ (my ~[[~.hoon ~]])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/foo/mar]]
      [%arch arch-type fil=[~ u=0v1] ~]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/bar/mar]]
      [%arch arch-type fil=[~ u=0v2] ~]
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            [%cast [~nul %home] %foo [%vale [~nul %home] %bar [12 13]]]
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
        ?>  ?=([%success %cast *] build-result.pin-result)
        ::
        =/  mark=term  p.cage.build-result.pin-result
        =/  =vase      q.cage.build-result.pin-result
        ::
        ;:  welp
          %-  expect-eq  !>
          :-  %foo
          mark
        ::
          %-  expect-eq  !>
          :-  [12 13]
          q.vase
        ::
          %-  expect-eq  !>
          :-  &
          (~(nest ut p.vase) | -:!>(*^))
    ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-cast-grow
  :-  `tank`leaf+"test-cast-grow"
  ::
  =/  foo-mark-src=@ta
    '''
    |_  cell=^
    ++  grab
      |%
      ++  noun  ^
      --
    --
    '''
  ::
  =/  bar-mark-src=@ta
    '''
    |_  sample=[@ @]
    ++  grab
      |%
      +=  noun  [@ @]
      --
    ++  grow
      |%
      ++  foo  sample
      --
    --
    '''
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  arch-type=type  -:!>(*arch)
  ::
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/foo/mar]]
      [%hoon hoon-src-type foo-mark-src]
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/bar/mar]]
      [%hoon hoon-src-type bar-mark-src]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /mar]]
      :^  %arch  arch-type  ~
      (my ~[[~.foo ~] [~.bar ~]])
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /foo/mar]]
      [%arch arch-type ~ (my ~[[~.hoon ~]])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /bar/mar]]
      [%arch arch-type ~ (my ~[[~.hoon ~]])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/foo/mar]]
      [%arch arch-type fil=[~ u=0v1] ~]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/bar/mar]]
      [%arch arch-type fil=[~ u=0v2] ~]
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            [%cast [~nul %home] %foo [%vale [~nul %home] %bar [12 13]]]
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
        ?>  ?=([%success %cast *] build-result.pin-result)
        ::
        =/  mark=term  p.cage.build-result.pin-result
        =/  =vase      q.cage.build-result.pin-result
        ::
        ;:  welp
          %-  expect-eq  !>
          :-  %foo
          mark
        ::
          %-  expect-eq  !>
          :-  [12 13]
          q.vase
        ::
          %-  expect-eq  !>
          :-  &
          (~(nest ut p.vase) | -:!>([12 13]))
    ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-mute
  :-  `tank`leaf+"test-mute"
  ::
  =/  atom-type=type  [%atom %$ ~]
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            %mute  subject=[%$ %foo !>([a=42 b=[43 c=44]])]
            ^=  mutations  ^-  (list [wing schematic:ford-gate])
            :~
              [~[%a] [%$ %noun atom-type 2]]
              [~[%c %b] [%$ %noun atom-type 4]]
        ==  ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete %success %pin *] i.moves)
        =/  result  result.p.card.i.moves
        =/  pin-result  build-result.result
        ?>  ?=([%success %mute *] build-result.pin-result)
        ::
        =/  mark=term  p.cage.build-result.pin-result
        =/  =vase      q.cage.build-result.pin-result
        ::
        ;:  welp
          %-  expect-eq  !>
          :-  %foo
          mark
        ::
          %-  expect-eq  !>
          :-  [2 43 4]
          q.vase
        ::
          %-  expect-eq  !>
          :-  &
          (~(nest ut p.vase) | -:!>([2 43 4]))
    ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-bake-renderer
  :-  `tank`leaf+"test-bake-renderer"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/foo/ren]]
      :-  %hoon
      :-  hoon-src-type
      '''
      /=  data  /!noun/
      data
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/data]]
      :-  %hoon
      :-  hoon-src-type
      '''
      [1 2 3 ~]
      '''
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            [%bake %foo *coin `rail:ford-gate`[[~nul %home] /data]]
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
        ?>  ?=([%success %bake *] build-result.pin-result)
        ::
        =/  =cage  cage.build-result.pin-result
        ::
        %+  weld
          %-  expect-eq  !>
          :-  %noun
          p.cage
        ::
        %+  weld
          %-  expect-eq  !>
          :-  [1 2 3 ~]
          q.q.cage
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.q.cage) | -:!>([1 2 3 ~]))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-bake-mark
  :-  `tank`leaf+"test-bake-mark"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  arch-type=type  -:!>(*arch)
  ::
  =/  scry-results=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/foo/mar]]
      :^  ~  %hoon  hoon-src-type
      '''
      |_  cell=^
      ++  grab
        |%
        ++  bar  ^
        --
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/bar/mar]]
      :^  ~  %hoon  hoon-src-type
      '''
      |_  sample=[@ @]
      ++  grab
        |%
        +=  noun  [@ @]
        --
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/foo/ren]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /bar/data]]
      `[%bar !>([12 13])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /data]]
      `[%arch !>(`arch`[fil=~ dir=(my [%bar ~]~)])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /bar/data]]
      `[%arch !>(`arch`[fil=`*@uv dir=~])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /mar]]
      :-  ~
      :^  %arch  arch-type  ~
      (my ~[[~.foo ~] [~.bar ~]])
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /foo/mar]]
      `[%arch arch-type ~ (my ~[[~.hoon ~]])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /bar/mar]]
      `[%arch arch-type ~ (my ~[[~.hoon ~]])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/foo/mar]]
      `[%arch arch-type fil=[~ u=0v1] ~]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/bar/mar]]
      `[%arch arch-type fil=[~ u=0v2] ~]
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            [%bake %foo *coin `rail:ford-gate`[[~nul %home] /data]]
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
        ?>  ?=([%success %bake *] build-result.pin-result)
        ::
        =/  =cage  cage.build-result.pin-result
        ::
        %+  weld
          %-  expect-eq  !>
          :-  %foo
          p.cage
        ::
        %+  weld
          %-  expect-eq  !>
          :-  [12 13]
          q.q.cage
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.q.cage) | -:!>([12 13]))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-diff
  :-  `tank`leaf+"test-diff"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  arch-type=type  -:!>(*arch)
  ::
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/foo/mar]]
      :+  %hoon  hoon-src-type
      '''
      |_  cell=^
      ++  grab
        |%
        ++  noun  ^
        --
      ++  grad
        |%
        ++  diff  |=(^ +<)
        ++  form  %foo
        --
      --
      '''
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /mar]]
      :^  %arch  arch-type  ~
      (my ~[[~.foo ~]])
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /foo/mar]]
      [%arch arch-type ~ (my ~[[~.hoon ~]])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/foo/mar]]
      [%arch arch-type fil=[~ u=0v1] ~]
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            :^  %diff  [~nul %home]
              [%$ %foo !>([12 13])]
            [%$ %foo !>([17 18])]
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
        ?>  ?=([%success %diff *] build-result.pin-result)
        ::
        =/  =cage  cage.build-result.pin-result
        ::
        %+  weld
          %-  expect-eq  !>
          :-  %foo
          p.cage
        ::
        %+  weld
          %-  expect-eq  !>
          :-  [17 18]
          q.q.cage
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.q.cage) | -:!>([17 18]))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-diff-form
  :-  `tank`leaf+"test-diff-form"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  arch-type=type  -:!>(*arch)
  ::
  =/  scry-results=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/txt/mar]]
      :^  ~  %hoon  hoon-src-type
      '''
      |_  txt=wain
      ++  grab
        |%
        ++  noun  wain
        --
      ++  grad
        |%
        ++  form  %txt-diff
        ++  diff
          |=  other-txt=wain
          ^-  (urge:clay cord)
          =,  differ
          (lusk txt other-txt (loss txt other-txt))
        --
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/txt-diff/mar]]
      :^  ~  %hoon  hoon-src-type
      '''
      |_  txt-diff=(urge:clay cord)
      ++  grab
        |%
        ++  noun  (urge:clay cord)
        --
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/diff/txt/mar]]
      ~
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /mar]]
      :-  ~
      :^  %arch  arch-type  ~
      (my ~[[~.txt ~] [~.txt-diff ~]])
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /txt/mar]]
      `[%arch arch-type ~ (my ~[[~.hoon ~]])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /txt-diff/mar]]
      `[%arch arch-type ~ (my ~[[~.hoon ~]])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/txt/mar]]
      `[%arch arch-type fil=[~ u=0v1] ~]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/txt-diff/mar]]
      `[%arch arch-type fil=[~ u=0v2] ~]
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            :^  %diff  [~nul %home]
              [%$ %txt !>(~[%a %b])]
            [%$ %txt !>(~[%a %d])]
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
        ?>  ?=([%success %diff *] build-result.pin-result)
        ::
        =/  =cage  cage.build-result.pin-result
        ::
        %+  weld
          %-  expect-eq  !>
          :-  %txt-diff
          p.cage
        ::
        %+  weld
          %-  expect-eq  !>
          :-  ~[[%& 1] [%| ~[%b] ~[%d]]]
          q.q.cage
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.q.cage) | -:!>(~[[%& 1] [%| ~[%b] ~[%d]]]))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-pact
  :-  `tank`leaf+"test-pact"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  arch-type=type  -:!>(*arch)
  ::
  =/  scry-results=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/txt/mar]]
      :^  ~  %hoon  hoon-src-type
      '''
      |_  txt=wain
      ++  grab
        |%
        ++  noun  wain
        --
      ++  grad
        |%
        ++  form  %txt-diff
        ++  diff
          |=  other-txt=wain
          ^-  (urge:clay cord)
          =,  differ
          (lusk txt other-txt (loss txt other-txt))
        ++  pact
          |=  diff=(urge:clay cord)
          ^-  wain
          =,  differ
          (lurk txt diff)
        --
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/txt-diff/mar]]
      :^  ~  %hoon  hoon-src-type
      '''
      |_  txt-diff=(urge:clay cord)
      ++  grab
        |%
        ++  noun  (urge:clay cord)
        --
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/diff/txt/mar]]
      ~
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /mar]]
      :-  ~
      :^  %arch  arch-type  ~
      (my ~[[~.txt ~] [~.txt-diff ~]])
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /txt/mar]]
      `[%arch arch-type ~ (my ~[[~.hoon ~]])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /txt-diff/mar]]
      `[%arch arch-type ~ (my ~[[~.hoon ~]])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/txt/mar]]
      `[%arch arch-type fil=[~ u=0v1] ~]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/txt-diff/mar]]
      `[%arch arch-type fil=[~ u=0v2] ~]
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            :^  %pact  [~nul %home]
              [%$ %txt !>(~[%a %b])]
            [%$ %txt-diff !>(~[[%& 1] [%| ~[%b] ~[%d]]])]
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
        ?>  ?=([%success %pact *] build-result.pin-result)
        ::
        =/  =cage  cage.build-result.pin-result
        ::
        %+  weld
          %-  expect-eq  !>
          :-  %txt
          p.cage
        ::
        %+  weld
          %-  expect-eq  !>
          :-  ~[%a %d]
          q.q.cage
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.q.cage) | -:!>(~[%a %d]))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-pact-mark
  :-  `tank`leaf+"test-pact-mark"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  arch-type=type  -:!>(*arch)
  ::
  =/  scry-results=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/txt/mar]]
      :^  ~  %hoon  hoon-src-type
      '''
      |_  txt=wain
      ++  grab
        |%
        ++  noun  wain
        --
      ++  grad
        |%
        ++  form  %txt-diff
        ++  diff
          |=  other-txt=wain
          ^-  (urge:clay cord)
          =,  differ
          (lusk txt other-txt (loss txt other-txt))
        ++  pact
          |=  diff=(urge:clay cord)
          ^-  wain
          =,  differ
          (lurk txt diff)
        --
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/foo/mar]]
      :^  ~  %hoon  hoon-src-type
      '''
      |_  foo=@t
      ++  grab
        |%
        ++  noun  @t
        ++  txt   of-wain:format
        --
      ++  grow
        |%
        ++  txt  (to-wain:format foo)
        --
      ++  grad  %txt
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/txt-diff/mar]]
      :^  ~  %hoon  hoon-src-type
      '''
      |_  txt-diff=(urge:clay cord)
      ++  grab
        |%
        ++  noun  (urge:clay cord)
        --
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/diff/txt/mar]]
      ~
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /mar]]
      :-  ~
      :^  %arch  arch-type  ~
      (my ~[[~.txt ~] [~.foo ~] [~.txt-diff ~]])
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /txt/mar]]
      `[%arch arch-type ~ (my ~[[~.hoon ~]])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /txt-diff/mar]]
      `[%arch arch-type ~ (my ~[[~.hoon ~]])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /foo/mar]]
      `[%arch arch-type ~ (my ~[[~.hoon ~]])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/txt/mar]]
      `[%arch arch-type fil=[~ u=0v1] ~]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/txt-diff/mar]]
      `[%arch arch-type fil=[~ u=0v2] ~]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/foo/mar]]
      `[%arch arch-type fil=[~ u=0v3] ~]
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            :^  %pact  [~nul %home]
              :+  %$  %foo  !>
              '''
              a
              b
              '''
            [%$ %txt-diff !>(~[[%& 1] [%| ~[%b] ~[%d]]])]
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
        ?>  ?=([%success %pact *] build-result.pin-result)
        ::
        =/  =cage  cage.build-result.pin-result
        ::
        %+  weld
          %-  expect-eq  !>
          :-  %foo
          p.cage
        ::
        %+  weld
          %-  expect-eq  !>
          :-  '''
              a
              d
              '''
          q.q.cage
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.q.cage) | -:!>(''))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-join
  :-  `tank`leaf+"test-join"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  ::
  =/  scry-results=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/txt/mar]]
      :^  ~  %hoon  hoon-src-type
      .^(@t %cx (en-beam:format [bek /hoon/txt/mar]))
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/txt-diff/mar]]
      :^  ~  %hoon  hoon-src-type
      .^(@t %cx (en-beam:format [bek /hoon/txt-diff/mar]))
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/diff/txt/mar]]
      ~
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            :^  %join  [~nul %home]  %txt
            ::  replace %a with %c on the first line
            ::
            :-  [%$ %txt-diff !>(~[[%| ~[%a] ~[%c]] [%& 1]])]
            ::  replace %b with %d on the second line
            ::
            [%$ %txt-diff !>(~[[%& 1] [%| ~[%b] ~[%d]]])]
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
        ?>  ?=([%success %join *] build-result.pin-result)
        ::
        =/  =cage  cage.build-result.pin-result
        ::
        %+  weld
          %-  expect-eq  !>
          :-  %txt-diff
          p.cage
        ::
        %+  weld
          %-  expect-eq  !>
          :-  ~[[%| ~[%a] ~[%c]] [%| ~[%b] ~[%d]]]
          q.q.cage
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.q.cage) | -:!>(*(urge:clay cord)))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-list
  :-  `tank`leaf+"test-list"
  ::
  =/  ud-type=type  [%atom %ud ~]
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      ^=  call-args
        :*  duct=~[/count]  type=~  %build  ~nul
            %pin  ~1234.5.6  %list
            :~  [%$ %noun ud-type 1]
                [%$ %noun ud-type 2]
                [%$ %noun ud-type 3]
        ==  ==
      ::
      ^=  moves
        :~  :*  duct=~[/count]  %give  %made  ~1234.5.6  %complete
                %success  %pin  ~1234.5.6
                %success  %list
                :~  [%success %$ %noun ud-type 1]
                    [%success %$ %noun ud-type 2]
                    [%success %$ %noun ud-type 3]
    ==  ==  ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-mash
  :-  `tank`leaf+"test-mash"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  ::
  =/  scry-results=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/txt/mar]]
      :^  ~  %hoon  hoon-src-type
      .^(@t %cx (en-beam:format [bek /hoon/txt/mar]))
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/txt-diff/mar]]
      :^  ~  %hoon  hoon-src-type
      .^(@t %cx (en-beam:format [bek /hoon/txt-diff/mar]))
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/diff/txt/mar]]
      ~
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  ~nul
            %pin  ~1234.5.6
            ^-  schematic:ford-gate
            :-  %mash
            :^  [~nul %home]  %txt
            ::  replace %a with %c on the first line
            ::
            ^=  first
            :+  [~nul %home]  %txt-diff
            ^-  schematic:ford-gate
            [%$ %txt-diff !>(~[[%| ~[%a] ~[%c]] [%& 1]])]
            ::  replace %b with %d on the second line
            ::
            ^=  second
            :+  [~nul %home]  %txt-diff
            ^-  schematic:ford-gate
            [%$ %txt-diff !>(~[[%& 1] [%| ~[%b] ~[%d]]])]
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
        ?>  ?=([%success %mash *] build-result.pin-result)
        ::
        =/  =cage  cage.build-result.pin-result
        ::
        %+  weld
          %-  expect-eq  !>
          :-  %txt-diff
          p.cage
        ::
        %+  weld
          %-  expect-eq  !>
          :-  ~[[%| ~[%a] ~[%c]] [%| ~[%b] ~[%d]]]
          q.q.cage
        ::
        %-  expect-eq  !>
        :-  &
        (~(nest ut p.q.cage) | -:!>(*(urge:clay cord)))
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::  tests multiple cores depending on a rich dependency tree
::
::    Test why multiple app cores don't receive dependencies
::
++  test-multi-core-same-dependency
  :-  `tank`leaf+"test-multi-core-same-dependency"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  ::
  =/  scry-results=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/gh/app]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /?  314
      /-  gh, plan-acct
      /+  gh-parse, connector
      ::
      (add sur-gh-two:gh lib-connector-val:connector)
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/gh/sur]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      [sur-gh-one=1 sur-gh-two=2 sur-gh-three=3]
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/plan-acct/sur]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/acct/plan/sur]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      [sur-plan-acct=1 sur-plan-acct=2]
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/lib/old/lib]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/old-lib/lib]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      [old-lib-val=10 ~]
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/parse/gh/lib]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/gh-parse/lib]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /-  gh
      /+  old-lib
      =,  old-lib
      [lib-gh-parse-val=(add old-lib-val sur-gh-three:gh) ~]
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/connector/lib]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /-  gh
      /+  old-lib
      =,  old-lib
      [lib-connector-val=(add old-lib-val sur-gh-one:gh) ~]
      '''
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/gh]  type=~  %build  ~nul
            %core  [[~nul %home] /hoon/gh/app]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(2 (lent moves))
        ?>  ?=([^ ^ ~] moves)
        ?>  ?=([* %give %made @da %complete %success %core *] i.moves)
        ::
        =/  =vase  vase.build-result.result.p.card.i.moves
        ::
        %+  weld
          %-  expect-eq  !>
          :-  13
          q.vase
        ::
        %+  weld
          %-  expect-eq  !>
          :-  &
          (~(nest ut p.vase) | -:!>(13))
        ::
        =/  files=(set [%x path])
          %-  sy  :~
            [%x /lib/old-lib/hoon]
            [%x /sur/gh/hoon]
            [%x /sur/plan-acct/hoon]
            [%x /sur/plan/acct/hoon]
            [%x /lib/old/lib/hoon]
            [%x /app/gh/hoon]
            [%x /lib/gh/parse/hoon]
            [%x /lib/gh-parse/hoon]
            [%x /lib/connector/hoon]
          ==
        ::
        %-  expect-eq  !>
        :_  i.t.moves
        ^-  move:ford-gate
        :*  duct=~[/gh]  %pass  wire=/~nul/clay-sub/~nul/home
            %c  %warp  [~nul ~nul]  %home
            `[%mult [%da ~1234.5.6] files]
        ==
    ==
  ::  add a gh2 app which is the same as gh; move dates forward
  ::
  ::    gh2 is the same except for adding one instead of two.
  =.  scry-results
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.7] /hoon/gh/app]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /?  314
      /-  gh, plan-acct
      /+  gh-parse, connector
      ::
      (add sur-gh-two:gh lib-connector-val:connector)
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.7] /hoon/gh2/app]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /?  314
      /-  gh, plan-acct
      /+  gh-parse, connector
      ::
      (add sur-gh-one:gh lib-connector-val:connector)
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.7] /hoon/gh/sur]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      [sur-gh-one=1 sur-gh-two=2 sur-gh-three=3]
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.7] /hoon/plan-acct/sur]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.7] /hoon/acct/plan/sur]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      [sur-plan-acct=1 sur-plan-acct=2]
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.7] /hoon/lib/old/lib]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.7] /hoon/old-lib/lib]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      [old-lib-val=10 ~]
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.7] /hoon/parse/gh/lib]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.7] /hoon/gh-parse/lib]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /-  gh
      /+  old-lib
      =,  old-lib
      [lib-gh-parse-val=(add old-lib-val sur-gh-three:gh) ~]
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.7] /hoon/connector/lib]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /-  gh
      /+  old-lib
      =,  old-lib
      [lib-connector-val=(add old-lib-val sur-gh-one:gh) ~]
      '''
    ==
  ::
  =^  results2  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.7
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/gh2]  type=~  %build  ~nul
            %core  [[~nul %home] /hoon/gh2/app]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(3 (lent moves))
        ?>  ?=([^ ^ ^ ~] moves)
        ?>  ?=([* %give %made @da %complete %success %core *] i.moves)
        ::
        =/  =vase  vase.build-result.result.p.card.i.moves
        ::
        %+  weld
          %-  expect-eq  !>
          :-  12
          q.vase
        ::
        %+  weld
          %-  expect-eq  !>
          :-  &
          (~(nest ut p.vase) | -:!>(12))
        ::
        %+  weld
          %-  expect-eq  !>
          :_  i.t.moves
          ^-  move:ford-gate
          :*  duct=~[/gh]  %pass  wire=/~nul/clay-sub/~nul/home
            %c  %warp  [~nul ~nul]  %home  ~
          ==
        ::
        =/  files=(set [%x path])
          %-  sy  :~
            [%x /lib/old-lib/hoon]
            [%x /sur/gh/hoon]
            [%x /sur/plan-acct/hoon]
            [%x /sur/plan/acct/hoon]
            [%x /lib/old/lib/hoon]
            [%x /app/gh/hoon]
            [%x /app/gh2/hoon]
            [%x /lib/gh/parse/hoon]
            [%x /lib/gh-parse/hoon]
            [%x /lib/connector/hoon]
          ==
        ::
        %-  expect-eq  !>
        :_  i.t.t.moves
        ^-  move:ford-gate
        :*  duct=~[/gh2]  %pass  wire=/~nul/clay-sub/~nul/home
            %c  %warp  [~nul ~nul]  %home
            `[%mult [%da ~1234.5.7] files]
        ==
    ==
  ::
  ::  add a gh3 app which is the same as gh; move dates forward
  ::
  ::    gh3 is the same except for adding zero instead of two.
  =.  scry-results
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.8] /hoon/gh/app]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /?  314
      /-  gh, plan-acct
      /+  gh-parse, connector
      ::
      (add sur-gh-two:gh lib-connector-val:connector)
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.8] /hoon/gh2/app]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /?  314
      /-  gh, plan-acct
      /+  gh-parse, connector
      ::
      (add sur-gh-one:gh lib-connector-val:connector)
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.8] /hoon/gh3/app]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /?  314
      /-  gh, plan-acct
      /+  gh-parse, connector
      ::
      (add 0 lib-connector-val:connector)
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.8] /hoon/gh/sur]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      [sur-gh-one=1 sur-gh-two=2 sur-gh-three=3]
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.8] /hoon/plan-acct/sur]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.8] /hoon/acct/plan/sur]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      [sur-plan-acct=1 sur-plan-acct=2]
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.8] /hoon/lib/old/lib]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.8] /hoon/old-lib/lib]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      [old-lib-val=10 ~]
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.8] /hoon/parse/gh/lib]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.8] /hoon/gh-parse/lib]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /-  gh
      /+  old-lib
      =,  old-lib
      [lib-gh-parse-val=(add old-lib-val sur-gh-three:gh) ~]
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.8] /hoon/connector/lib]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /-  gh
      /+  old-lib
      =,  old-lib
      [lib-connector-val=(add old-lib-val sur-gh-one:gh) ~]
      '''
    ==
  ::
  =^  results3  ford-gate
    %-  test-ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.8
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/gh3]  type=~  %build  ~nul
            %core  [[~nul %home] /hoon/gh3/app]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(3 (lent moves))
        ?>  ?=([^ ^ ^ ~] moves)
        ?>  ?=([* %give %made @da %complete %success %core *] i.moves)
        ::
        =/  =vase  vase.build-result.result.p.card.i.moves
        ::
        %+  weld
          %-  expect-eq  !>
          :-  11
          q.vase
        ::
        %+  weld
          %-  expect-eq  !>
          :-  &
          (~(nest ut p.vase) | -:!>(11))
        ::
        %+  weld
          %-  expect-eq  !>
          :_  i.t.moves
          ^-  move:ford-gate
          :*  duct=~[/gh2]  %pass  wire=/~nul/clay-sub/~nul/home
            %c  %warp  [~nul ~nul]  %home  ~
          ==
        ::
        =/  files=(set [%x path])
          %-  sy  :~
            [%x /lib/old-lib/hoon]
            [%x /sur/gh/hoon]
            [%x /sur/plan-acct/hoon]
            [%x /sur/plan/acct/hoon]
            [%x /lib/old/lib/hoon]
            [%x /app/gh/hoon]
            [%x /app/gh2/hoon]
            [%x /app/gh3/hoon]
            [%x /lib/gh/parse/hoon]
            [%x /lib/gh-parse/hoon]
            [%x /lib/connector/hoon]
          ==
        ::
        %-  expect-eq  !>
        :_  i.t.t.moves
        ^-  move:ford-gate
        :*  duct=~[/gh3]  %pass  wire=/~nul/clay-sub/~nul/home
            %c  %warp  [~nul ~nul]  %home
            `[%mult [%da ~1234.5.8] files]
        ==
    ==
  ::
  ::  change the implementation of /lib/connector/hoon
  ::
  =.  scry-results
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.9] /hoon/gh/app]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /?  314
      /-  gh, plan-acct
      /+  gh-parse, connector
      ::
      (add sur-gh-two:gh lib-connector-val:connector)
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.9] /hoon/gh2/app]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /?  314
      /-  gh, plan-acct
      /+  gh-parse, connector
      ::
      (add sur-gh-one:gh lib-connector-val:connector)
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.9] /hoon/gh3/app]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /?  314
      /-  gh, plan-acct
      /+  gh-parse, connector
      ::
      (add 0 lib-connector-val:connector)
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.9] /hoon/gh/sur]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      [sur-gh-one=1 sur-gh-two=2 sur-gh-three=3]
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.9] /hoon/plan-acct/sur]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.9] /hoon/acct/plan/sur]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      [sur-plan-acct=1 sur-plan-acct=2]
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.9] /hoon/lib/old/lib]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.9] /hoon/old-lib/lib]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      [old-lib-val=10 ~]
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.9] /hoon/parse/gh/lib]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.9] /hoon/gh-parse/lib]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /-  gh
      /+  old-lib
      =,  old-lib
      [lib-gh-parse-val=(add old-lib-val sur-gh-three:gh) ~]
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.9] /hoon/connector/lib]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /-  gh
      /+  old-lib
      =,  old-lib
      [lib-connector-val=(add old-lib-val 5) ~]
      '''
    ==
  ::
  =^  results4  ford-gate
    %-  test-ford-take-with-comparator  :*
      ford-gate
      now=~1234.5.9
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  take-args
        :*  wire=/~nul/clay-sub/~nul/home  duct=~[/gh3]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.9] (sy [%x /lib/connector/hoon]~)]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(4 (lent moves))
        ?>  ?=([^ ^ ^ ^ ~] moves)
        ?>  ?=([* %give %made @da %complete %success %core *] i.moves)
        ?>  ?=([* %give %made @da %complete %success %core *] i.t.moves)
        ?>  ?=([* %give %made @da %complete %success %core *] i.t.t.moves)
        ::
        =/  =vase  vase.build-result.result.p.card.i.moves
        ::
        =/  files=(set [%x path])
          %-  sy  :~
            [%x /lib/old-lib/hoon]
            [%x /sur/gh/hoon]
            [%x /sur/plan-acct/hoon]
            [%x /sur/plan/acct/hoon]
            [%x /lib/old/lib/hoon]
            [%x /app/gh/hoon]
            [%x /app/gh2/hoon]
            [%x /app/gh3/hoon]
            [%x /lib/gh/parse/hoon]
            [%x /lib/gh-parse/hoon]
            [%x /lib/connector/hoon]
          ==
        ::
        ;:  weld
          (expect-eq !>([~[/gh3] duct.i.moves]))
          (compare-vase !>(15) vase.build-result.result.p.card.i.moves)
        ::
          (expect-eq !>([~[/gh2] duct.i.t.moves]))
          (compare-vase !>(16) vase.build-result.result.p.card.i.t.moves)
        ::
          (expect-eq !>([~[/gh] duct.i.t.t.moves]))
          (compare-vase !>(17) vase.build-result.result.p.card.i.t.t.moves)
        ::
          %-  expect-eq  !>
          :_  i.t.t.t.moves
          ^-  move:ford-gate
          :*  duct=~[/gh3]  %pass  wire=/~nul/clay-sub/~nul/home
              %c  %warp  [~nul ~nul]  %home
              `[%mult [%da ~1234.5.9] files]
          ==
    ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
  ==
::  tests that we can do the simple adjacent mark case, and that we use grab
::  when both available.
::
++  test-walk-prefer-grab
  :-  `tank`leaf+"test-walk-prefer-grab"
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  arch-type=type  -:!>(*arch)
  ::
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/one/mar]]
      :-  %hoon
      :-  hoon-src-type
      '''
      |_  [a=tape b=@ud]
      ::  convert to
      ++  grow
        |%
        ++  two  [b a "grow"]
        --
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/two/mar]]
      :-  %hoon
      :-  hoon-src-type
      '''
      |_  [a=@ud b=tape c=type]
      ++  grab
        |%
        ++  one  |=([a=tape b=@ud] [b a "grab"])
        --
      --
      '''
    ::
      ::  make sure we can deal with random not-hoon files in mar
      :-  [%cy [[~nul %home %da ~1234.5.6] /js/dummy/mar]]
      :-  %js
      :-  hoon-src-type
      '''
      window.onload = function()
      '''
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /mar]]
      :-  %arch
      :-  arch-type
      :-  ~
      (my ~[[~.one ~] [~.two ~] [~.dummy ~]])
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /one/mar]]
      :-  %arch
      :-  arch-type
      :-  ~
      (my ~[[~.hoon ~]])
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /two/mar]]
      :-  %arch
      :-  arch-type
      :-  ~
      (my ~[[~.hoon ~]])
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /dummy/mar]]
      :-  %arch
      :-  arch-type
      :-  ~
      (my ~[[~.js ~]])
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/one/mar]]
      :-  %arch
      :-  arch-type
      :-  fil=[~ u=0v1]
      ~
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/two/mar]]
      :-  %arch
      :-  arch-type
      :-  fil=[~ u=0v2]
      ~
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /js/dummy/mar]]
      :-  %arch
      :-  arch-type
      :-  fil=[~ u=0v3]
      ~
    ==
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/walk]  type=~  %build  ~nul
            %walk  [~nul %home]  %one  %two
        ==
      ::
      ^=  moves
        :~  ^-  move:ford-gate
            :*  duct=~[/walk]  %give  %made  ~1234.5.6
                %complete  %success  %walk
                [%grab %one %two]~
            ==
            ^-  move:ford-gate
            :*  duct=~[/walk]  %pass  /~nul/clay-sub/~nul/home
                %c  %warp  [~nul ~nul]  %home  ~  %mult  [%da ~1234.5.6]
                %-  sy  :~
                  [%x /mar/two/hoon]  [%x /mar/one/hoon]
    ==  ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/walk] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~[/walk]  %pass  /~nul/clay-sub/~nul/home
                %c  %warp  [~nul ~nul]  %home  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-walk-large-graph
  :-  `tank`leaf+"test-walk-large-graph"
  ::
  =^  results1  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results large-mark-graph)
      ::
      ^=  call-args
        :*  duct=~[/walk]  type=~  %build  ~nul
            %walk  [~nul %home]  %one  %four
        ==
      ::
      ^=  moves
        :~  ^-  move:ford-gate
            :*  duct=~[/walk]  %give  %made  ~1234.5.6
                %complete  %success  %walk
                [[%grab %one %two] [%grab %two %four] ~]
            ==
            ^-  move:ford-gate
            :*  duct=~[/walk]  %pass  /~nul/clay-sub/~nul/home
                %c  %warp  [~nul ~nul]  %home  ~  %mult  [%da ~1234.5.6]
                %-  sy  :~
                  [%x /mar/one/hoon]  [%x /mar/two/hoon]
                  [%x /mar/four/hoon]  [%x /mar/five/hoon]
    ==  ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  test-ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/walk] type=~ %kill ~nul]
      ::
      ^=  moves
        :~  :*  duct=~[/walk]  %pass  /~nul/clay-sub/~nul/home
                %c  %warp  [~nul ~nul]  %home  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    (expect-ford-empty ford-gate ~nul)
  ==
::  |data: shared data between cases
::  +|  data
++  large-mark-graph
  ^-  (map [term beam] cage)
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  arch-type=type  -:!>(*arch)
  ::
  %-  my  :~
    :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/one/mar]]
    :-  %hoon
    :-  hoon-src-type
    '''
    |_  [a=tape b=@ud]
    ::  convert to
    ++  grow
      |%
      ++  two  [b a "grow"]
      ++  five  b
      --
    --
    '''
  ::
    :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/two/mar]]
    :-  %hoon
    :-  hoon-src-type
    '''
    |_  [a=@ud b=tape c=tape]
    ++  grab
      |%
      ++  one  |=([a=tape b=@ud] [b a "grab"])
      --
    --
    '''
  ::
    :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/three/mar]]
    :-  %hoon
    :-  hoon-src-type
    '''
    |_  [b=tape c=tape]
    ++  grab
      |%
      ++  one  |=([a=tape b=@ud] [a "grab"])
      --
    ++  grow
      |%
      ++  two
        [b c]
      --
    --
    '''
  ::
    :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/four/mar]]
    :-  %hoon
    :-  hoon-src-type
    '''
    |_  [c=tape b=tape]
    ++  grab
      |%
      ++  two
        |=  [a=@ud b=tape c=tape]
        [c b]
      --
    --
    '''
  ::
    :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/five/mar]]
    :-  %hoon
    :-  hoon-src-type
    '''
    |_  a=@u
    ++  grab
      |%
      ++  four
        ::  ignore the value entirely
        |=  [c=tape b=tape]
        5
      --
    ++  grow
      |%
      ++  one
        [a "empty" "grow"]
      --
    --
    '''
  ::
    :-  [%cy [[~nul %home %da ~1234.5.6] /mar]]
    :-  %arch
    :-  arch-type
    :-  ~
    (my ~[[~.one ~] [~.two ~] [~.three ~] [~.four ~] [~.five ~]])
  ::
    :-  [%cy [[~nul %home %da ~1234.5.6] /one/mar]]
    :-  %arch
    :-  arch-type
    :-  ~
    (my ~[[~.hoon ~]])
  ::
    :-  [%cy [[~nul %home %da ~1234.5.6] /two/mar]]
    :-  %arch
    :-  arch-type
    :-  ~
    (my ~[[~.hoon ~]])
  ::
    :-  [%cy [[~nul %home %da ~1234.5.6] /three/mar]]
    :-  %arch
    :-  arch-type
    :-  ~
    (my ~[[~.hoon ~]])
  ::
    :-  [%cy [[~nul %home %da ~1234.5.6] /four/mar]]
    :-  %arch
    :-  arch-type
    :-  ~
    (my ~[[~.hoon ~]])
  ::
    :-  [%cy [[~nul %home %da ~1234.5.6] /five/mar]]
    :-  %arch
    :-  arch-type
    :-  ~
    (my ~[[~.hoon ~]])
  ::
    :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/one/mar]]
    :-  %arch
    :-  arch-type
    :-  fil=[~ u=0v1]
    ~
  ::
    :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/two/mar]]
    :-  %arch
    :-  arch-type
    :-  fil=[~ u=0v2]
    ~
  ::
    :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/three/mar]]
    :-  %arch
    :-  arch-type
    :-  fil=[~ u=0v3]
    ~
  ::
    :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/four/mar]]
    :-  %arch
    :-  arch-type
    :-  fil=[~ u=0v4]
    ~
  ::
    :-  [%cy [[~nul %home %da ~1234.5.6] /hoon/five/mar]]
    :-  %arch
    :-  arch-type
    :-  fil=[~ u=0v5]
    ~
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
::  +compare-vase: compares the value of a vase and ensure that the types nest
::
++  compare-vase
  |=  [expected=vase actual=vase]
  ^-  tang
  %+  weld
    %-  expect-eq  !>
    :-  q.expected
    q.actual
  ::
  %-  expect-eq  !>
  :-  &
  (~(nest ut p.actual) | p.expected)
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
  |=  $:  ford-gate=_ford-gate
          now=@da
          scry=sley
          call-args=[=duct type=* wrapped-task=(hobo task:able:ford-gate)]
          expected-moves=(list move:ford-gate)
      ==
  ^-  [tang _ford-gate]
  ::
  =/  ford  (ford-gate now=now eny=0xdead.beef scry=scry)
  ::
  =^  moves  ford-gate
    %-  call:ford  call-args
  ::
  =/  output=tang
    %-  expect-eq  !>
    :-  expected-moves
    moves
  ::
  [output ford-gate]
::
++  test-ford-take
  |=  $:  ford-gate=_ford-gate
          now=@da
          scry=sley
          take-args=[=wire =duct wrapped-sign=(hypo sign:ford-gate)]
          expected-moves=(list move:ford-gate)
      ==
  ^-  [tang _ford-gate]
  ::
  =/  ford  (ford-gate now=now eny=0xdead.beef scry=scry)
  ::
  =^  moves  ford-gate
    %-  take:ford  take-args
  ::
  =/  output=tang
    %-  expect-eq  !>
    :-  expected-moves
    moves
  ::
  [output ford-gate]
::  +test-ford-call-with-comparator
::
::    Sometimes we can't just do simple comparisons between the moves statements and
::    must instead specify a gate that performs the comparisons.
::
++  test-ford-call-with-comparator
  |=  $:  ford-gate=_ford-gate
          now=@da
          scry=sley
          call-args=[=duct type=* wrapped-task=(hobo task:able:ford-gate)]
          move-comparator=$-((list move:ford-gate) tang)
      ==
  ^-  [tang _ford-gate]
  ::
  =/  ford  (ford-gate now=now eny=0xdead.beef scry=scry)
  ::
  =^  moves  ford-gate
    %-  call:ford  call-args
  ::
  =/  output=tang  (move-comparator moves)
  ::
  [output ford-gate]
::  +test-ford-take-with-comparator
::
++  test-ford-take-with-comparator
  |=  $:  ford-gate=_ford-gate
          now=@da
          scry=sley
          take-args=[=wire =duct wrapped-sign=(hypo sign:ford-gate)]
          move-comparator=$-((list move:ford-gate) tang)
      ==
  ^-  [tang _ford-gate]
  ::
  =/  ford  (ford-gate now=now eny=0xdead.beef scry=scry)
  ::
  =^  moves  ford-gate
    %-  take:ford  take-args
  ::
  =/  output=tang  (move-comparator moves)
  ::
  [output ford-gate]
::  +expect-ford-empty: assert that ford's state is one empty ship
::
::    At the end of every test, we want to assert that we have cleaned up all
::    state.
::
++  expect-ford-empty
  |=  [ford-gate=_ford-gate ship=@p]
  ^-  tang
  =/  ford  *ford-gate
  %-  expect-eq  !>
  :-  (my [ship *ford-state:ford]~)
  state-by-ship.ax.+>+<.ford
--
