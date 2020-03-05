/+  *test, *test-ford
::
|%
++  test-tear  ^-  tang
  ::
  ;:  welp
    %+  expect-eq
      !>  ~['a' 'bc' 'de']
      !>  (tear:ford-gate 'a-bc-de')
  ::
    %+  expect-eq
      !>  ~['abc']
      !>  (tear:ford-gate 'abc')
  ::
    %+  expect-eq
      !>  ~['ab/c']
      !>  (tear:ford-gate 'ab/c')
  ==
::
++  test-unify-jugs  ^-  tang
  ::
  %+  expect-eq
    !>  ^-  (jug @tas @ud)
        (my ~[[%a (sy 1 2 ~)] [%b (sy 3 4 5 6 ~)] [%c (sy 7 8 ~)]])
    !>  %+  unify-jugs:ford-gate
          `(jug @tas @ud)`(my ~[[%a (sy 1 2 ~)] [%b (sy 3 4 ~)]])
        `(jug @tas @ud)`(my ~[[%b (sy 5 6 ~)] [%c (sy 7 8 ~)]])
::
++  test-resource-wire-encoding  ^-  tang
  ::
  ;:  welp
    %+  expect-eq
      !>  /cx/~nul/desk/~1234.5.6/bar/foo
      !>  ^-  path
          %-  scry-request-to-path:ford-gate
          [%c care=%x [[~nul %desk [%da ~1234.5.6]] /foo/bar]]
  ::
    %+  expect-eq
      !>  [%c care=%x [[~nul %desk [%da ~1234.5.6]] /foo/bar]]
      !>  %-  need
          (path-to-scry-request:ford-gate /cx/~nul/desk/~1234.5.6/bar/foo)
  ==
::
++  test-parse-scaffold  ^-  tang
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /bar/foo]))
      [1 1]
    "!.  |=(a=@ud +(a))"
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  ::
  %+  expect-eq
    !>  :-  [1 19]
        :-  ~
        :_  [[1 19] ""]
        ^-  scaffold:ford-gate
        :*  source-rail=[[~nul %desk] /bar/foo]
            zuse-version=%309
            structures=~
            libraries=~
            cranes=~
            ^=  sources
              :~  :*  %dbug  [/~nul/desk/~1234.5.6/foo/bar [[1 1] [1 19]]]
                      (ream '!.  |=(a=@ud +(a))')
        ==    ==  ==
    !>  parsed
::
++  test-parse-scaffold-sur-libk  ^-  tang
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
  %+  expect-eq
    !>  :*  source-rail=[[~nul %desk] /bar/foo]
            zuse-version=309
            structures=~[[`%struct %struct] [`%face %other]]
            libraries=~[[`%library %library] [~ %thing]]
            cranes=~
            ^=  sources
              :~  :*  %dbug
                      [/~nul/desk/~1234.5.6/foo/bar [3 1] [4 8]]
                      (ream '|=(a a)')
        ==    ==  ==
    !>  p.u.q.parsed
::
++  test-parse-scaffold-zuse-version  ^-  tang
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
  %+  expect-eq
    !>  :*  source-rail=[[~nul %desk] /bar/foo]
            zuse-version=400
            structures=~
            libraries=~
            cranes=~
            ^=  sources
              :~  :*  %dbug
                      [/~nul/desk/~1234.5.6/foo/bar [2 1] [3 8]]
                      (ream '|=(a a)')
        ==    ==  ==
    !>  p.u.q.parsed
::
++  test-parse-scaffold-crane-fssg  ^-  tang
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
  %+  expect-eq
    !>  :*  source-rail=[[~nul %desk] /bar/foo]
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
    !>  p.u.q.parsed
::
++  test-parse-scaffold-crane-fsbc  ^-  tang
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
  %+  expect-eq
    !>  :*  source-rail=[[~nul %desk] /bar/foo]
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
    !>  p.u.q.parsed
::
++  test-parse-scaffold-crane-fsbr  ^-  tang
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
  %+  expect-eq
    !>  :*  source-rail=[[~nul %desk] /bar/foo]
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
    !>  p.u.q.parsed
::
++  test-parse-scaffold-crane-fsts  ^-  tang
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
  %+  expect-eq
    !>  :*  source-rail=[[~nul %desk] /bar/foo]
            zuse-version=309
            structures=~
            libraries=~
            ^=  crane
              :~  :*  %fsts  %a
                      %fssg  %dbug  [/~nul/desk/~1234.5.6/foo/bar [1 12] [1 13]]
                      [%bust %null]
              ==  ==
            ^=  sources
              :~  :*  %dbug
                      [/~nul/desk/~1234.5.6/foo/bar [2 1] [2 2]]  [%sand %ud 5]
        ==    ==  ==
    !>  p.u.q.parsed
::
++  test-parse-scaffold-crane-fsdt  ^-  tang
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
  %+  expect-eq
    !>  :*  source-rail=[[~nul %desk] /bar/foo]
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
              :~  :*  %dbug
                      [/~nul/desk/~1234.5.6/foo/bar [4 1] [4 2]]  [%sand %ud 5]
        ==    ==  ==
    !>  p.u.q.parsed
::
++  test-parse-scaffold-crane-fscm  ^-  tang
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
  %+  expect-eq
    !>  :*  source-rail=[[~nul %desk] /bar/foo]
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
              :~  :*  %dbug
                      [/~nul/desk/~1234.5.6/foo/bar [8 1] [8 2]]  [%sand %ud 1]
        ==    ==  ==
    !>  p.u.q.parsed
::
++  test-parse-scaffold-crane-fspm  ^-  tang
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
  %+  expect-eq
    !>  :*  source-rail=[[~nul %desk] /bar/foo]
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
              :~  :*  %dbug
                      [/~nul/desk/~1234.5.6/foo/bar [2 1] [2 2]]  [%sand %ud 1]
        ==    ==  ==
    !>  p.u.q.parsed
::
++  test-parse-scaffold-crane-fscb  ^-  tang
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
  %+  expect-eq
    !>  :*  source-rail=[[~nul %desk] /bar/foo]
            zuse-version=309
            structures=~
            libraries=~
            ^=  crane
              :~  :*  %fscb  %fszy  %mark
              ==  ==
            ^=  sources
              :~  :*  %dbug
                      [/~nul/desk/~1234.5.6/foo/bar [2 1] [2 2]]  [%sand %ud 8]
        ==    ==  ==
    !>  p.u.q.parsed
::
++  test-parse-scaffold-crane-fssm  ^-  tang
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
  %+  expect-eq
    !>  :*  source-rail=[[~nul %desk] /bar/foo]
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
              :~  :*  %dbug
                      [/~nul/desk/~1234.5.6/foo/bar [4 1] [4 2]]  [%sand %ud 7]
        ==    ==  ==
    !>  p.u.q.parsed
::
++  test-parse-scaffold-crane-fscl  ^-  tang
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
  %+  expect-eq
    !>  :*  source-rail=[[~nul %desk] /bar/foo]
            zuse-version=309
            structures=~
            libraries=~
            ^=  crane
              :~  :*  %fsts  %tests
                      %fscl  [[~ ~[~ ~ ~ [~ [%sand %tas 495.874.958.708]]]] ~]
                      %fscb  %fszy  %mark
              ==  ==
            ^=  sources
              :~  :*  %dbug
                      [/~nul/desk/~1234.5.6/foo/bar [4 1] [4 2]]  [%sand %ud 3]
        ==    ==  ==
    !>  p.u.q.parsed
::
++  test-parse-scaffold-crane-fskt  ^-  tang
  ::
  =/  parsed
    %+  (full (parse-scaffold:ford-gate [[~nul %desk %da ~1234.5.6] /bar/foo]))
      [1 1]
    """
    /=  data
      /^  (list @ud)
      /.  /~  1
          /~  2
          ==
    6
    """
  ?~  q.parsed
    [%leaf "failed to parse at {<p.parsed>}"]~
  %+  expect-eq
    !>  :*  source-rail=[[~nul %desk] /bar/foo]
            zuse-version=309
            structures=~
            libraries=~
            ^=  crane
              :~  :*  %fsts  %data
                      %fskt
                      :*  %dbug
                          [/~nul/desk/~1234.5.6/foo/bar [2 7] [2 17]]
                          %make
                          :+  %dbug
                            [/~nul/desk/~1234.5.6/foo/bar [2 8] [2 12]]
                          [%wing ~[%list]]
                          :~  :+  %dbug
                                [/~nul/desk/~1234.5.6/foo/bar [2 13] [2 16]]
                              [%base [%atom %ud]]
                          ==
                      ==
                      %fsdt
                      :~  :-  %fssg
                          :+  %dbug
                            [/~nul/desk/~1234.5.6/foo/bar [3 11] [3 12]]
                          [%sand %ud 1]
                          ::
                          :-  %fssg
                          :+  %dbug
                            [/~nul/desk/~1234.5.6/foo/bar [4 11] [4 12]]
                          [%sand %ud 2]
              ==  ==  ==
            ^=  sources
              :~  :*  %dbug
                      [/~nul/desk/~1234.5.6/foo/bar [6 1] [6 2]]  [%sand %ud 6]
        ==    ==  ==
    !>  p.u.q.parsed
::
++  test-parse-scaffold-crane-fszp  ^-  tang
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
  %+  expect-eq
    !>  :*  source-rail=[[~nul %desk] /bar/foo]
            zuse-version=309
            structures=~
            libraries=~
            ^=  crane
              :~  :*  %fsts  %data
                      %fszp  %mark
              ==  ==
            ^=  sources
              :~  :*  %dbug
                      [/~nul/desk/~1234.5.6/foo/bar [2 1] [2 2]]  [%sand %ud 2]
        ==    ==  ==
    !>  p.u.q.parsed
::
++  test-parse-scaffold-crane-fszy  ^-  tang
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
  %+  expect-eq
    !>  :*  source-rail=[[~nul %desk] /bar/foo]
            zuse-version=309
            structures=~
            libraries=~
            ^=  crane
              :~  :*  %fsts  %data
                      %fszy  %mark
              ==  ==
            ^=  sources
              :~  :*  %dbug
                      [/~nul/desk/~1234.5.6/foo/bar [2 1] [2 2]]  [%sand %ud 9]
        ==    ==  ==
    !>  p.u.q.parsed
::
++  test-literal  ^-  tang
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::  send a pinned literal, expects a %made response with pinned literal
      ::
      ^=  call-args
        [duct=~ type=~ %build live=%.n [%$ %noun !>(**)]]
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.6
                %complete  %success  %$  %noun  !>(**)
        ==  ==
    ==
  ::
  %+  welp
    results1
  (expect-ford-empty ford-gate ~nul)
::
++  test-autocons-same  ^-  tang
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::  if we autocons the same schematic, we should get two of it as a result
      ::
      ^=  call-args
        :*  duct=~  type=~  %build  live=%.n
            [[%$ %noun !>(**)] [%$ %noun !>(**)]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.6  %complete
                %success
                [%success %$ %noun !>(**)]
                [%success %$ %noun !>(**)]
    ==  ==  ==
  ::
  %+  welp
    results1
  (expect-ford-empty ford-gate ~nul)
::
++  test-autocons-different  ^-  tang
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      :: if we autocons different schematics, we get different values
      ::
      ^=  call-args
        :*  duct=~  type=~  %build  live=%.n
            [[%$ %noun !>(42)] [%$ %noun !>(43)]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.6  %complete
                %success
                [%success %$ %noun !>(42)]
                [%success %$ %noun !>(43)]
    ==  ==  ==
  ::
  %+  welp
    results1
    (expect-ford-empty ford-gate ~nul)
::
++  test-scry-clay-succeed  ^-  tang
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-succeed ~1234.5.6 [%noun !>(42)])
      ::  test a pinned scry which succeeds
      ::
      ^=  call-args
        :*  duct=~  type=~  %build  live=%.n
            [%scry %c ren=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.6  %complete  %success
                [%scry %noun !>(42)]
    ==  ==  ==
  ::
  %+  welp
    results1
  (expect-ford-empty ford-gate ~nul)
::
++  test-scry-clay-fail  ^-  tang
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-fail ~1234.5.6)
      ::  attempting to scry a path which fails should produce an error
      ::
      ^=  call-args
        :*  duct=~  type=~  %build  live=%.n
            [%scry %c ren=%x rail=[[~nul %desk] /bar/foo]]
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
++  test-scry-clay-block  ^-  tang
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-block ~1234.5.6)
      ::  when we scry on a blocked path, expect a subscription move
      ::
      ^=  call-args
        :*  duct=~  type=~  %build  live=%.n
            [%scry %c ren=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %pass
                wire=/scry-request/cx/~nul/desk/~1234.5.6/foo/bar
                %c  %warp  ~nul  %desk
                ~  %sing  %x  [%da ~1234.5.6]  /foo/bar
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=scry-is-forbidden
      ::  when clay responds, send a %made
      ::
      ^=  take-args
        :*  wire=/scry-request/cx/~nul/desk/~1234.5.6/foo/bar  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %writ ~ [%x [%da ~1234.5.6] %desk] /bar/foo %noun !>(42)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~  %give  %made  ~1234.5.6  %complete  %success
                [%scry %noun !>(42)]
    ==  ==  ==
  ::
  ;:  welp
    results1
    results2
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-scry-clay-multiblock  ^-  tang
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-block ~1234.5.6)
      ::  when we scry on a blocked path, expect a subscription move
      ::
      ^=  call-args
        :*  duct=~[/one]  type=~  %build  live=%.n
            [%scry %c ren=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/one]  %pass
                wire=/scry-request/cx/~nul/desk/~1234.5.6/foo/bar
                %c  %warp  ~nul  %desk
                ~  %sing  %x  [%da ~1234.5.6]  /foo/bar
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=(scry-block ~1234.5.6)
      ::  when we scry on a blocked path, expect a subscription move
      ::
      ^=  call-args
        :*  duct=~[/two]  type=~  %build  live=%.n
            [%pin ~1234.5.6 [%scry %c ren=%x rail=[[~nul %desk] /bar/foo]]]
        ==
      ::
      ^=  moves  ~
    ==
  ::
  =^  results3  ford-gate
    %-  ford-take  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::  when clay responds, send a %made
      ::
      ^=  take-args
        :*  wire=/scry-request/cx/~nul/desk/~1234.5.6/foo/bar  duct=~
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %writ ~ [%x [%da ~1234.5.6] %desk] /bar/foo %noun !>(42)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/two]  %give  %made  ~1234.5.7  %complete  %success
                [%scry %noun !>(42)]
            ==
            :*  duct=~[/one]  %give  %made  ~1234.5.6  %complete  %success
                [%scry %noun !>(42)]
    ==  ==  ==
  ::
  ;:  welp
    results1
    results2
    results3
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-scry-clay-cancel  ^-  tang
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-block ~1234.5.6)
      ::  when we scry on a blocked path, expect a subscription move
      ::
      ^=  call-args
        :*  duct=~[/one]  type=~  %build  live=%.n
            [%scry %c ren=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/one]  %pass
                wire=/scry-request/cx/~nul/desk/~1234.5.6/foo/bar
                %c  %warp  ~nul  %desk
                ~  %sing  %x  [%da ~1234.5.6]  /foo/bar
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/one] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/one]  %pass
                wire=/scry-request/cx/~nul/desk/~1234.5.6/foo/bar
                %c  %warp  ~nul  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-scry-clay-live  ^-  tang
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-succeed ~1234.5.6 [%noun !>(42)])
      ::
      ^=  call-args
        :*  duct=~[/first]  type=~  %build  live=%.y
            [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/first]  %give  %made  ~1234.5.6  %complete  %success
                [%scry %noun !>(42)]
            ==
            :*  duct=~[/first]  %pass  wire=/clay-sub/~nul/desk/~1234.5.6
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=(scry-succeed ~1234.5.7 [%noun !>(43)])
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/desk/~1234.5.6  duct=~[/first]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/first]  %give  %made  ~1234.5.7  %complete  %success
                [%scry %noun !>(43)]
            ==
            :*  duct=~[/first]  %pass  wire=/clay-sub/~nul/desk/~1234.5.7
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results3  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/first] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/first]  %pass  wire=/clay-sub/~nul/desk/~1234.5.7
                %c  %warp  ~nul  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-scry-clay-live-again  ^-  tang
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-succeed ~1234.5.6 [%noun !>(42)])
      ::  perform a live scry, we should get a %made and a clay subscription
      ::
      ^=  call-args
        :*  duct=~[/first]  type=~  %build  live=%.y
            [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/first]  %give  %made  ~1234.5.6  %complete  %success
                [%scry %noun !>(42)]
            ==
            :*  duct=~[/first]  %pass  wire=/clay-sub/~nul/desk/~1234.5.6
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=(scry-succeed ~1234.5.7 [%noun !>(42)])
      ::
      ^=  call-args
        :*  duct=~[/second]  type=~  %build  live=%.y
            [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/second]  %give  %made  ~1234.5.7  %complete  %success
                [%scry %noun !>(42)]
            ==
            :*  duct=~[/second]  %pass  wire=/clay-sub/~nul/desk/~1234.5.7
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results3  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/first] type=~ %kill ~]
      ^=  moves
        :~  :*  duct=~[/first]  %pass  wire=/clay-sub/~nul/desk/~1234.5.6
                %c  %warp  ~nul  %desk  ~
    ==  ==  ==
  ::
  =^  results4  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.9
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/second] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/second]  %pass  wire=/clay-sub/~nul/desk/~1234.5.7
                %c  %warp  ~nul  %desk  ~
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
++  test-scry-clay-same-path  ^-  tang
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-blocks blocks)
      ::
      call-args=[duct=~[/first] type=~ %build live=%.y autocons]
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        %+  expect-eq
          !>  %-  sy
              :~  :*  duct=~[/first]  %pass
                      wire=/scry-request/cx/~nul/desk/~1234.5.7/foo/bar
                      %c  %warp  ~nul  %desk
                      `[%sing %x [%da ~1234.5.7] /foo/bar]
                  ==
                  :*  duct=~[/first]  %pass
                      wire=/scry-request/cx/~nul/desk/~1234.5.8/foo/bar
                      %c  %warp  ~nul  %desk
                      `[%sing %x [%da ~1234.5.8] /foo/bar]
              ==  ==
          !>  (sy moves)
    ==
  ::
  =^  results2  ford-gate
    %-  ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=(scry-blocks blocks)
      ::
      ^=  take-args
        :*  wire=/scry-request/cx/~nul/desk/~1234.5.7/foo/bar  duct=~[/first]
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
    %-  ford-take  :*
      ford-gate
      now=~1234.5.8
      scry=(scry-blocks blocks)
      ::
      ^=  take-args
        :*  wire=/scry-request/cx/~nul/desk/~1234.5.8/foo/bar  duct=~[/first]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            :*  %c  %writ  ~  [%x [%da ~1234.5.8] %desk]
                /bar/foo  %noun  scry-type  %eight
            ==
        ==
      ::
      ^=  expected-moves
        ^-  (list move:ford-gate)
        :~  :*  duct=~[/first]  %give  %made  ~1234.5.6  %complete  %success
                [%success %scry %noun scry-type %seven]
                [%success %scry %noun scry-type %eight]
    ==  ==  ==
  ::
  =^  results4  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.9
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/first] type=~ %kill ~]
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
++  test-pinned-in-past  ^-  tang
  ::
  =/  schematic  [%pin ~1234.5.5 [%$ %noun !>(42)]]
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/pin] type=~ %build live=%.n schematic]
      ::
      ^=  expected-moves
        :~  :*  duct=~[/pin]  %give  %made  ~1234.5.6  %complete
                %success  %$  %noun  !>(42)
    ==  ==  ==
  results1
::
++  test-pinned-in-future  ^-  tang
  ::
  =/  schematic  [%pin ~1234.5.7 [%$ %noun !>(42)]]
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/pin] type=~ %build live=%.n schematic]
      ::
      ^=  expected-moves
        :~  :*  duct=~[/pin]  %give  %made  ~1234.5.6  %complete
                %success  %$  %noun  !>(42)
    ==  ==  ==
  results1
::
++  test-pinned-in-pin  ^-  tang
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=(scry-succeed ~1234.5.6 [%noun !>(42)])
      ::
      ^=  call-args
        :*  duct=~[/pinned-in-pin]  type=~  %build  live=%.n
            %pin  ~1234.5.7
            %pin  ~1234.5.6
            [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/pinned-in-pin]
                %give  %made  ~1234.5.8  %complete
                [%success %scry %noun !>(42)]
    ==  ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-pinned-in-live  ^-  tang
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
    [%success [%scry %noun !>(42)]]
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-42
      ::
      call-args=[duct=~[/live] type=~ %build live=%.y schematic]
      moves=[duct=~[/live] %give %made ~1234.5.6 %complete result]~
    ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/live] type=~ %kill ~]
      moves=~
    ==
  ::
  ;:  weld
    results1
    results2
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-live-build-that-blocks  ^-  tang
  ::
  =/  scry-blocked  (scry-block ~1234.5.6)
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  =/  scry-43  (scry-succeed ~1234.5.8 [%noun !>(43)])
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-blocked
      ::
      ^=  call-args
        :*  duct=~[/live]  type=~  %build  live=%.y
            [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/live]  %pass
                wire=/scry-request/cx/~nul/desk/~1234.5.6/foo/bar
                %c  %warp  ~nul  %desk
                ~  %sing  %x  [%da ~1234.5.6]  /foo/bar
    ==  ==  ==
  ::
  =^  results2    ford-gate
    %-  ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=scry-42
      ::
      ^=  take-args
        :*  wire=/scry-request/cx/~nul/desk/~1234.5.6/foo/bar  duct=~[/live]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %writ ~ [%x [%da ~1234.5.6] %desk] /bar/foo %noun !>(42)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/live]  %give  %made  ~1234.5.6  %complete  %success
                [%scry %noun !>(42)]
            ==
            :*  duct=~[/live]  %pass  wire=/clay-sub/~nul/desk/~1234.5.6
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results3  ford-gate
    %-  ford-take  :*
      ford-gate
      now=~1234.5.8
      scry=scry-43
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/desk/~1234.5.6  duct=~[/live]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.8] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/live]  %give  %made  ~1234.5.8  %complete  %success
                [%scry %noun !>(43)]
            ==
            :*  duct=~[/live]  %pass  wire=/clay-sub/~nul/desk/~1234.5.8
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.8] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results4  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.9
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/live] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/live]  %pass  wire=/clay-sub/~nul/desk/~1234.5.8
                %c  %warp  ~nul  %desk  ~
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
++  test-live-and-once  ^-  tang
  ::
  =/  scry-blocked  (scry-block ~1234.5.6)
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-blocked
      ::
      ^=  call-args
        :*  duct=~[/live]  type=~  %build  live=%.y
            [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/live]  %pass
                wire=/scry-request/cx/~nul/desk/~1234.5.6/foo/bar
                %c  %warp  ~nul  %desk
                ~  %sing  %x  [%da ~1234.5.6]  /foo/bar
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry-blocked
      ::
      ^=  call-args
        :*  duct=~[/once]  type=~  %build  live=%.n
            [%pin ~1234.5.6 [%scry %c ren=%x rail=[[~nul %desk] /bar/foo]]]
        ==
      ::
      moves=~
    ==
  ::
  =^  results3  ford-gate
    %-  ford-take  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      ^=  take-args
        :*  wire=/scry-request/cx/~nul/desk/~1234.5.6/foo/bar  duct=~[/live]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %writ ~ [%x [%da ~1234.5.6] %desk] /bar/foo %noun !>(42)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/live]  %give  %made  ~1234.5.6  %complete
                [%success [%scry %noun !>(42)]]
            ==
            :*  duct=~[/live]  %pass  wire=/clay-sub/~nul/desk/~1234.5.6
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar]~)]
            ==
            :*  duct=~[/once]  %give  %made  ~1234.5.7  %complete
                [%success [%scry %noun !>(42)]]
    ==  ==  ==
  ::
  =^  results4  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.9
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/live] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/live]  %pass  wire=/clay-sub/~nul/desk/~1234.5.6
                %c  %warp  ~nul  %desk  ~
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
++  test-live-two-deep  ^-  tang
  ::
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  =/  scry-43  (scry-succeed ~1234.5.7 [%noun !>(43)])
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-42
      ::
      ^=  call-args
        :*  duct=~[/two-deep-4u]  type=~  %build  live=%.y
            [%same [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/two-deep-4u]  %give  %made  ~1234.5.6  %complete
                %success  [%scry %noun !>(42)]
            ==
            :*  duct=~[/two-deep-4u]  %pass
                wire=/clay-sub/~nul/desk/~1234.5.6
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=scry-43
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/desk/~1234.5.6  duct=~[/two-deep-4u]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/two-deep-4u]  %give  %made  ~1234.5.7  %complete
                %success  [%scry %noun !>(43)]
            ==
            :*  duct=~[/two-deep-4u]  %pass
                wire=/clay-sub/~nul/desk/~1234.5.7
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results3  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/two-deep-4u] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/two-deep-4u]  %pass
                wire=/clay-sub/~nul/desk/~1234.5.7
                %c  %warp  ~nul  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-live-three-deep  ^-  tang
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
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/ride] type=~ %build live=%.y same]
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %give  %made  ~1234.5.6  %complete
                [%success [%ride scry-type %constant]]
            ==
            :*  duct=~[/ride]  %pass  wire=/clay-sub/~nul/desk/~1234.5.6
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/desk/~1234.5.6  duct=~[/ride]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %pass  wire=/clay-sub/~nul/desk/~1234.5.7
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results3  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/ride] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %pass  wire=/clay-sub/~nul/desk/~1234.5.7
                %c  %warp  ~nul  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-live-triangle  ^-  tang
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
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/ride] type=~ %build live=%.y autocons]
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %give  %made  ~1234.5.6  %complete  %success
                [%success [%ride ride-type %constant]]
                [%success [%scry %noun !>(%it-does-in-fact-matter)]]
            ==
            :*  duct=~[/ride]  %pass  wire=/clay-sub/~nul/desk/~1234.5.6
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/desk/~1234.5.6  duct=~[/ride]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %give  %made  ~1234.5.7  %complete  %success
                [%success [%ride ride-type %constant]]
                [%success [%scry %noun !>(%changed)]]
            ==
            :*  duct=~[/ride]  %pass  wire=/clay-sub/~nul/desk/~1234.5.7
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results3  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/ride] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %pass  wire=/clay-sub/~nul/desk/~1234.5.7
                %c  %warp  ~nul  %desk  ~
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
++  test-live-and-pinned-triangle  ^-  tang
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
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/static] type=~ %build live=%.y static]
      ::
      ^=  moves
        :~  :*  duct=~[/static]  %give  %made  ~1234.5.6  %complete  %success
                [%success [%ride ride-type %constant]]
                [%success [%scry %noun scry-type %it-does-in-fact-matter]]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      call-args=[duct=~[/autocons] type=~ %build live=%.y autocons]
      ::
      ^=  moves
        :~  :*  duct=~[/autocons]  %give  %made  ~1234.5.7  %complete  %success
                [%success [%ride ride-type %constant]]
                [%success [%scry %noun scry-type %it-does-in-fact-matter]]
            ==
            :*  duct=~[/autocons]  %pass  wire=/clay-sub/~nul/home/~1234.5.7
                %c  %warp  ~nul  %home
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results3  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/static] type=~ %kill ~]
      ::
      moves=~
    ==
  ::
  =^  results4  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.9
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/autocons] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/autocons]  %pass  wire=/clay-sub/~nul/home/~1234.5.7
                %c  %warp  ~nul  %home  ~
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
++  test-call  ^-  tang
  ::
  =/  sample-schematic=schematic:ford-gate  [%$ %noun !>(5)]
  =/  gate-schematic=schematic:ford-gate  [%$ %noun !>(|=(a=@ud +(a)))]
  =/  call-schematic=schematic:ford-gate
    [%call gate-schematic sample-schematic]
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/call] type=~ %build live=%.y call-schematic]
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
        (expect-eq !>(6) result)
    ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/call] type=~ %kill ~]
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
++  test-call-scry-succeed  ^-  tang
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry-42
      ::
      call-args=[duct=~[/call] type=~ %build live=%.y call-schematic]
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
        (expect-eq !>(42) result)
    ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/call] type=~ %kill ~]
      ::
      moves=~
    ==
  ::
  ;:  weld
    results1
    results2
    (expect-ford-empty ford-gate ~nul)
  ==
++  test-call-scry-fail  ^-  tang
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry-failed
      ::
      call-args=[duct=~[/dead] type=~ %build live=%.y call-schematic]
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete %error *] i.moves)
        ::  compare the move to the expected move, omitting check on stack trace
        ::
        %+  expect-eq
          !>  :*  duct=~[/dead]  %give  %made  ~1234.5.6  %complete
                  [%error [leaf+"ford: %call execution failed:" ~]]
              ==
          !>  i.moves(|7 ~)
    ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/dead] type=~ %kill ~]
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
++  test-call-scry-block  ^-  tang
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
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-blocked
      ::
      call-args=[duct=~[/live] type=~ %build live=%.y call-schematic]
      ::
      ^=  moves
        :~  :*  duct=~[/live]  %pass
                wire=/scry-request/cx/~nul/desk/~1234.5.6/foo/bar
                %c  %warp  ~nul  %desk
                ~  %sing  %x  [%da ~1234.5.6]  /foo/bar
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  ford-take-with-comparator  :*
      ford-gate
      now=~1234.5.7
      scry=scry-blocked
      ::
      ^=  call-args
        :*  wire=/scry-request/cx/~nul/desk/~1234.5.6/foo/bar  duct=~
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
        =/  result  |7:i.moves
        ::
        (expect-eq !>(42) result)
    ==
  ::
  =^  results3  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/live] type=~ %kill ~]
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
++  test-call-scry-varies  ^-  tang
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      ^=  call-args
        [duct=~[/call] type=~ %build live=%.y call-schematic]
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  =(2 (lent moves))
        ?>  ?=([^ ^ ~] moves)
        ?>  ?=([* %give %made @da %complete %success %call *] i.moves)
        ::
        %+  weld
          =/  result  |7:i.moves
          (expect-eq !>(%first) result)
        ::  make sure the other move is a subscription
        ::
        %+  expect-eq
          !>  :*  duct=~[/call]  %pass  wire=/clay-sub/~nul/desk/~1234.5.6
                  %c  %warp  ~nul  %desk
                  `[%mult [%da ~1234.5.6] (sy [%x /timer] ~)]
              ==
          !>  i.t.moves
    ==
  ::
  =^  results2  ford-gate
    %-  ford-take-with-comparator  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      ^=  call-args
        :*  wire=/clay-sub/~nul/desk/~1234.5.6  duct=~[/call]
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
        ::
        %+  weld
          =/  result  |7:i.moves
          (expect-eq !>(%second) result)
        ::  make sure the other move is a subscription
        ::
        %+  expect-eq
          !>  :*  duct=~[/call]  %pass  wire=/clay-sub/~nul/desk/~1234.5.7
                  %c  %warp  ~nul  %desk
                  `[%mult [%da ~1234.5.7] (sy [%x /timer] ~)]
              ==
          !>  i.t.moves
    ==
  ::
  =^  results3  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/call] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/call]  %pass  wire=/clay-sub/~nul/desk/~1234.5.7
                %c  %warp  ~nul  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-dude  ^-  tang
  ::
  =/  schematic=schematic:ford-gate
    :*  %dude  >%test-no-error<
        [%scry [%c care=%x bel=[[~nul %desk] /bar/foo]]]
    ==
  ::
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-42
      ::
      call-args=[duct=~[/once] type=~ %build live=%.n schematic]
      ::
      ^=  moves
        :~  :*  duct=~[/once]  %give  %made  ~1234.5.6  %complete  %success
                [%scry %noun !>(42)]
    ==  ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-dude-error  ^-  tang
  ::
  =/  schematic=schematic:ford-gate
    :*  %dude  >%in-the-error-message<
        [%scry [%c care=%x bel=[[~nul %desk] /bar/foo]]]
    ==
  ::
  =/  scry-42  (scry-fail ~1234.5.6)
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-42
      ::
      call-args=[duct=~[/once] type=~ %build live=%.n schematic]
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
++  test-hood  ^-  tang
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
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/hood] type=~ %build live=%.y schematic]
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
            :*  duct=~[/hood]  %pass  /clay-sub/~nul/desk/~1234.5.6
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar/hoon] ~)]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/hood] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/hood]  %pass  wire=/clay-sub/~nul/desk/~1234.5.6
                %c  %warp  ~nul  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-slim  ^-  tang
  ::
  =/  formula=hoon  (ream '(add 2 2)')
  =/  subject-type=type  -:!>(.)
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      ^=  call-args
        [duct=~[/dead] type=~ %build live=%.y [%slim subject-type formula]]
      ::
      ^=  moves
        :~  :*  duct=~[/dead]  %give  %made  ~1234.5.6  %complete
                [%success [%slim (~(mint ut subject-type) [%noun formula])]]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/dead] type=~ %kill ~]
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
++  test-slit  ^-  tang
  ::
  =/  gate=vase    (ride %noun '|=(a=@ud ["one" a])')
  =/  sample=vase  !>(42)
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/slit] type=~ %build live=%.y [%slit gate sample]]
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
        %+  expect-eq
          !>  &
          !>  (~(nest ut actual-type) | expected-type)
    ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/slit] type=~ %kill ~]
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
++  test-slit-error  ^-  tang
  ::
  =/  gate=vase    (ride %noun '|=(a=@ud ["one" a])')
  =/  sample=vase  !>("a tape instead of @ud")
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/slit] type=~ %build live=%.y [%slit gate sample]]
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
        %+  expect-eq
          !>  :~  [%palm ["." "-" "" ""] [%leaf "have"] [%leaf "\"\""] ~]
                  :~  %palm  ["." "-" "" ""]
                      [%leaf "want"]
                      [%palm ["/" "" "" ""] [%leaf "a"] [%leaf "@ud"] ~]
                  ==
                  [%leaf "ford: %slit failed:"]
                  [%leaf "nest-fail"]
              ==
          !>  messages
    ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/slit] type=~ %kill ~]
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
++  test-ride  ^-  tang
  ::
  =/  fun  |=(a=@ (add 2 a))
  =/  formula=hoon  (ream '!:  (fun 3)')
  =/  subject-schematic=schematic:ford-gate  [%$ %noun !>(.)]
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      ^=  call-args
        :*  duct=~[/dead]  type=~  %build  live=%.y
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
        =/  result  |7:i.moves
        (expect-eq !>(5) result)
    ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/dead] type=~ %kill ~]
      moves=~
    ==
  ::
  ;:  weld
    results1
    results2
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-ride-scry-succeed  ^-  tang
  ::
  =/  scry-42  (scry-succeed ~1234.5.6 [%noun !>(42)])
  ::
  =/  formula=hoon  (ream '!:  .^(* %cx /~nul/desk/~1234.5.6/foo/bar)')
  =/  subject-schematic=schematic:ford-gate  [%$ %noun !>(.)]
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry-42
      ::
      ^=  call-args
        :*  duct=~[/dead]  type=~  %build  live=%.y
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
        =/  result  |7:i.moves
        (expect-eq !>(42) result)
    ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/dead] type=~ %kill ~]
      moves=~
    ==
  ::
  ;:  weld
    results1
    results2
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-ride-scry-fail  ^-  tang
  ::
  =/  scry-failed  (scry-fail ~1234.5.6)
  ::
  =/  formula=hoon  (ream '!:  .^(* %cx /~nul/desk/~1234.5.6/foo/bar)')
  =/  subject-schematic=schematic:ford-gate  [%$ %noun !>(.)]
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry-failed
      ::
      ^=  call-args
        :*  duct=~[/dead]  type=~  %build  live=%.y
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
        ::  compare the move to the expected move, omitting check on stack trace
        ::
        %+  expect-eq
          !>  :*  duct=~[/dead]  %give  %made  ~1234.5.6  %complete
                  [%error [leaf+"ford: %ride failed to execute:" ~]]
              ==
          !>  i.moves(|7 ~)
    ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/dead] type=~ %kill ~]
      moves=~
    ==
  ::
  ;:  weld
    results1
    results2
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-ride-scry-block  ^-  tang
  ::
  =/  scry-blocked  (scry-block ~1234.5.6)
  ::
  =/  formula=hoon  (ream '!:  .^(* %cx /~nul/desk/~1234.5.6/foo/bar)')
  =/  subject-schematic=schematic:ford-gate  [%$ %noun !>(.)]
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-blocked
      ::
      ^=  call-args
        :*  duct=~[/live]  type=~  %build  live=%.y
            [%ride formula subject-schematic]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/live]  %pass
                wire=/scry-request/cx/~nul/desk/~1234.5.6/foo/bar
                %c  %warp  ~nul  %desk
                ~  %sing  %x  [%da ~1234.5.6]  /foo/bar
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  ford-take-with-comparator  :*
      ford-gate
      now=~1234.5.7
      ::  TODO: using +scry-is-forbidden causes a bail: 4
      ::
      scry=scry-blocked
      ::
      ^=  take-args
        :*  wire=/scry-request/cx/~nul/desk/~1234.5.6/foo/bar  duct=~[/live]
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
        =/  result  |7:i.moves
        (expect-eq !>(42) result)
    ==
  ::
  =^  results3  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/live] type=~ %kill ~]
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
++  test-ride-scry-promote  ^-  tang
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
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/ride] type=~ %build live=%.y ride]
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %give  %made  ~1234.5.6  %complete
                [%success [%ride scry-type %constant]]
            ==
            :*  duct=~[/ride]  %pass  wire=/clay-sub/~nul/desk/~1234.5.6
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/desk/~1234.5.6  duct=~[/ride]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %pass  wire=/clay-sub/~nul/desk/~1234.5.7
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results3  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/ride] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %pass  wire=/clay-sub/~nul/desk/~1234.5.7
                %c  %warp  ~nul  %desk  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-five-oh-fora  ^-  tang
  ::
  =/  scry-type=type
    [%cell [%face %title [%atom %t ~]] [%face %contents -:!>("")]]
  ::
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %desk %da ~1234.5.6] /a/posts]]
      [%noun scry-type [title='post-a' contents="post-a-contents"]]
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.6] /b/posts]]
      [%noun scry-type [title='post-b' contents="post-b-contents"]]
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.7] /a/posts]]
      [%noun scry-type [title='post-a' contents="post-a-contents"]]
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.7] /b/posts]]
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/post-a] type=~ %build live=%.y rendered-a]
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  ?=([^ ^ ~] moves)
        %+  welp
          %-  verify-post-made  :*
            move=i.moves
            duct=~[/post-a]
            type=scry-type
            date=~1234.5.6
            title='post-a'
            contents="post-a-contents"
          ==
        %+  expect-eq
          !>  :*  duct=~[/post-a]  %pass
                  wire=/clay-sub/~nul/desk/~1234.5.6  %c  %warp
                  ~nul  %desk
                  `[%mult [%da ~1234.5.6] (sy [%x /posts/a] [%x /posts/b] ~)]
              ==
          !>  i.t.moves
    ==
  ::
  =^  results2  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      call-args=[duct=~[/post-b] type=~ %build live=%.y rendered-b]
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  ?=([^ ^ ~] moves)
        %+  welp
          %-  verify-post-made  :*
            move=i.moves
            duct=~[/post-b]
            type=scry-type
            date=~1234.5.7
            title='post-b'
            contents="post-b-contents"
          ==
        %+  expect-eq
          !>  :*  duct=~[/post-b]  %pass
                  wire=/clay-sub/~nul/desk/~1234.5.7  %c  %warp
                  ~nul  %desk
                  `[%mult [%da ~1234.5.7] (sy [%x /posts/a] [%x /posts/b] ~)]
              ==
          !>  i.t.moves
    ==
  ::
  =^  results3  ford-gate
    %-  ford-take-with-comparator  :*
      ford-gate
      now=~1234.5.8
      scry=scry
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/desk/~1234.5.6  duct=~[/post-a]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.8] (sy [%x /posts/a]~)]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  ?=([^ ^ ~] moves)
        %+  welp
          %-  verify-post-made  :*
            move=i.moves
            duct=~[/post-a]
            type=scry-type
            date=~1234.5.8
            title='post-a'
            contents="post-a-contents-changed"
          ==
        %+  expect-eq
          !>  :*  duct=~[/post-a]  %pass
                  wire=/clay-sub/~nul/desk/~1234.5.8
                  %c  %warp  ~nul  %desk
                  `[%mult [%da ~1234.5.8] (sy [%x /posts/a] [%x /posts/b] ~)]
              ==
          !>  i.t.moves
    ==
  ::
  =^  results4  ford-gate
    %-  ford-take  :*
      ford-gate
      now=~1234.5.9
      scry=scry
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/desk/~1234.5.7  duct=~[/post-b]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.8] (sy [%x /posts/a]~)]
        ==
      ::
      moves=~
    ==
  ::
  =^  results5  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.10
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/post-a] type=~ %kill ~]
      ::
      moves=~
    ==
  ::
  =^  results6  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.11
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/post-b] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/post-a]  %pass  wire=/clay-sub/~nul/desk/~1234.5.8
                %c  %warp  ~nul  %desk  ~
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
++  test-alts  ^-  tang
  ::
  =/  scry-type=type  [%atom %tas ~]
  ::
  =/  scry-results=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /one/scry]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /two/scry]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.7] /one/scry]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.7] /two/scry]]
      `[%noun scry-type 'scry-two']
    ::
      :-  [%cx [[~nul %home %da ~1234.5.8] /one/scry]]
      `[%noun scry-type 'scry-one']
    ==
  ::
  =/  scry  (scry-with-results-and-failures scry-results)
  ::
  =/  scry1=schematic:ford-gate  [%scry [%c %x [~nul %home] /one/scry]]
  =/  scry2=schematic:ford-gate  [%scry [%c %x [~nul %home] /two/scry]]
  =/  alts=schematic:ford-gate   [%alts [scry1 scry2 ~]]
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/alts] type=~ %build live=%.y alts]
      ::
      ^=  moves
        :~  :*  duct=~[/alts]  %give  %made  ~1234.5.6  %complete  %error
                :~  [%leaf "%alts: all options failed"]
                    [%leaf "option"]
                    :+  %rose  ["  " "\{" "}"]  :~
                        [%leaf "scry failed for"]
                        [%leaf "%cx /~nul/home/~1234.5.6/scry/one"]
                    ==
                    [%leaf "option"]
                    :+  %rose  ["  " "\{" "}"]  :~
                        [%leaf "scry failed for"]
                        [%leaf "%cx /~nul/home/~1234.5.6/scry/two"]
                    ==
            ==  ==
            :*  duct=~[/alts]  %pass  wire=/clay-sub/~nul/home/~1234.5.6
                %c  %warp  ~nul  %home
                `[%mult [%da ~1234.5.6] (sy [%x /scry/two] [%x /scry/one] ~)]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/home/~1234.5.6  duct=~[/alts]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /scry/two]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/alts]  %give  %made  ~1234.5.7  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-two'
            ==
            :*  duct=~[/alts]  %pass  wire=/clay-sub/~nul/home/~1234.5.7
                %c  %warp  ~nul  %home
                `[%mult [%da ~1234.5.7] (sy [%x /scry/two] [%x /scry/one] ~)]
    ==  ==  ==
  ::
  =^  results3  ford-gate
    %-  ford-take  :*
      ford-gate
      now=~1234.5.8
      scry=scry
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/home/~1234.5.6  duct=~[/alts]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.8] (sy [%x /scry/one]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/alts]  %give  %made  ~1234.5.8  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-one'
            ==
            :*  duct=~[/alts]  %pass  wire=/clay-sub/~nul/home/~1234.5.8
                %c  %warp  ~nul  %home
                `[%mult [%da ~1234.5.8] (sy [%x /scry/one] ~)]
    ==  ==  ==
  ::
  =^  results4  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.9
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/alts] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/alts]  %pass  wire=/clay-sub/~nul/home/~1234.5.8
                %c  %warp  ~nul  %home  ~
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
++  test-alts-and-live  ^-  tang
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
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/same] type=~ %build live=%.y same]
      ::
      ^=  moves
        :~  :*  duct=~[/same]  %give  %made  ~1234.5.6  %complete
                %success  %scry  %noun  scry-type  'scry-two'
            ==
            :*  duct=~[/same]  %pass  wire=/clay-sub/~nul/desk/~1234.5.6
                %c  %warp  ~nul  %desk
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
    %-  ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      call-args=[duct=~[/alts] type=~ %build live=%.y alts]
      ::
      ^=  moves
        :~  :*  duct=~[/alts]  %give  %made  ~1234.5.7  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-two'
            ==
            :*  duct=~[/alts]  %pass  wire=/clay-sub/~nul/desk/~1234.5.7
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /scry/two] [%x /scry/one] ~)]
    ==  ==  ==
  ::
  ::  tell ford that /scry/one exists now
  ::
  =^  results3  ford-gate
    %-  ford-take  :*
      ford-gate
      now=~1234.5.8
      scry=scry
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/desk/~1234.5.7  duct=~[/alts]
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
            :*  duct=~[/alts]  %pass  wire=/clay-sub/~nul/desk/~1234.5.8
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.8] (sy [%x /scry/one] ~)]
    ==  ==  ==
  ::
  ::  kill the /same build
  ::
  ::    We should no longer subscribe to /scry/two in the resulting clay
  ::    subscription.
  ::
  =^  results4  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.9
      scry=scry
      ::
      call-args=[duct=~[/same] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/same]  %pass  wire=/clay-sub/~nul/desk/~1234.5.6
                %c  %warp  ~nul  %desk  ~
    ==  ==  ==
  ::
  =^  results5  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.10
      scry=scry
      ::
      call-args=[duct=~[/alts] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/alts]  %pass  wire=/clay-sub/~nul/desk/~1234.5.8
                %c  %warp  ~nul  %desk  ~
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
++  test-double-alts  ^-  tang
  ::
  =/  scry-type=type  [%atom %tas ~]
  ::
  =/  scry-results=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %desk %da ~1234.5.6] /one/scry]]
      ~
      :-  [%cx [[~nul %desk %da ~1234.5.6] /two/scry]]
      `[%noun scry-type 'scry-two']
      :-  [%cx [[~nul %desk %da ~1234.5.6] /three/scry]]
      ~
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.7] /one/scry]]
      ~
      :-  [%cx [[~nul %desk %da ~1234.5.7] /two/scry]]
      `[%noun scry-type 'scry-two']
      :-  [%cx [[~nul %desk %da ~1234.5.7] /three/scry]]
      ~
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.8] /one/scry]]
      ~
      :-  [%cx [[~nul %desk %da ~1234.5.8] /two/scry]]
      `[%noun scry-type 'scry-two']
      :-  [%cx [[~nul %desk %da ~1234.5.8] /three/scry]]
      `[%noun scry-type 'scry-three']
    ::
      :-  [%cx [[~nul %desk %da ~1234.5.9] /one/scry]]
      `[%noun scry-type 'scry-one']
      :-  [%cx [[~nul %desk %da ~1234.5.9] /two/scry]]
      `[%noun scry-type 'scry-two-changed']
      :-  [%cx [[~nul %desk %da ~1234.5.9] /three/scry]]
      `[%noun scry-type 'scry-three']
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
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/first] type=~ %build live=%.y alts1]
      ::
      ^=  moves
        :~  :*  duct=~[/first]  %give  %made  ~1234.5.6  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-two'
            ==
            :*  duct=~[/first]  %pass  wire=/clay-sub/~nul/desk/~1234.5.6
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /scry/one] [%x /scry/two] ~)]
    ==  ==  ==
  ::  alts2 will depend on both scry3 and scry2
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      call-args=[duct=~[/second] type=~ %build live=%.y alts2]
      ::
      ^=  moves
        :~  :*  duct=~[/second]  %give  %made  ~1234.5.7  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-two'
            ==
            :*  duct=~[/second]  %pass  wire=/clay-sub/~nul/desk/~1234.5.7
                %c  %warp  ~nul  %desk  ~  %mult  [%da ~1234.5.7]
                (sy [%x /scry/two] [%x /scry/three] ~)
    ==  ==  ==
  ::
  ::  alts2 should now just return 'scry-three'
  ::
  =^  results3  ford-gate
    %-  ford-take  :*
      ford-gate
      now=~1234.5.8
      scry=scry
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/desk/~1234.5.7  duct=~[/second]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.8] (sy [%x /scry/three]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/second]  %give  %made  ~1234.5.8  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-three'
            ==
            :*  duct=~[/second]  %pass  wire=/clay-sub/~nul/desk/~1234.5.8
                %c  %warp  ~nul  %desk  ~  %mult  [%da ~1234.5.8]
                (sy [%x /scry/three] ~)
    ==  ==  ==
  ::
  =^  results4  ford-gate
    %-  ford-take  :*
      ford-gate
      now=~1234.5.9
      scry=scry
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/desk/~1234.5.6  duct=~[/first]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.9] (sy [%x /scry/one] [%x /scry/two] ~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/first]  %give  %made  ~1234.5.9  %complete
                %success  %alts  %success  %scry  %noun  scry-type  'scry-one'
            ==
            :*  duct=~[/first]  %pass  wire=/clay-sub/~nul/desk/~1234.5.9
                %c  %warp  ~nul  %desk  ~  %mult  [%da ~1234.5.9]
                (sy [%x /scry/one] ~)
    ==  ==  ==
  ::
  =^  results5  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.10
      scry=scry
      ::
      call-args=[duct=~[/first] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/first]  %pass  wire=/clay-sub/~nul/desk/~1234.5.9
                %c  %warp  ~nul  %desk  ~
    ==  ==  ==
  ::
  =^  results6  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.11
      scry=scry
      ::
      call-args=[duct=~[/second] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/second]  %pass  wire=/clay-sub/~nul/desk/~1234.5.8
                %c  %warp  ~nul  %desk  ~
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
++  disabled-test-cache-reclamation-trivial
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::  send a pinned literal, expects a %made response with pinned literal
      ::
      ^=  call-args
        [duct=~[/trivial] type=~ %build live=%.n [%$ %noun !>(**)]]
      ::
      ^=  moves
        :~  :*  duct=~[/trivial]  %give  %made  ~1234.5.6
                %complete  %success  %$  %noun  !>(**)
        ==  ==
    ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry-is-forbidden
      ::  ask ford to wipe its cache
      ::
      call-args=[duct=~[/trivial] type=~ %wipe 10]
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
++  disabled-test-cache-reclamation-live-rebuild
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-succeed ~1234.5.6 [%noun !>(42)])
      ::
      ^=  call-args
        :*  duct=~[/build]  type=~  %build  live=%.y
            [%scry %c care=%x rail=[[~nul %desk] /bar/foo]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/build]  %give  %made  ~1234.5.6  %complete  %success
                [%scry %noun !>(42)]
            ==
            :*  duct=~[/build]  %pass  wire=/clay-sub/~nul/desk
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry-is-forbidden
      ::  ask ford to wipe its cache
      ::
      call-args=[duct=~[/build] type=~ %wipe 10]
      ::  cache wiping should never produce any moves
      ::
      moves=~
    ==
  ::
  =^  results3  ford-gate
    %-  ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=(scry-succeed ~1234.5.7 [%noun !>(43)])
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/desk  duct=~[/build]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/build]  %give  %made  ~1234.5.7  %complete  %success
                [%scry %noun !>(43)]
            ==
            :*  duct=~[/build]  %pass  wire=/clay-sub/~nul/desk
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar]~)]
    ==  ==  ==
  ::
  =^  results4  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/build] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/build]  %pass  wire=/clay-sub/~nul/desk
                %c  %warp  ~nul  %desk  ~
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
++  disabled-test-cache-reclamation-live-promote
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
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/ride] type=~ %build live=%.y same]
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %give  %made  ~1234.5.6  %complete
                [%success [%ride scry-type %constant]]
            ==
            :*  duct=~[/ride]  %pass  wire=/clay-sub/~nul/desk
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.6] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.7
      scry=scry-is-forbidden
      ::  ask ford to wipe its cache
      ::
      call-args=[duct=~[/ride] type=~ %wipe 10]
      ::  cache wiping should never produce any moves
      ::
      moves=~
    ==
  ::
  =^  results3  ford-gate
    %-  ford-take  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/desk  duct=~[/ride]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%x /foo/bar]~)]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %pass  wire=/clay-sub/~nul/desk
                %c  %warp  ~nul  %desk
                `[%mult [%da ~1234.5.7] (sy [%x /foo/bar] ~)]
    ==  ==  ==
  ::
  =^  results4  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/ride] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/ride]  %pass  wire=/clay-sub/~nul/desk
                %c  %warp  ~nul  %desk  ~
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
++  disabled-test-five-oh-cache-reclamation  ^-  tang
  ::
  =/  scry-type=type
    [%cell [%face %title [%atom %t ~]] [%face %contents -:!>("")]]
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry
      ::
      call-args=[duct=~[/post-a] type=~ %build live=%.y rendered-a]
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  ?=([^ ^ ~] moves)
        %+  welp
          %-  verify-post-made  :*
            move=i.moves
            duct=~[/post-a]
            type=scry-type
            date=~1234.5.6
            title='post-a'
            contents="post-a-contents"
          ==
        %+  expect-eq
          !>  :*  duct=~[/post-a]  %pass  wire=/clay-sub/~nul/desk
                  %c  %warp  ~nul  %desk
                  `[%mult [%da ~1234.5.6] (sy [%x /posts/a] [%x /posts/b] ~)]
              ==
          !>  i.t.moves
    ==
  ::
  =^  results2  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.7
      scry=scry
      ::
      call-args=[duct=~[/post-b] type=~ %build live=%.y rendered-b]
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  ?=([^ ~] moves)
        %-  verify-post-made  :*
          move=i.moves
          duct=~[/post-b]
          type=scry-type
          date=~1234.5.7
          title='post-b'
          contents="post-b-contents"
    ==  ==
  ::
  =^  results3  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/wipe] type=~ %wipe 10]
      moves=~
    ==
  ::
  =^  results4  ford-gate
    %-  ford-take-with-comparator  :*
      ford-gate
      now=~1234.5.9
      scry=scry
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/desk  duct=~[/post-a]
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
          %-  verify-post-made  :*
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
            %-  verify-post-made  :*
              move=i.moves
              duct=~[/post-a]
              type=scry-type
              date=~1234.5.9
              title='post-a'
              contents="post-a-contents-changed"
            ==
          %-  verify-post-made  :*
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
          %-  verify-post-made  :*
            move=i.moves
            duct=~[/post-b]
            type=scry-type
            date=~1234.5.9
            title='post-b'
            contents="post-b"
          ==
        ::
        %-  verify-post-made  :*
          move=i.t.moves
          duct=~[/post-a]
          type=scry-type
          date=~1234.5.9
          title='post-a'
          contents="post-a-contents-changed"
    ==  ==
  ::
  =^  results5  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.10
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/post-b] type=~ %kill ~]
      moves=~
    ==
  ::
  =^  results6  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.11
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/post-a] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/post-a]  %pass  wire=/clay-sub/~nul/desk
                %c  %warp  ~nul  %desk  ~
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
++  disabled-test-reef-slow
  ::
  =/  hoon-parsed=hoon
    (rain /~nul/base/~1234.5.6/sys/hoon/hoon hoon-scry)
  ~&  %parsed-hoon
  ::
  =/  arvo-parsed=hoon
    (rain /~nul/base/~1234.5.6/sys/arvo/hoon arvo-scry)
  ~&  %parsed-arvo
  ::
  =/  zuse-parsed=hoon
    (rain /~nul/base/~1234.5.6/sys/zuse/hoon zuse-scry)
  ~&  %parsed-zuse
  ::
  =/  pit=vase  !>(~)
  =/  hoon-compiled=vase  (slap pit hoon-parsed)
  ~&  %hoon-compiled
  =/  arvo-compiled=vase  (slap (slot 7 hoon-compiled) arvo-parsed)
  ~&  %arvo-compiled
  =/  pit-compiled=vase   (slap arvo-compiled [%cnts ~[[%& 1] %is] ~])
  ~&  %pit-compiled
  =/  zuse-compiled=vase  (slap pit-compiled zuse-parsed)
  ~&  %zuse-compiled
  ::
  =/  scry-results=(map [term beam] cage)  (with-reef ~1234.5.6 ~)
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/reef]  type=~  %build  live=%.n
            [%reef [~nul %base]]
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
        ?>  ?=([%success %reef *] +.result)
        ::
        =/  kernel=vase  vase.build-result.result
        ::
        %+  weld
          =/  result
            q:(slym (slap (slap kernel [%limb %format]) [%limb %en-beam]) *beam)
          %+  expect-eq
            !>  (en-beam *beam)
            !>  result
        ::
        %+  expect-eq
          !>  &
          !>  (slab %format p.kernel)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-reef-short-circuit  ^-  tang
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-reef ~1234.5.6)
      ::
      ^=  call-args
        :*  duct=~[/reef]  type=~  %build  live=%.n
            [%reef [~nul %home]]
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
        ?>  ?=([%success %reef *] +.result)
        ::
        =/  kernel=vase  vase.build-result.result
        ::
        %+  weld
          =/  result
            q:(slym (slap (slap kernel [%limb %format]) [%limb %en-beam]) *beam)
          %+  expect-eq
            !>  (en-beam *beam)
            !>  result
        ::
        %+  expect-eq
          !>  &
          !>  (slab %format p.kernel)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-path  ^-  tang
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
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%path disc=[~nul %desk] prefix='lib' raw-path='foo-bar']
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/path]  %give  %made  ~1234.5.6  %complete
                %success  %path  [[~nul %desk] /hoon/bar/foo/lib]
    ==  ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-plan-hoon  ^-  tang
  ::
  =/  =hoon  (ream '`@tas`%constant')
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      ^=  call-args
        :*  duct=~[/plan]  type=~  %build  live=%.n
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
        ?>  ?=([%success %plan *] +.result)
        ::
        =/  =vase  vase.build-result.result
        ::
        (expect-eq !>(%constant) vase)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core  ^-  tang
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
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/foo-bar/lib]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/path]  %give  %made  ~1234.5.6  %complete
                %success  %core  [%atom %tas ~]  %constant
    ==  ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-linker  ^-  tang
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/program/gen]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %core *] build-result.result)
        ::
        =/  =vase  vase.build-result.result
        ::
        (expect-eq !>(["onetwo" 3]) vase)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-multi-hoon  ^-  tang
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/program/gen]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %core *] build-result.result)
        ::
        =/  =vase  vase.build-result.result
        ::
        (expect-eq !>(["one" 1]) vase)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fsts-fssg  ^-  tang
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/program/gen]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %core *] build-result.result)
        ::
        =/  =vase  vase.build-result.result
        ::
        (expect-eq !>(3) vase)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fsdt-fskt  ^-  tang
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/program/gen]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %core *] build-result.result)
        ::
        =/  =vase  vase.build-result.result
        ::
        (expect-eq !>([1 2 3 4 5 ~]) vase)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fskt-nest-fail  ^-  tang
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
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/program/gen]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/path]  %give  %made  ~1234.5.6  %complete  %error
                :~  :-  %leaf
                    "ford: %core on /~nul/home/0/gen/program/hoon failed:"
                    [%leaf "/^ failed: nest-fail"]
    ==  ==  ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fssm  ^-  tang
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/program/gen]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %core *] build-result.result)
        ::
        =/  =vase  vase.build-result.result
        ::
        (expect-eq !>([5 5 ~]) vase)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fsbr  ^-  tang
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/program/gen]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %core *] build-result.result)
        ::
        =/  =vase  vase.build-result.result
        ::
        (expect-eq !>([6 6 ~]) vase)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fsbr-out-of-options  ^-  tang
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
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/program/gen]]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/path]  %give  %made  ~1234.5.6  %complete  %error
                :~  :-  %leaf
                    "ford: %core on /~nul/home/0/gen/program/hoon failed:"
                    [%leaf "/| failed: out of options"]
    ==  ==  ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-plan-fszp-as-noun  ^-  tang
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
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
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %plan *] build-result.result)
        ::
        =/  =vase  vase.build-result.result
        ::
        (expect-eq !>([1 2 3 ~]) vase)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fszp-as-mark  ^-  tang
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            %core  [[~nul %home] /hoon/program/gen]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %core *] build-result.result)
        ::
        =/  =vase  vase.build-result.result
        ::
        (expect-eq !>(["five" 5]) vase)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fscl-fszp  ^-  tang
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/program/gen]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %core *] build-result.result)
        ::
        =/  =vase  vase.build-result.result
        ::
        (expect-eq !>([1 2 3 ~]) vase)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fscm  ^-  tang
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/program/gen]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %core *] build-result.result)
        ::
        =/  =vase  vase.build-result.result
        ::
        (expect-eq !>([1 2 3 ~]) vase)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-plan-fsbc  ^-  tang
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
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
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %plan *] build-result.result)
        ::
        =/  =vase  vase.build-result.result
        ::
        %+  expect-eq
          !>([[[~nul %home [%da ~1234.5.6]] /other/lib] %value])
          vase
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fscb  ^-  tang
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  arch-type=type  -:!>(*arch)
  =/  scry-results=(map [term beam] cage)
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/program/gen]]
      :-  %hoon
      :-  hoon-src-type
      '''
      /=  data  /^  (map @ta @ud)
                /:  /===/data
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/program/gen]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %core *] build-result.result)
        ::
        =/  =vase  vase.build-result.result
        ::
        =/  expected  (my [[%one 1] [%two 2] ~])
        (expect-eq !>(expected) vase)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fspm  ^-  tang
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  arch-type=type  -:!>(*arch)
  =/  scry-results=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/program/gen]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /=  data  /&second&first&/~["four" 5 "six"]
      data
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/first/mar]]
      :-  ~
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
      :-  ~
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
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/noun/mar]]
      ~
    ==
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            %core  [[~nul %home] /hoon/program/gen]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %core *] build-result.result)
        ::
        =/  =vase  vase.build-result.result
        (expect-eq !>(5) vase)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-core-fszy-renderer  ^-  tang
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            %core  [[~nul %home] /hoon/program/gen]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %core *] build-result.result)
        ::
        =/  =vase  vase.build-result.result
        ::
        (expect-eq !>([1 2 3 ~]) vase)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-bunt  ^-  tang
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%bunt [~nul %home] %foo]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %bunt *] build-result.result)
        ::
        =/  =vase  q.cage.build-result.result
        ::
        (expect-eq !>([0 0]) vase)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-volt  ^-  tang
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%volt [~nul %home] %foo [12 13]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %volt *] build-result.result)
        ::
        =/  =vase  q.cage.build-result.result
        ::
        (expect-eq !>([12 13]) vase)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-vale  ^-  tang
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%vale [~nul %home] %foo [12 13]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %vale *] build-result.result)
        ::
        =/  =vase  q.cage.build-result.result
        ::
        (expect-eq !>([12 13]) vase)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-vale-error  ^-  tang
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
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%vale [~nul %home] %foo 42]
        ==
      ::
      ^=  moves
        :~  :*  duct=~[/path]  %give  %made  ~1234.5.6  %complete  %error
                :~  :-  %leaf
                    %+  weld
                      "ford: %vale failed: invalid input for mark: "
                    "/~nul/home/~1234.5.6/mar/foo/hoon"
                ::
                    :-  %leaf
                    "ford: %call execution failed:"
    ==  ==  ==  ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-cast  ^-  tang
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
  =/  scry-results=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/foo/mar]]
      `[%hoon hoon-src-type foo-mark-src]
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/bar/mar]]
      `[%hoon hoon-src-type bar-mark-src]
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/noun/mar]]
      ~
    ==
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%cast [~nul %home] %foo [%vale [~nul %home] %bar [12 13]]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %cast *] build-result.result)
        ::
        =/  =cage      cage.build-result.result
        ::
        (expect-cage %foo !>([12 13]) cage)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-cast-grow  ^-  tang
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
  =/  scry-results=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/foo/mar]]
      `[%hoon hoon-src-type foo-mark-src]
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/bar/mar]]
      `[%hoon hoon-src-type bar-mark-src]
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/noun/mar]]
      ~
    ==
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%cast [~nul %home] %foo [%vale [~nul %home] %bar [12 13]]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %cast *] build-result.result)
        ::
        =/  =cage      cage.build-result.result
        ::
        (expect-cage %foo !>([12 13]) cage)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-mute  ^-  tang
  ::
  =/  atom-type=type  [%atom %$ ~]
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
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
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %mute *] build-result.result)
        ::
        =/  =cage      cage.build-result.result
        ::
        (expect-cage %foo !>([2 43 4]) cage)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-bake-renderer  ^-  tang
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  scry-results=(map [term beam] cage)
    %+  with-reef  ~1234.5.6
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%bake %foo *coin `rail:ford-gate`[[~nul %home] /data]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %bake *] build-result.result)
        ::
        =/  =cage  cage.build-result.result
        ::
        (expect-cage %foo !>([1 2 3 ~]) cage)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-bake-mark  ^-  tang
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  arch-type=type  -:!>(*arch)
  ::
  =/  scry-results=(map [term beam] (unit cage))
    %+  with-reef-unit  ~1234.5.6
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
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/noun/mar]]
      ~
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /data]]
      `[%arch !>(`arch`[fil=~ dir=(my [%bar ~]~)])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /bar/data]]
      `[%arch !>(`arch`[fil=`*@uv dir=~])]
    ==
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%bake %foo *coin `rail:ford-gate`[[~nul %home] /data]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ~|  build-result.result
        ?>  ?=([%success %bake *] build-result.result)
        ::
        =/  =cage  cage.build-result.result
        ::
        (expect-cage %foo !>([12 13]) cage)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::  renderers can fail, and we should fall back to using the mark
::
++  test-bake-mark-fallback  ^-  tang
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  scry-results=(map [term beam] (unit cage))
    %+  with-reef-unit  ~1234.5.6
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/dat/ren]]
      :^  ~  %hoon  hoon-src-type
      '''
      /=  data  /~  !!
      data
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/dat/mar]]
      :^  ~  %hoon  hoon-src-type
      '''
      |_  atom=@
      ++  grab
        |%
        ++  txt  @
        --
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/txt/mar]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /txt/data]]
      :^  ~  %txt  hoon-src-type
      '''
      one
      '''
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /data]]
      `[%arch !>(`arch`[fil=~ dir=(my [%txt ~]~)])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /txt/data]]
      `[%arch !>(`arch`[fil=`*@uv dir=~])]
    ==
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            [%bake %dat *coin `rail:ford-gate`[[~nul %home] /data]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=(^ moves)
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %bake *] build-result.result)
        ::
        =/  =cage  cage.build-result.result
        ::
        (expect-cage %dat !>('one') cage)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-diff  ^-  tang
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
    ==
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
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
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %diff *] build-result.result)
        ::
        =/  =cage  cage.build-result.result
        ::
        (expect-cage %foo !>([17 18]) cage)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-diff-form  ^-  tang
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
    ==
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
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
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %diff *] build-result.result)
        ::
        =/  =cage  cage.build-result.result
        ::
        (expect-cage %txt-diff !>(~[[%& 1] [%| ~[%b] ~[%d]]]) cage)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-pact  ^-  tang
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
    ==
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
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
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %pact *] build-result.result)
        ::
        =/  =cage  cage.build-result.result
        ::
        (expect-cage %txt !>(~[%a %d]) cage)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-pact-mark  ^-  tang
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
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/noun/mar]]
      ~
    ==
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
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
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %pact *] build-result.result)
        ::
        =/  =cage  cage.build-result.result
        ::
        =/  expected  '''
                      a
                      d
                      '''
        ::
        (expect-cage %foo !>(expected) cage)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-join  ^-  tang
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  ::
  =/  scry-results=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/txt/mar]]
      :^  ~  %hoon  hoon-src-type
      txt-scry
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/txt-diff/mar]]
      :^  ~  %hoon  hoon-src-type
      diff-scry
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/diff/txt/mar]]
      ~
    ==
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
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
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %join *] build-result.result)
        ::
        =/  =cage  cage.build-result.result
        ::
        =/  result=(urge:clay cord)  ~[[%| ~[%a] ~[%c]] [%| ~[%b] ~[%d]]]
        (expect-cage %txt-diff !>(result) cage)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-join-grad  ^-  tang
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  ::
  =/  scry-results=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/txt/mar]]
      :^  ~  %hoon  hoon-src-type
      txt-scry
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/txt-diff/mar]]
      :^  ~  %hoon  hoon-src-type
      diff-scry
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/diff/txt/mar]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/hoon/mar]]
      :^  ~  %hoon  hoon-src-type
      '''
      |_  foo=@t
      ++  grab
        |%
        ++  noun  @t
        ++  txt  of-wain:format
        --
      ++  grow
        |%
        ++  txt  (to-wain:format foo)
        --
      ++  grad  %txt
      --
      '''
    ==
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
            :^  %join  [~nul %home]  %hoon
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
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %join *] build-result.result)
        ::
        =/  =cage  cage.build-result.result
        ::
        =/  result=(urge:clay cord)  ~[[%| ~[%a] ~[%c]] [%| ~[%b] ~[%d]]]
        (expect-cage %txt-diff !>(result) cage)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-list  ^-  tang
  ::
  =/  ud-type=type  [%atom %ud ~]
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      ^=  call-args
        :*  duct=~[/count]  type=~  %build  live=%.n
            %list
            :~  [%$ %noun ud-type 1]
                [%$ %noun ud-type 2]
                [%$ %noun ud-type 3]
        ==  ==
      ::
      ^=  moves
        :~  :*  duct=~[/count]  %give  %made  ~1234.5.6  %complete
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
++  test-mash  ^-  tang
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  ::
  =/  scry-results=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/txt/mar]]
      :^  ~  %hoon  hoon-src-type
      txt-scry
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/txt-diff/mar]]
      :^  ~  %hoon  hoon-src-type
      diff-scry
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/diff/txt/mar]]
      ~
    ==
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/path]  type=~  %build  live=%.n
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
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %mash *] build-result.result)
        ::
        =/  =cage  cage.build-result.result
        ::
        =/  result=(urge:clay cord)  ~[[%| ~[%a] ~[%c]] [%| ~[%b] ~[%d]]]
        ::
        (expect-cage %txt-diff !>(result) cage)
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
++  test-multi-core-same-dependency  ^-  tang
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/gh]  type=~  %build  live=%.y
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
          (expect-eq !>(13) vase)
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
            [%x /sys/hoon/hoon]
            [%x /sys/arvo/hoon]
            [%x /sys/zuse/hoon]
          ==
        ::
        %+  expect-eq
          !>  ^-  move:ford-gate
              :*  duct=~[/gh]  %pass  wire=/clay-sub/~nul/home/~1234.5.6
                  %c  %warp  ~nul  %home
                  `[%mult [%da ~1234.5.6] files]
              ==
          !>  i.t.moves
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.7
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/gh2]  type=~  %build  live=%.y
            %core  [[~nul %home] /hoon/gh2/app]
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
          (expect-eq !>(12) vase)
        ::
        =/  files=(set [%x path])
          %-  sy  :~
            [%x /lib/old-lib/hoon]
            [%x /sur/gh/hoon]
            [%x /sur/plan-acct/hoon]
            [%x /sur/plan/acct/hoon]
            [%x /lib/old/lib/hoon]
            [%x /app/gh2/hoon]
            [%x /lib/gh/parse/hoon]
            [%x /lib/gh-parse/hoon]
            [%x /lib/connector/hoon]
            [%x /sys/hoon/hoon]
            [%x /sys/arvo/hoon]
            [%x /sys/zuse/hoon]
          ==
        ::
        %+  expect-eq
          !>  ^-  move:ford-gate
              :*  duct=~[/gh2]  %pass  wire=/clay-sub/~nul/home/~1234.5.7
                  %c  %warp  ~nul  %home
                  `[%mult [%da ~1234.5.7] files]
              ==
          !>  i.t.moves
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
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.8
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  call-args
        :*  duct=~[/gh3]  type=~  %build  live=%.y
            %core  [[~nul %home] /hoon/gh3/app]
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
          (expect-eq !>(11) vase)
        ::
        =/  files=(set [%x path])
          %-  sy  :~
            [%x /lib/old-lib/hoon]
            [%x /sur/gh/hoon]
            [%x /sur/plan-acct/hoon]
            [%x /sur/plan/acct/hoon]
            [%x /lib/old/lib/hoon]
            [%x /app/gh3/hoon]
            [%x /lib/gh/parse/hoon]
            [%x /lib/gh-parse/hoon]
            [%x /lib/connector/hoon]
            [%x /sys/hoon/hoon]
            [%x /sys/arvo/hoon]
            [%x /sys/zuse/hoon]
          ==
        ::
        %+  expect-eq
          !>  ^-  move:ford-gate
              :*  duct=~[/gh3]  %pass  wire=/clay-sub/~nul/home/~1234.5.8
                  %c  %warp  ~nul  %home
                  `[%mult [%da ~1234.5.8] files]
              ==
          !>  i.t.moves
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
    %-  ford-take-with-comparator  :*
      ford-gate
      now=~1234.5.9
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/home/~1234.5.6  duct=~[/gh]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.9] (sy [%x /lib/connector/hoon]~)]
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
            [%x /sys/hoon/hoon]
            [%x /sys/arvo/hoon]
            [%x /sys/zuse/hoon]
          ==
        ::
        ;:  weld
          (expect-eq !>(~[/gh]) !>(duct.i.moves))
          (expect-eq !>(17) vase.build-result.result.p.card.i.moves)
        ::
          %+  expect-eq
            !>  ^-  move:ford-gate
                :*  duct=~[/gh]  %pass  wire=/clay-sub/~nul/home/~1234.5.9
                    %c  %warp  ~nul  %home
                    `[%mult [%da ~1234.5.9] files]
                ==
            !>  i.t.moves
    ==  ==
  ::
  =^  results5  ford-gate
    %-  ford-take-with-comparator  :*
      ford-gate
      now=~1234.5.9
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/home/~1234.5.7  duct=~[/gh2]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.9] (sy [%x /lib/connector/hoon]~)]
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
        =/  files=(set [%x path])
          %-  sy  :~
            [%x /lib/old-lib/hoon]
            [%x /sur/gh/hoon]
            [%x /sur/plan-acct/hoon]
            [%x /sur/plan/acct/hoon]
            [%x /lib/old/lib/hoon]
            [%x /app/gh2/hoon]
            [%x /lib/gh/parse/hoon]
            [%x /lib/gh-parse/hoon]
            [%x /lib/connector/hoon]
            [%x /sys/hoon/hoon]
            [%x /sys/arvo/hoon]
            [%x /sys/zuse/hoon]
          ==
        ::
        ;:  weld
          (expect-eq !>(~[/gh2]) !>(duct.i.moves))
          (expect-eq !>(16) vase.build-result.result.p.card.i.moves)
        ::
          %+  expect-eq
            !>  ^-  move:ford-gate
                :*  duct=~[/gh2]  %pass  wire=/clay-sub/~nul/home/~1234.5.9
                    %c  %warp  ~nul  %home
                    `[%mult [%da ~1234.5.9] files]
                ==
            !>  i.t.moves
    ==  ==
  ::
  =^  results6  ford-gate
    %-  ford-take-with-comparator  :*
      ford-gate
      now=~1234.5.9
      scry=(scry-with-results-and-failures scry-results)
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/home/~1234.5.8  duct=~[/gh3]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.9] (sy [%x /lib/connector/hoon]~)]
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
        =/  files=(set [%x path])
          %-  sy  :~
            [%x /lib/old-lib/hoon]
            [%x /sur/gh/hoon]
            [%x /sur/plan-acct/hoon]
            [%x /sur/plan/acct/hoon]
            [%x /lib/old/lib/hoon]
            [%x /app/gh3/hoon]
            [%x /lib/gh/parse/hoon]
            [%x /lib/gh-parse/hoon]
            [%x /lib/connector/hoon]
            [%x /sys/hoon/hoon]
            [%x /sys/arvo/hoon]
            [%x /sys/zuse/hoon]
          ==
        ::
        ;:  weld
          (expect-eq !>(~[/gh3]) !>(duct.i.moves))
          (expect-eq !>(15) vase.build-result.result.p.card.i.moves)
        ::
          %+  expect-eq
            !>  ^-  move:ford-gate
                :*  duct=~[/gh3]  %pass  wire=/clay-sub/~nul/home/~1234.5.9
                    %c  %warp  ~nul  %home
                    `[%mult [%da ~1234.5.9] files]
                ==
            !>  i.t.moves
    ==  ==
  ::
  =^  results7  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.10
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/gh] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/gh]  %pass  /clay-sub/~nul/home/~1234.5.9
                %c  %warp  ~nul  %home  ~
    ==  ==  ==
  ::
  =^  results8  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.11
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/gh2] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/gh2]  %pass  /clay-sub/~nul/home/~1234.5.9
                %c  %warp  ~nul  %home  ~
    ==  ==  ==
  ::
  =^  results9  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.12
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/gh3] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/gh3]  %pass  /clay-sub/~nul/home/~1234.5.9
                %c  %warp  ~nul  %home  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    results3
    results4
    results5
    results6
    results7
    results8
    results9
    (expect-ford-empty ford-gate ~nul)
  ==
::  tests that we can do the simple adjacent mark case, and that we use grab
::  when both available.
::
++  test-walk-prefer-grab  ^-  tang
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
      ::  todo: %cy looks funny here
      ::
      ::  make sure we can deal with random not-hoon files in mar
      :-  [%cy [[~nul %home %da ~1234.5.6] /js/dummy/mar]]
      :-  %js
      :-  hoon-src-type
      '''
      window.onload = function()
      '''
    ==
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results scry-results)
      ::
      ^=  call-args
        :*  duct=~[/walk]  type=~  %build  live=%.y
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
            :*  duct=~[/walk]  %pass  /clay-sub/~nul/home/~1234.5.6
                %c  %warp  ~nul  %home  ~  %mult  [%da ~1234.5.6]
                %-  sy  :~
                  [%x /mar/two/hoon]
                  [%x /mar/one/hoon]
                  [%x /sys/hoon/hoon]
                  [%x /sys/arvo/hoon]
                  [%x /sys/zuse/hoon]
    ==  ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/walk] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/walk]  %pass  /clay-sub/~nul/home/~1234.5.6
                %c  %warp  ~nul  %home  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-walk-large-graph  ^-  tang
  ::
  =^  results1  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results large-mark-graph)
      ::
      ^=  call-args
        :*  duct=~[/walk]  type=~  %build  live=%.y
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
            :*  duct=~[/walk]  %pass  /clay-sub/~nul/home/~1234.5.6
                %c  %warp  ~nul  %home  ~  %mult  [%da ~1234.5.6]
                %-  sy  :~
                  [%x /mar/one/hoon]
                  [%x /mar/two/hoon]
                  [%x /mar/four/hoon]
                  [%x /mar/five/hoon]
                  [%x /sys/hoon/hoon]
                  [%x /sys/arvo/hoon]
                  [%x /sys/zuse/hoon]
    ==  ==  ==  ==
  ::
  =^  results2  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.6
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/walk] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/walk]  %pass  /clay-sub/~nul/home/~1234.5.6
                %c  %warp  ~nul  %home  ~
    ==  ==  ==
  ::
  ;:  weld
    results1
    results2
    (expect-ford-empty ford-gate ~nul)
  ==
::  +test-walk-large-graph, except we're going to shove data through it.
::
++  test-cast-large-graph  ^-  tang
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results large-mark-graph)
      ::
      ^=  call-args
        :*  duct=~[/large]  type=~  %build  live=%.n
            [%cast [~nul %home] %four [%volt [~nul %home] %one ["one" 1]]]
        ==
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ::
        ?>  =(1 (lent moves))
        ?>  ?=([^ ~] moves)
        ?>  ?=([* %give %made @da %complete %success %cast *] i.moves)
        ::
        =/  result=cage  cage.build-result.result.p.card.i.moves
        ::
        (expect-cage %four !>(["grab" "one"]) result)
    ==
  ::
  ;:  weld
    results1
    (expect-ford-empty ford-gate ~nul)
  ==
::
++  test-complex-live-cranes
  ^-  tang
  ::  set the build-cache to 2 to force eviction
  ::
  =^  results0  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.5
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/config] type=~ %keep [2.048 0]]
      moves=~
    ==
  ::
  =/  hoon-src-type=type  [%atom %$ ~]
  =/  item-type=type  -:!>([%item ''])
  =/  arch-type=type  -:!>(*arch)
  ::
  =/  scry-results1=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/program/app]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /+  things
      /=  data
        /^  box:things
        /;  |=  a=(map knot item:things)
            [*config:things a]
        /:  /===/web  /_  /item/
      ::
      data
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/things/lib]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      |%
      +$  box     [config (map knot item)]
      +$  config  [%config @t]
      +$  item    [%item @t]
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/item/mar]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /+  things
      |_  item:things
      ::  convert to
      ++  grab
        |%
        ++  noun  item:things
        --
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /hoon/item/ren]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.6] /item/data1/web]]
      :-  ~
      :-  %item
      :-  item-type
      [%item 'one']
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /web]]
      `[%arch !>(`arch`[fil=~ dir=(my [[%data1 ~] ~])])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /data1/web]]
      `[%arch !>(`arch`[fil=~ dir=(my [%item ~]~)])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.6] /item/data1/web]]
      `[%arch !>(`arch`[fil=`0v1 dir=~])]
    ==
  ::
  =^  results1  ford-gate
    %-  ford-call-with-comparator  :*
      ford-gate
      now=~1234.5.6
      scry=(scry-with-results-and-failures scry-results1)
      ::
      ^=  call-args
        ^-  [=duct type=* wrapped-task=(hobo task:able:ford-gate)]
        :*  duct=~[/app]  type=~  %build  live=%.y
            [%core source-path=`rail:ford-gate`[[~nul %home] /hoon/program/app]]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  =(2 (lent moves))
        ?>  ?=(^ moves)
        ::
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %core *] build-result.result)
        ::
        =/  =vase  vase.build-result.result
        ::
        =/  result=[[%config @t] (map knot [%item @t])]
          :-  [%config '']
          (my [%data1 [%item 'one']] ~)
        ::
        (expect-eq !>(result) vase)
        ::
        ::  TODO: check the subscription?
    ==
  ::
  =/  scry-results2=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.7] /hoon/program/app]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /+  things
      /=  data
        /^  box:things
        /;  |=  a=(map knot item:things)
            [*config:things a]
        /:  /===/web  /_  /item/
      ::
      data
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.7] /hoon/things/lib]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      |%
      +$  box     [config (map knot item)]
      +$  config  [%config @t]
      +$  item    [%item @t]
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.7] /hoon/item/mar]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /+  things
      |_  item:things
      ::  convert to
      ++  grab
        |%
        ++  noun  item:things
        --
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.7] /hoon/item/ren]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.7] /item/data1/web]]
      :-  ~
      :-  %item
      :-  item-type
      [%item 'one']
    ::
      :-  [%cx [[~nul %home %da ~1234.5.7] /item/data2/web]]
      :-  ~
      :-  %item
      :-  item-type
      [%item 'two']
    ::
      :-  [%cy [[~nul %home %da ~1234.5.7] /web]]
      `[%arch !>(`arch`[fil=~ dir=(my [[%data1 ~] [%data2 ~] ~])])]
    ::a
      :-  [%cy [[~nul %home %da ~1234.5.7] /data1/web]]
      `[%arch !>(`arch`[fil=~ dir=(my [%item ~]~)])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.7] /data2/web]]
      `[%arch !>(`arch`[fil=~ dir=(my [%item ~]~)])]

      :-  [%cy [[~nul %home %da ~1234.5.7] /item/data1/web]]
      `[%arch !>(`arch`[fil=`0v1 dir=~])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.7] /item/data2/web]]
      `[%arch !>(`arch`[fil=`0v2 dir=~])]
    ==
  ::
  =^  results2  ford-gate
    %-  ford-take-with-comparator  :*
      ford-gate
      now=~1234.5.7
      scry=(scry-with-results-and-failures scry-results2)
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/home/~1234.5.6  duct=~[/app]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            [%c %wris [%da ~1234.5.7] (sy [%y /web]~)]
        ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  =(2 (lent moves))
        ?>  ?=(^ moves)
        ::
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %core *] build-result.result)
        ::
        =/  =vase  vase.build-result.result
        ::
        =/  result=[[%config @t] (map knot [%item @t])]
          :-  [%config '']
          (my [%data1 [%item 'one']] [%data2 [%item 'two']] ~)
        ::
        (expect-eq !>(result) vase)
        ::
        ::  TODO: check the subscription?
    ==
  ::
  =/  scry-results3=(map [term beam] (unit cage))
    %-  my  :~
      :-  [%cx [[~nul %home %da ~1234.5.8] /hoon/program/app]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /+  things
      /=  data
        /^  box:things
        /;  |=  a=(map knot item:things)
            [*config:things a]
        /:  /===/web  /_  /item/
      ::
      data
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.8] /hoon/things/lib]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      |%
      +$  box     [config (map knot item)]
      +$  config  [%config @t]
      +$  item    [%item @t]
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.8] /hoon/item/mar]]
      :-  ~
      :-  %hoon
      :-  hoon-src-type
      '''
      /+  things
      |_  item:things
      ::  convert to
      ++  grab
        |%
        ++  noun  item:things
        --
      --
      '''
    ::
      :-  [%cx [[~nul %home %da ~1234.5.8] /hoon/item/ren]]
      ~
    ::
      :-  [%cx [[~nul %home %da ~1234.5.8] /item/data1/web]]
      :-  ~
      :-  %item
      :-  item-type
      [%item 'one']
    ::
      :-  [%cx [[~nul %home %da ~1234.5.8] /item/data2/web]]
      :-  ~
      :-  %item
      :-  item-type
      [%item 'changed-two']
    ::
      :-  [%cy [[~nul %home %da ~1234.5.8] /web]]
      `[%arch !>(`arch`[fil=~ dir=(my [[%data1 ~] [%data2 ~] ~])])]
    ::a
      :-  [%cy [[~nul %home %da ~1234.5.8] /data1/web]]
      `[%arch !>(`arch`[fil=~ dir=(my [%item ~]~)])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.8] /data2/web]]
      `[%arch !>(`arch`[fil=~ dir=(my [%item ~]~)])]

      :-  [%cy [[~nul %home %da ~1234.5.8] /item/data1/web]]
      `[%arch !>(`arch`[fil=`0v1 dir=~])]
    ::
      :-  [%cy [[~nul %home %da ~1234.5.8] /item/data2/web]]
      `[%arch !>(`arch`[fil=`0v222 dir=~])]
    ==
  ::
  =^  results3  ford-gate
    %-  ford-take-with-comparator  :*
      ford-gate
      now=~1234.5.7
      scry=(scry-with-results-and-failures scry-results3)
      ::
      ^=  take-args
        :*  wire=/clay-sub/~nul/home/~1234.5.7  duct=~[/app]
            ^=  wrapped-sign  ^-  (hypo sign:ford-gate)  :-  *type
            :*  %c  %wris  [%da ~1234.5.8]
                (sy [%x /web/data2/item] [%y /web/data2/item] ~)
        ==  ==
      ::
      ^=  comparator
        |=  moves=(list move:ford-gate)
        ^-  tang
        ::
        ?>  =(2 (lent moves))
        ?>  ?=(^ moves)
        ::
        ?>  ?=([* %give %made @da %complete *] i.moves)
        =/  result  result.p.card.i.moves
        ?>  ?=([%success %core *] build-result.result)
        ::
        =/  =vase  vase.build-result.result
        ::
        =/  result=[[%config @t] (map knot [%item @t])]
          :-  [%config '']
          (my [%data1 [%item 'one']] [%data2 [%item 'changed-two']] ~)
        ::
        (expect-eq !>(result) vase)
        ::
        ::  TODO: check the subscription?
    ==
  ::
  =^  results4  ford-gate
    %-  ford-call  :*
      ford-gate
      now=~1234.5.8
      scry=scry-is-forbidden
      ::
      call-args=[duct=~[/app] type=~ %kill ~]
      ::
      ^=  moves
        :~  :*  duct=~[/app]  %pass  wire=/clay-sub/~nul/home/~1234.5.8
                %c  %warp  ~nul  %home  ~
    ==  ==  ==
  ::
  ;:  weld
    results0
    results1
    results2
    results3
    results4
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
  ==
--
