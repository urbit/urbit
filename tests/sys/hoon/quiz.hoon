/+  quiz, *test
|%
++  test-add-sub
  :: a fate is a gate that takes any sample and returns a loobean.
  :: returning %.n means the test failed, and %.y means it passed.
  =+  fate=!>(|=([a=@ b=@] =(0 (sub (sub (add a b) b) a))))
  =+  check=~(check quiz `@uv`1 30.000)
  %-  expect  !>((check fate ~ ~))
++  test-jam-cue
  =+  fate=!>(|=(a=* ^-(? =(a (cue (jam a))))))
  =+  check=~(check quiz `@uv`1 2.000)
  %-  expect  !>((check fate ~ ~))
++  test-flop
  =+  fate=!>(|=(a=(list @ud) ^-(? =(a (flop (flop a))))))
  =+  check=~(check quiz `@uv`1 100)
  =+  gief=((gen-list.check @ud) (gen-atom.check @ud))
  %-  expect  !>((check fate `gief ~))
++  test-gen-noun
  =+  fate=!>(|=(* ^-(? %.y)))
  =+  check=~(check quiz `@uv`1 100)
  %-  expect  !>((check fate `gen-noun.quiz ~))
++  test-drop
  :: if you only care about some samples, you can drop the others by returning
  :: %drop rather than %.y or %.n.
  :: the test runner will report how many generated samples were dropped.
    =+  fate=!>(|=([a=@ b=@] ^-($?(%drop ?) ?:((lth b a) %drop =(b (add (sub b a) a))))))
  =+  check=~(check quiz `@uv`1 2.000)
  %-  expect  !>((check fate ~ ~))
++  test-giving
  :: here we supply the quiz libary with a 'giver', a gate that generates input.
  :: in our case, it's sorted lists of natural numbers, descending.
  =/  fate
    !>
    |=  a=(list @ud)
    =(a (sort a gth))
  =/  give
    |=  [size=@ud rng=_og]
    !>
    |-
    ^-  (list @ud)
    ?:  =(0 size)
      ~
    =^  ran  rng  (rads:rng size)
    [size $(size ran, rng rng)]
  =+  check=~(check quiz `@uv`1 3.000)
  %-  expect  !>((check fate `give ~))
++  test-shrinking
  :: here we supply the quiz library with an 'alts' gate, a gate that shrinks
  :: input.
  :: since we are checking for sortedness, we can shrink by returning a number
  :: of 2-element lists.
  =/  fate
    !>
    |=  a=(list @ud)
    =(a (sort a gth))
  =/  give
    ^-  (give.quiz (list @ud))
    |=  [size=@ud rng=_og]
    |-
    ^-  (list @ud)
    ?:  (gth size 1.000.000) 
      ~[6 5 4 2 3 1]
    ?:  =(0 size)
      ~
    =^  ran  rng  (rads:rng size)
    [size $(size ran, rng rng)]
  =/  alts
    |=  sam=vase
    =+  ssam=!<((list @ud) sam)
    :: return the two first elements of list (in a list) and tail.
    ^-  (list vase)
    ?:  (lte (lent ssam) 2)
      ~
    ~[!>((scag 2 ssam)) !>((slag 1 ssam))]
  =+  check=~(check quiz `@uv`1 3.000)
  %+  expect-eq  !>(|)  !>((check fate `give `alts))
--