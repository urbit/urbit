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
  =+  fate=!>(|=([a=(list @ud) b=(list @ud)] ^-(? =((flop (weld a b)) (weld (flop b) (flop a))))))
  =+  check=~(check quiz `@uv`1 100)
  =+  giel=((gen-list.check @ud) (gen-atom.check @ud))
  =+  gief=((gen-cell.check (list @ud) (list @ud)) giel giel)
  %-  expect  !>((check fate `gief ~))
++  test-gen-noun
  :: if a noun is a cell, then the subtrees are equal iff the hashes of the subtrees are equal.
  =+  fate=!>(|=(* ^-($?(? %drop) ?.(.?(+6) %drop =(=(+12 +13) =((sham +12) (sham +13)))))))
  =+  check=~(check quiz `@uv`1 100)
  %-  expect  !>((check fate `gen-noun.quiz ~))
++  test-gen-const
  :: here, part of the sample always stays the same.
  =+  fate=!>(|=([@ @tas] =(+13 %constant)))
  =+  check=~(check quiz `@uv`1 100)
  =+  gief=((gen-cell.quiz * @tas) gen-noun.quiz ((gen-const.quiz @tas) %constant))
  %-  expect  !>((check fate `gief ~))
++  test-gen-freq
  :: gen-freq lets you specify a list of generators and have each chosen with a different frequency.
  :: in this case, we will mostly generate atoms, and sometimes a noun where the subtrees are equal.
  :: we should observe a %drop rate of ~67 percent.
  =+  fate=!>(|=(* ?.(.?(+6) %drop =(+12 +13))))
  =+  check=~(check quiz `@uv`1 100)
  =/  dup=(give.quiz *)
    |=  [size=@ud rng=_og]
    ^-  *
    =+  a=(gen-noun.quiz size rng)
    [a a]
  =/  l=(list (pair @ (give.quiz *)))  ~[[3 (gen-atom.quiz @)] [1 dup]]
  =+  gief=((gen-freq.quiz *) l)
  %-  expect  !>((check fate `gief ~))
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