/+  quiz, *test
|%
++  giv  givers.quiz
:: declare standard parameters for check.
:: every instance of quiz is seeded with some entropy.
:: that means test runs will not be flaky and will run exactly the same
:: each time.
++  check  ~(check quiz `@uv`1 2.000)
++  test-add-sub
  :: a fate is a gate that takes any sample and returns a loobean.
  :: returning %.n means the test failed, and %.y means it passed.
  =+  fate=!>(|=([a=@ b=@] =(0 (sub (sub (add a b) b) a))))
  :: you can seed the gate with any entropy and number of runs you like.
  =+  check=~(check quiz `@uv`1 30.000)  :: we can choose to run more times, or set the entropy
  :: the `check` arm will return %.y if all runs passed, and %.n if any failed.
  :: it takes two optional parameters as units, which we leave alone for now.
  %-  expect  !>((check fate ~ ~))
++  test-noun
  :: if a noun is a cell, then the subtrees are equal iff the hashes of the subtrees are equal.
  =+  fate=!>(|=(* ^-($?(? %drop) ?.(.?(+6) %drop =(=(+12 +13) =((sham +12) (sham +13)))))))
  =+  check=check.quiz :: we can also run with standard parameters: `@uv`0 entropy and 100 runs.
  %-  expect  !>((check fate ~ ~))
++  test-jam-cue
  =+  fate=!>(|=(a=* ^-(? =(a (cue (jam a))))))
  %-  expect  !>((check fate ~ ~))
++  test-drop
  :: if you only care about some samples, you can drop the others by returning
  :: %drop rather than %.y or %.n.
  :: the test runner will report how many generated samples were dropped.
    =+  fate=!>(|=([a=@ b=@] ^-($?(%drop ?) ?:((lth b a) %drop =(b (add (sub b a) a))))))
  %-  expect  !>((check fate ~ ~))
++  test-tired
  :: by virtue of random sample generation, the same sample may get generated
  :: multiple times.  in these instances, the test is not re-run.  instead, every
  :: such collission gets counted, and after a set number (10.000 currently) have
  :: occured, the test bails due to being %tired and reports the number of actual
  :: unique successful runs. so in this fate, there are only 4 possible samples,
  :: all will be found, and even though we choose to do the default number of runs,
  :: the check will get %tired and only do 4 in total.
  =+  fate=!>(|=([a=? b=?] |(=(a b) ?!(=(a b)))))
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
  %+  expect-eq  !>(|)  !>((check fate `give `alts))
++  test-flop-1 
  :: quiz can even generate automatically more complex types, such as lists.
  :: however, since the approach works for almost all types it is also not very
  :: specialized on any particular type.
  :: for example, we can state confidently that no list of length 50 or more will be generated.
  :: this is because the algorithm has a 50/50 chance to terminate each time
  :: after picking a new element, so the odds of a length 50 list is 1/2^50.
  =+  fate=!>(|=([a=(list @ud) b=(list @ud)] ^-(? &((gth 50 (lent a)) =((flop (weld a b)) (weld (flop b) (flop a)))))))
  %-  expect  !>((check fate ~ ~))
++  test-flop-2
  :: in the following we use quiz's built-in "giver" for lists.
  :: it is far more intelligent in list creation than the default behavior.
  :: for example, it will create lists of random lengths with a much better
  :: distribtuion, gradually growing the likely length of the list every run.
  :: here we use some combinators to create new givers.
  =+  fate=!>(|=([a=(list @ud) b=(list @ud)] ^-(? &((gth 100 (lent a)) =((flop (weld a b)) (weld (flop b) (flop a)))))))
  :: we can generate a list of any type.
  :: specify the type of the elements of the list by passing the mold to the list giver.
  :: then pass it a giver of that type.
  =+  giel=((list:giv @ud) (atom:giv @ud))
  =+  gief=((cell:giv (list @ud) (list @ud)) giel giel)
  %+  weld
  :: this will generate even some very long lists with high probability.
  %+  expect-eq  !>(|)  !>((check fate `gief ~))
  :: if we fix the test to remove the bug where we only accepted short lists, the fate is heeded.
  =+  fate=!>(|=([a=(list @ud) b=(list @ud)] ^-(? =((flop (weld a b)) (weld (flop b) (flop a))))))
  %-  expect  !>((check fate ~ ~))
++  test-giver-const
  :: here, part of the sample always stays the same, because we use the constant giver.
  =+  fate=!>(|=([@ @tas] =(+13 %constant)))
  =+  gief=((cell:giv * @tas) noun:giv ((const:giv @tas) %constant))
  %-  expect  !>((check fate `gief ~))
++  test-giver-freq
  :: the freq giver lets you specify a list of generators and have each chosen with a different frequency.
  :: in this case, we will mostly generate atoms, and sometimes a noun where the subtrees are equal.
  :: we should observe a %drop rate of ~67 percent.
  =+  fate=!>(|=(* ?.(.?(+6) %drop =(+12 +13))))
  =/  dup=(give.quiz *)
    |=  [size=@ud rng=_og]
    ^-  *
    =+  a=(noun:giv size rng)
    [a a]
  =/  l=(list (pair @ (give.quiz *)))  ~[[3 (atom:giv @)] [1 dup]]
  =+  gief=((freq:giv *) l)
  %-  expect  !>((check fate `gief ~))
++  test-giver-pick
  :: the pick giver is similar to the freq giver, but assigns equal probability to each giver.
  =+  fate=!>(|=(a=@ ?:(=(a 2) %drop =(a 3))))
  =+  gief=((pick:giv @) ~[((const:giv @) 2) ((const:giv @) 3)])
  %-  expect  !>((check fate `gief ~))
--