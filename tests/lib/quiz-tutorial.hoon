/+  quiz, *test
::
:: this file contains example tests for how to use the quiz library.
:: the quiz library is a library for property-based testing.
:: it is inspired by the haskell library quickcheck.
:: the idea is that you write a gate that takes a sample and returns a loobean.
:: the gate is then run with a number of random samples, and the results are
:: checked for consistency. if the gate returns %.y for all samples, the test
:: passes. if it returns %.n for any sample, the test fails. if it returns
:: %drop for any sample, the test runner will report how many samples were
:: dropped. if the test runner runs out of entropy, it will report how many
:: samples were actually run.
:: %quiz can generate most types of samples automatically. in many instances,
:: it's preferable however for the user to supply a 'norn' gate that generates
:: samples. this is because the user may have some knowledge about the domain
:: that can be used to generate more interesting samples. for example, if you
:: are testing a sorting algorithm, you may want to generate sorted lists.
:: %quiz can also shrink samples automatically. but here, too, the user may
:: supply a gate that shrinks samples in a more intelligent way. this gate is
:: called 'alts', because it returns a list of alternative samples, that should
:: be in some sense smaller than the original sample.
::
|%
:: assign an easier name to this core.
::
++  nor  norns.quiz
::
:: declare standard parameters for check.
:: every instance of quiz is seeded with some entropy.
:: that means test runs will not be flaky and will run exactly the same
:: each time.
::
++  check  ~(check quiz `@uv`1 2.000)
++  test-add-sub
  ::
  :: a fate is a gate that takes any sample and returns a loobean.
  :: returning %.n means the test failed, and %.y means it passed.
  ::
  =+  fate=!>(|=([a=@ b=@] =(0 (sub (sub (add a b) b) a))))
  :: you can seed the gate with any entropy and number of runs you like.
  ::
  =+  check=~(check quiz `@uv`1 30.000)  :: we can choose to run more times, or set the entropy
  :: the `check` arm will return %.y if all runs passed, and %.n if any failed.
  :: it takes two optional parameters as units, which we leave alone for now.
  ::
  %-  expect  !>((check fate ~ ~))
++  test-noun
  ::
  :: if a noun is a cell, then the subtrees are equal iff the hashes of the subtrees are equal.
  ::
  =+  fate=!>(|=(* ^-($?(? %drop) ?.(.?(+6) %drop =(=(+12 +13) =((sham +12) (sham +13)))))))
  =+  check=check.quiz :: we can also run with standard parameters: `@uv`0 entropy and 100 runs.
  %-  expect  !>((check fate ~ ~))
++  test-jam-cue
  =+  fate=!>(|=(a=* ^-(? =(a (cue (jam a))))))
  %-  expect  !>((check fate ~ ~))
++  test-drop
  ::
  :: if you only care about some samples, you can drop the others by returning
  :: %drop rather than %.y or %.n.
  :: the test runner will report how many generated samples were dropped.
  ::
    =+  fate=!>(|=([a=@ b=@] ^-($?(%drop ?) ?:((lth b a) %drop =(b (add (sub b a) a))))))
  %-  expect  !>((check fate ~ ~))
++  test-tired
  ::
  :: by virtue of random sample generation, the same sample may get generated
  :: multiple times.  in these instances, the test is not re-run.  instead, every
  :: such collission gets counted, and after a set number (10.000 currently) have
  :: occured, the test bails due to being %tired and reports the number of actual
  :: unique successful runs. so in this fate, there are only 4 possible samples,
  :: all will be found, and even though we choose to do the default number of runs,
  :: the check will get %tired and only do 4 in total.
  ::
  =+  fate=!>(|=([a=? b=?] |(=(a b) ?!(=(a b)))))
  %-  expect  !>((check fate ~ ~))
++  test-giving
  ::
  :: here we supply the quiz libary with a 'norn', a gate that generates input.
  :: in our case, it's sorted lists of natural numbers, descending.
  ::
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
  ::
  :: here we supply the quiz library with an 'alts' gate, a gate that shrinks
  :: input.
  :: since we are checking for sortedness, we can shrink by returning a number
  :: of 2-element lists.
  :: be careful to ensure that each new alternative is smaller than than the
  :: original, because otherwise the search for the 'smallest' failing sample
  :: may not terminate.
  ::
  =/  fate
    !>
    |=  a=(list @ud)
    =(a (sort a gth))
  =/  give
    ^-  (norn.quiz (list @ud))
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
  ::
  :: quiz can even generate automatically more complex types, such as lists.
  :: however, since the approach works for almost all types it is also not very
  :: specialized on any particular type.
  :: for example, we can state confidently that no list of length 50 or more will be generated.
  :: this is because the algorithm has a 50/50 chance to terminate each time
  :: after picking a new element, so the odds of a length 50 list is 1/2^50.
  ::
  =+  fate=!>(|=([a=(list @ud) b=(list @ud)] ^-(? &((gth 50 (lent a)) =((flop (weld a b)) (weld (flop b) (flop a)))))))
  %-  expect  !>((check fate ~ ~))
++  test-flop-2
  ::
  :: in the following we use quiz's built-in 'norn' for lists.
  :: it is far more intelligent in list creation than the default behavior.
  :: for example, it will create lists of random lengths with a much better
  :: distribtuion, gradually growing the likely length of the list every run.
  :: here we use some combinators to create new norns.
  ::
  =+  fate=!>(|=([a=(list @ud) b=(list @ud)] ^-(? &((gth 100 (lent a)) =((flop (weld a b)) (weld (flop b) (flop a)))))))
  :: we can generate a list of any type.
  :: specify the type of the elements of the list by passing the mold to the list norn.
  :: then pass it a norn of that type.
  ::
  =+  giel=((list:nor @ud) (atom:nor @ud))
  =+  gief=((cell:nor (list @ud) (list @ud)) giel giel)
  %+  weld
  :: this will generate even some very long lists with high probability.
  ::
  %+  expect-eq  !>(|)  !>((check fate `gief ~))
  :: if we fix the test to remove the bug where we only accepted short lists, the fate is heeded.
  ::
  =+  fate=!>(|=([a=(list @ud) b=(list @ud)] ^-(? =((flop (weld a b)) (weld (flop b) (flop a))))))
  %-  expect  !>((check fate ~ ~))
++  test-norn-const
  ::
  :: here, part of the sample always stays the same, because we use the constant norn.
  ::
  =+  fate=!>(|=([@ @tas] =(+13 %constant)))
  =+  gief=((cell:nor * @tas) noun:nor ((const:nor @tas) %constant))
  %-  expect  !>((check fate `gief ~))
++  test-norn-freq
  ::
  :: the freq norn lets you specify a list of generators and have each chosen with a different frequency.
  :: in this case, we will mostly generate atoms, and sometimes a noun where the subtrees are equal.
  :: we should observe a %drop rate of ~67 percent.
  ::
  =+  fate=!>(|=(* ?.(.?(+6) %drop =(+12 +13))))
  =/  dup=(norn.quiz *)
    |=  [size=@ud rng=_og]
    ^-  *
    =+  a=(noun:nor size rng)
    [a a]
  =/  l=(list (pair @ (norn.quiz *)))  ~[[3 (atom:nor @)] [1 dup]]
  =+  gief=((freq:nor *) l)
  %-  expect  !>((check fate `gief ~))
++  test-norn-pick
  ::
  :: the pick norn is similar to the freq norn, but assigns equal probability to each norn.
  ::
  =+  fate=!>(|=(a=@ ?:(=(a 2) %drop =(a 3))))
  =+  gief=((pick:nor @) ~[((const:nor @) 2) ((const:nor @) 3)])
  %-  expect  !>((check fate `gief ~))
--