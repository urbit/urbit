:>    basic containers
|%
::
++  first
  |*  a=^
  -.a
::
++  second
  |*  a=^
  +.a
::
++  either  |*([a=mold b=mold] $%({$& p/a} {$| p/b}))   ::  either
::
++  thr
  |%
  ++  apply
    :>  applies {b} {a} is first, or {b} to {a} is second.
    |*  [a=(either) b=$-(* *) c=$-(* *)]
    ?-  -.a
      $&  (b p.a)
      $|  (c p.a)
    ==
  ::
  ++  firsts
    :>  returns a list of all first elements in {a}.
    |*  a=(list (either))
    =>  .(a (homo a))
    |-
    ?~  a
      ~
    ?-  -.i.a
      $&  [p.i.a $(a t.a)]
      $|  $(a t.a)
    ==
  ::
  ++  seconds
    :>  returns a list of all second elements in {a}.
    |*  a=(list (either))
    =>  .(a (homo a))
    |-
    ?~  a
      ~
    ?-  -.i.a
      $&  $(a t.a)
      $|  [p.i.a $(a t.a)]
    ==
  ::
  ++  partition
    :>  splits the list of eithers into two lists based on first or second.
    |*  a=(list (either))
    =>  .(a (homo a))
    |-
    ^-  {(list _?>(?=({{%& *} *} a) p.i.a)) (list _?>(?=({{%| *} *} a) p.i.a))}
    ?~  a
      [~ ~]
    =+  ret=$(a t.a)
    ?-  -.i.a
      $&  [[p.i.a -.ret] +.ret]
      $|  [-.ret [p.i.a +.ret]]
    ==
  --
++  maybe  |*(a=mold $@($~ {$~ u/a}))                   ::  maybe
++  myb
  |%
  ++  is-null
    :>    returns %.y if maybe is null.
    :>
    :>  corresponds to {isJust} in haskell.
    |*  a=(maybe)
    :>  whether {a} is null.
    ?~  a  %.y
    %.n
  ::
  ++  exists
    :>    returns %.y if maybe contains a real value.
    :>
    :>  corresponds to {isNothing} in haskell.
    |*  a=(maybe)
    :>  whether {a} is not null.
    ?~  a  %.n
    %.y
  ::
  ++  need
    :>    returns the value or crashes.
    :>
    :>  corresponds to {fromJust} in haskell.
    |*  a=(maybe)
    ?~  a  ~>(%mean.[%leaf "need"] !!)
    :>  the value from the maybe.
    u.a
  ::
  ++  default
    :>    returns the value in the maybe, or a default value on null.
    :>
    :>  corresponds to {fromMaybe} in haskell.
    |*  [a=(maybe) b=*]
    ?~(a b u.a)
  ::
  ++  from-list
    :>    returns the first value of the list, or null on empty list.
    :>
    :>  corresponds to {listToMaybe} in haskell.
    |*  a=(list)
    ^-  (maybe _i.a)
    ?~  a  ~
    [~ i.a]
  ::
  ++  to-list
    :>    converts the maybe to a list.
    :>
    :>  corresponds to {maybeToList} in haskell.
    |*  a=(maybe)
    ^-  (list _u.a)
    ?~  a  ~
    [u.a ~]
  ::
  ++  concat
    :>    converts a list of maybes to a list of non-null values.
    :>
    :>  corresponds to {catMaybes} in haskell.
    |*  a=(list (maybe))
    =>  .(a (homo a))
    |-
    ^-  (list _u.+.i.-.a)
    ?~  a  ~
    ?~  i.a
      $(a t.a)
    [u.i.a $(a t.a)]
  ::
  ++  map
    :>    a version of map that can throw out items.
    :>
    :>  takes a list of items and a function of the type
    :>
    :>  todo: while this was in Data.Maybe in haskell, this might better
    :>  logically be put in our list class? murn is.
    :>
    :>  corresponds to {mapMaybes} in haskell.
    |*  [a=(list) b=$-(* (maybe))]
    =>  .(a (homo a))
    |-
    ^-  (list _,.+:*b)
    ?~  a  ~
    =+  c=(b i.a)
    ?~  c
      $(a t.a)
    ::  todo: the span of c does not have the faces of a maybe. how do i either
    ::  force a resurface or act safely on the incoming?
    [+.c $(a t.a)]
  ::
  ++  apply
    :>  applies {b} to {a}.
    |*  [a=(maybe) b=$-(* (maybe))]
    ?~  a  ~
    (b u.a)
  ::
  ::  todo: bind, bond, both, flit, hunt, lift, mate,
  ::
  ::  used in other files: bond, drop (but only once)
  ::  unusued: clap
  --
++  ls
  ::  we are back to a basic problem here: when we try to pass lists without
  ::  {i} and {t} faces, we have to use {-} and {+} to access the structure of
  ::  the list. but we then can't deal with incoming lists that do have faces,
  ::  as `+:[i="one" t=~]` is `t=~`, not `~`.
  ::
  ::  what i really want is that the sapn outside a |* is `{"" 2 "" $~}`, but
  ::  inside, it is `(list $?(@ud tape))`. all of a sudden, you don't need
  ::  ++limo or ++homo, because you have the right span from the beginning!
  ::  those two functions really feel like they're working around the type
  ::  system instead of cooperating with it.
  ::
  :>  list utilities
  |%
  :>  #  %basic
  :>    basic list manipulation
  +|
  ::
  ++  head
    :>  returns the first item in the list, which must be non-empty.
    |*  a=(list)
    =>  .(a (homo a))
    :>  the first item in the list.
    ?~  a  ~>(%mean.[%leaf "head"] !!)
    i.a
  ::
  ++  last
    :>  returns the final item in the list, which must be non-empty.
    |*  a=(list)
    :>  the last item in a list.
    ?~  a  ~>(%mean.[%leaf "last"] !!)
    ?~  t.a
      i.a
    $(a t.a)
  ::
  ++  tail
    :>  returns all items after the head of the list, which must be non-empty.
    |*  a=(list)
    ^+  a
    ?~  a  ~>(%mean.[%leaf "tail"] !!)
    t.a
  ::
  ++  init
    :>  returns all items in the list except the last one. must be non-empty.
    |*  a=(list)
    =>  .(a (homo a))
    |-
    ^+  a
    ?~  a  ~>(%mean.[%leaf "init"] !!)
    |-
    ?~  t.a
      ~
    [i.a $(a t.a)]
::    ::
::    ::  ommitted: uncons, null
::    ::
  ++  size
    :>    returns the number of items in {a}.
    :>
    :>  corresponds to {length} in haskell.
    |=  a=(list)
    =|  b=@u
    ^-  @u
    |-
    ?~  a
      b
    $(a t.a, b +(b))
  ::
  :>  #  %transformations
  :>    functions which change a list into another list
  +|
  ::
  ++  map
    :>  applies a gate to each item in the list.
    |*  [a=(list) b=$-(* *)]
    ^-  (list _*b)
    ?~  a  ~
    [(b i.a) $(a t.a)]
  ::
  ++  reverse
    :>  reverses the order of the items in the list.
    |*  a=(list)
    =>  .(a (homo a))
    ^+  a
    =+  b=`_a`~
    |-
    ?~  a  b
    $(a t.a, b [i.a b])
  ::
  ++  intersperse
    :>  places {a} between each element in {b}.
    |*  [a=* b=(list)]
    =>  .(b (homo b))
    |-
    ^+  (homo [a b])
    ?~  b
      ~
    =+  c=$(b t.b)
    ?~  c
      [i.b ~]
    [i.b a c]
  ::
  ++  intercalate
    :>  places {a} between each list in {b}, and flatten to a single list.
    |*  [a=(list) b=(list (list))]
    =>  .(a ^.(homo a), b ^.(homo b))
    |-
    ^+  (concat [a b])
    ?~  b
      ~
    =+  c=$(b t.b)
    ?~  c
      i.b
    :(weld i.b a c)
  ::
  ++  transpose
    :>  transposes rows and columns of a 2d list structure.
    |*  input=(list (list))
    ::  todo: this should homogenize with each sublist.
    ^-  (list (list))
    =/  items
      %^  foldl  input  `{(list) (list (list))}`[~ ~]
      |=  :>  current: the list of first items under construction.
          :>  remaining: the remaining item lists.
          :>  next: the next list in {input}.
          {state/{current/(list) remaining/(list (list))} next/(list)}
      ?~  next
        state
      ?~  t.next
        [[i.next current.state] remaining.state]
      [[i.next current.state] [t.next remaining.state]]
    ?~  +.items
      `(list (list))`[(reverse -.items) ~]
    [(reverse -.items) $(input (reverse +.items))]
  ::
::    ::  ++  subsequences
::    ::    |=  a=(list)
::    ::    ?~  a
::    ::      ~
::    ::    :-  -.a
::    ::    %^  foldr
::    ::    $(a +.a)
::    ::    `(list)`~
::    ::    |=  [ys=(list) r=(list)]
::    ::    ~ ::[ys [-.a ys] r ~]
::    ::  TODO:
::    ::  ++subsequences
::    ::  ++permutations

  ::
  :>  #  %folds
  :>    functions which reduce a list to a value
  +|
  ::
  ++  foldl
    :>    left associative fold
    :>
    :>  this follows haskell giving an explicit starting value instead of {roll}.
    |*  [a=(list) b=* c=$-({* *} *)]
    ^+  b
    ?~  a
      b
    $(a t.a, b (c b i.a))
  ::
  ++  foldr
    :>  right associative fold
    |*  [a=(list) b=* c=$-({* *} *)]
    ^+  b
    ?~  a
      b
    (c $(a t.a) i.a)
  ::
  ++  concat
    :>  concatenate a list of lists into a single level.
    |*  a=(list (list))
    =>  .(a ^.(homo a))
    |-  ^+  (homo i:-.a)
    ?~  a
      ~
    (weld (homo i.a) $(a t.a))
  ::
  ++  weld
    :>  combine two lists, possibly of different types.
    |*  [a=(list) b=(list)]
    =>  .(a ^.(homo a), b ^.(homo b))
    |-  ^-  (list $?(_i.-.a _i.-.b))
    ?~  a  b
    [i.a $(a t.a)]
  ::
  ++  any
    :>  returns yes if any element satisfies the predicate
    |*  [a=(list) b=$-(* ?)]
    ?~  a
      %.n
    ?|((b i.a) $(a t.a))
  ::
  ++  all
    :>  returns yes if all elements satisfy the predicate
    |*  [a=(list) b=$-(* ?)]
    ?~  a
      %.y
    ?&((b i.a) $(a t.a))
  ::
  ::  haskell has a bunch of methods like sum or maximum which leverage type
  ::  classes, but I don't think they can be written generically in hoon.
  ::
  ::
  :>  #  %building
  :>    functions which build lists
  +|
  ++  scanl
    :>  returns a list of successive reduced values from the left.
    |*  [a=(list) b=* c=$-({* *} *)]
    =>  .(a (homo a))
    |-
    ?~  a
      [b ~]
    [b $(a t.a, b (c b i.a))]
  ::
  ++  scanl1
    :>  a variant of ++scanl that has no starting value.
    |*  [a=(list) c=$-({* *} *)]
    =>  .(a (homo a))
    |-
    ?~  a
      ~
    ?~  t.a
      ~
    (scanl t.a i.a c)
  ::
  ++  scanr
    :>  the right-to-left version of scanl.
    |*  [a=(list) b=* c=$-({* *} *)]
    =>  .(a (homo a))
    |-
    ^-  (list _b)
    ?~  a
      [b ~]
    =+  rest=$(a t.a)
    ?>  ?=(^ rest)
    [(c i.a i.rest) rest]
  ::
  ++  scanr1
    :>  a variant of ++scanr that has no starting value.
    |*  [a=(list) c=$-({* *} *)]
    =>  .(a (homo a))
    |-
    ^+  a
    ?~  a
      ~
    ?~  t.a
      [i.a ~]
    =+  rest=$(a t.a)
    ?>  ?=(^ rest)
    [(c i.a i.rest) rest]
  ::
  ++  map-foldl
    :>    performs both a ++map and a ++foldl in one pass.
    :>
    :>  corresponds to {mapAccumL} in haskell.
    |*  [a=(list) b=* c=$-({* *} {* *})]
    ^-  {_b (list _+:*c)}
    ?~  a
      [b ~]
    =+  d=(c b i.a)
    =+  recurse=$(a t.a, b -.d)
    [-.recurse [+.d +.recurse]]
  ::
  ++  map-foldr
    :>    performs both a ++map and a ++foldr in one pass.
    :>
    :>  corresponds to {mapAccumR} in haskell.
    |*  [a=(list) b=* c=$-({* *} {* *})]
    ^-  {_b (list _+:*c)}
    ?~  a
      [b ~]
    =+  recurse=$(a t.a)
    =+  d=(c -.recurse i.a)
    [-.d [+.d +.recurse]]
  ::
  ++  unfoldr
    :>  generates a list from a seed value and a function.
    |*  [b=* c=$-(* (maybe {* *}))]
    |-
    ^-  (list _b)
    =+  current=(c b)
    ?~  current
      ~
    ::  todo: the span of {c} is resurfaced to have a u. this might do funky
    ::  things with faces.
    [-.+.current $(b +.+.current)]
  ::
  :>  #  %sublists
  :>    functions which return a portion of the list
  +|
  ::
  ++  take
    :>  returns the first {a} elements of {b}.
    |*  [a=@ b=(list)]
    =>  .(b (homo b))
    |-
    ^+  b
    ?:  =(0 a)
      ~
    ?~  b
      ~
    [i.b $(a (dec a), b +.b)]
  ::
  ++  drop
    :>  returns {b} without the first {a} elements.
    |*  [a=@ b=(list)]
    ?:  =(0 a)
      b
    ?~  b
      b
    $(a (dec a), b +.b)
  ::
  ++  split-at
    :>  returns {b} split into two lists at the {a}th element.
    |*  [a=@ b=(list)]
    =>  .(b (homo b))
    |-
    ^+  [b b]
    ?:  =(0 a)
      [~ b]
    ?~  b
      [~ b]
    =+  d=$(a (dec a), b t.b)
    [[i.b -.d] +.d]
  ::
  ++  take-while
    :>  returns elements from {a} until {b} returns %.no.
    |*  [a=(list) b=$-(* ?)]
    =>  .(a (homo a))
    |-
    ^+  a
    ?~  a
      ~
    ?.  (b -.a)
      ~
    [i.a $(a t.a)]
  ::
  ++  drop-while
    :>  returns elements form {a} once {b} returns %.no.
    |*  [a=(list) b=$-(* ?)]
    =>  .(a (homo a))
    |-
    ?~  a
      ~
    ?.  (b i.a)
      a
    $(a t.a)
  ::
  ++  drop-while-end
    :>  drops the largest suffix of {a} which matches {b}.
    |*  [a=(list) b=$-(* ?)]
    =>  .(a (homo a))
    |-
    ?~  a
      ~
    =+  r=$(a t.a)
    ?:  ?&(=(r ~) (b i.a))
      ~
    [i.a r]
  ::
  ++  split-on
    :>    returns [the longest prefix of {b}, the rest of the list].
    :>
    :>  corresponds to {span} in haskell. renamed to not conflict with hoon.
    |*  [a=(list) b=$-(* ?)]
    =>  .(a (homo a))
    |-
    ^+  [a a]
    ?~  a
      [~ ~]
    ?.  (b i.a)
      [~ a]
    =+  d=$(a +.a)
    [[i.a -.d] +.d]
  ::
  ++  break
    :>  like {split-on}, but reverses the return code of {b}.
    |*  [a=(list) b=$-(* ?)]
    =>  .(a (homo a))
    |-
    ^+  [a a]
    ?~  a
      [~ ~]
    ?:  (b i.a)
      [~ a]
    =+  d=$(a t.a)
    [[i.a -.d] +.d]
  ::
  ++  strip-prefix
    :>  returns a {maybe} of {b} with the prefix {a} removed, or ~ if no match.
    |*  [a=(list) b=(list)]
    ^-  (maybe _b)
    ?~  a
      `b
    ?~  b
      ~
    $(a +.a, b +.b)
  ::
  :: todo: ++group
  ::
  ++  inits
    :>    returns all initial segments in reverse order.
    :>
    :>  unlike haskell, this does not return the empty list as the first
    :>  element, as hoon uses null as the list terminator.
    |*  a=(list)
    =>  .(a (homo a))
    %-  flop
    |-
    ?~  a  ~
    [a $(a (init a))]
  ::
  ++  tails
    :>  returns all final segments, longest first.
    |*  a=(list)
    =>  .(a (homo a))
    |-
    ?~  a  ~
    [a $(a t.a)]
  ::
  :>  #  %predicates
  :>    functions which compare lists
  +|
  ::
  ++  is-prefix-of
    :>  returns %.y if the first list is a prefix of the second.
    |*  [a=(list) b=(list)]
    =>  .(a (homo a), b (homo b))
    |-
    ^-  ?
    ?~  a
      %.y
    ?~  b
      %.n
    ?.  =(i.a i.b)
      %.n
    $(a t.a, b t.b)
  ::
  ++  is-suffix-of
    :>  returns %.y if the first list is the suffix of the second.
    |*  [a=(list) b=(list)]
    =>  .(a (homo a), b (homo b))
    ^-  ?
    ::  todo: this is performant in haskell because of laziness but may not be
    ::  adequate in hoon.
    (is-prefix-of (reverse a) (reverse b))
  ::
  ++  is-infix-of
    :>  returns %.y if the first list appears anywhere in the second.
    |*  [a=(list) b=(list)]
    =>  .(a (homo a), b (homo b))
    |-
    ^-  ?
    ?~  a
      %.y
    ?~  b
      %.n
    ?:  (is-prefix-of a b)
      %.y
    $(b t.b)
  ::
  :: todo: ++is-subsequence-of
  ::
  :>  #  %searching
  :>    finding items in lists
  ::
  ++  elem
    :>  does {a} occur in list {b}?
    |*  [a=* b=(list)]
    ?~  b
      %.n
    ?:  =(a i.b)
      %.y
    $(b t.b)
  ::
  ++  lookup
    :>  looks up the key {a} in the association list {b}
    |*  [a=* b=(list (pair))]
    ^-  (maybe _+.-.b)
    ?~  b
      ~
    ?:  =(a p.i.b)
      [~ q.i.b]
    $(b t.b)
  ::
  ++  find
    :>  returns the first element of {a} which matches predicate {b}.
    |*  [a=(list) b=$-(* ?)]
    ^-  (maybe _-.a)
    ?~  a
      ~
    ?:  (b i.a)
      [~ i.a]
    $(a t.a)
  ::
  ++  filter
    :>  filter all items in {a} which match predicate {b}.
    |*  [a=(list) b=$-(* ?)]
    =>  .(a (homo a))
    |-
    ^+  a
    ?~  a
      ~
    ?.  (b i.a)
      [i.a $(a t.a)]
    $(a t.a)
  ::
  ++  partition
    :>  returns two lists, one whose elements match {b}, the other which doesn't.
    |*  [a=(list) b=$-(* ?)]
    =>  .(a (homo a))
    |-
    ^+  [a a]
    ?~  a
      [~ ~]
    =+  rest=$(a t.a)
    ?:  (b i.a)
      [[i.a -.rest] +.rest]
    [-.rest [i.a +.rest]]
  ::
  :>  #  %indexing
  :>    finding indices in lists
  +|
  ::
  ++  elem-index
    :>  returns {maybe} the first occurrence of {a} occur in list {b}.
    =|  i=@u
    |=  [a=* b=(list)]
    ^-  (maybe @ud)
    ?~  b
      ~
    ?:  =(a i.b)
      `i
    $(b t.b, i +(i))
  ::
  ++  elem-indices
    :>  returns a list of indices of all occurrences of {a} in {b}.
    =|  i/@u
    |=  [a=* b=(list)]
    ^-  (list @ud)
    ?~  b
      ~
    ?:  =(a i.b)
      [i $(b t.b, i +(i))]
    $(b t.b, i +(i))
  ::
  ++  find-index
    :>  returns {maybe} the first occurrence which matches {b} in {a}.
    =|  i=@u
    |*  [a=(list) b=$-(* ?)]
    ^-  (maybe @ud)
    ?~  a
      ~
    ?:  (b i.a)
      `i
    $(a t.a, i +(i))
  ::
  ++  find-indices
    :>  returns a list of indices of all items in {a} which match {b}.
    =|  i=@u
    |*  [a=(list) b=$-(* ?)]
    ^-  (list @ud)
    ?~  a
      ~
    ?:  (b i.a)
      [i $(a t.a, i +(i))]
    $(a t.a, i +(i))
  ::
  ++  zip
    :>  takes a list of lists, returning a list of each first items.
    |*  a=(list (list))
    =>  .(a (multi-homo a))
    |^  ^+  a
        ?~  a  ~
        ?.  valid
          ~
        =+  h=heads
        ?~  h  ~
        [heads $(a tails)]
    ::
    ++  valid
      %+  all  a
      |=  next=(list)
      ?~  a  %.n
      %.y
    ::
    ++  heads
      ^+  (homo i:-.a)
      |-
      ?~  a  ~
      ?~  i.a  ~
      [i.i.a $(a t.a)]
    ::
    ++  tails
      ^+  a
      |-
      ?~  a  ~
      ?~  i.a  ~
      [t.i.a $(a t.a)]
    --
  ++  multi-homo
    |*  a=(list (list))
    ^+  =<  $
      |%  +-  $  ?:(*? ~ [i=(homo (snag 0 a)) t=$])
      --
    a
  ::
  :>  #  %set
  :>    set operations on lists
  +|
  ++  unique
    :>    removes duplicates elements from {a}
    :>
    :>  corresponds to {nub} in haskell.
    |*  a=(list)
    =>  .(a (homo a))
    =|  seen/(list)
    ^+  a
    |-
    ?~  a
      ~
    ?:  (elem i.a seen)
      $(a t.a)
    [i.a $(seen [i.a seen], a t.a)]
  ::
  ++  delete
    :>  removes the first occurrence of {a} in {b}
    |*  [a=* b=(list)]
    =>  .(b (homo b))
    ^+  b
    |-
    ?~  b
      ~
    ?:  =(a i.b)
      t.b
    [i.b $(b t.b)]
  ::
  ++  delete-firsts
    :>  deletes the first occurrence of each element in {b} from {a}.
    |*  [a=(list) b=(list)]
    =>  .(a (homo a), b (homo b))
    |-
    ^+  a
    ?~  a
      ~
    ?~  b
      a
    ?:  (elem i.a b)
      $(a t.a, b (delete i.a b))
    [i.a $(a t.a)]
  ::
  ++  union
    :>  the list union of {a} and {b}.
    |*  [a=(list) b=(list)]
    =>  .(a (homo a), b (homo b))
    |-
    ^+  (weld a b)
    ?~  a
      b
    ?~  b
      ~
    [i.a $(a t.a, b (delete i.a b))]
  ::
  ++  intersect
    :>  the intersection of {a} and {b}.
    |*  [a=(list) b=(list)]
    =>  .(a (homo a), b (homo b))
    |-
    ^+  a
    ?~  a
      ~
    ?:  (elem i.a b)
      [i.a $(a t.a)]
    $(a t.a)
  ::
  ::  todo: everything about ++sort and ++sort-on needs more thought. the
  ::  haskell implementation uses the Ord typeclass to sort things by
  ::  default. ++sort as is is probably the correct thing to do.
  ::
  --
::
++  dict
  :>    a dictionary mapping keys of {a} to values of {b}.
  :>
  :>  a dictionary is treap ordered; it builds a treap out of the hashed key
  :>  values.
  |*  [a=mold b=mold]
  %+  cork  (tree (pair a b))
  |=  c/(tree (pair a b))  ^+  c
  ?.((valid:dct c) ~ c)
::
++  dct
  |%
  :>  #  %query
  :>    looks up values in the dict.
  +|
  ++  empty
    :>  is the dict empty?
    |*  a=(dict)
    ?~  a  %.y
    %.n
  ::
  ++  size
    :>    returns the number of elements in {a}.
    |=  a=(dict)
    ^-  @u
    ?~  a  0
    :(add 1 $(a l.a) $(a r.a))
  ::
  ++  member
    :>  returns %.y if {b} is a key in {a}.
    |=  [a=(dict) key=*]
    ^-  ?
    ?~  a  %.n
    ?|(=(key p.n.a) $(a l.a) $(a r.a))
  ::
  ++  get
    :>  grab value by key.
    |*  [a=(dict) key=*]
    ^-  (maybe _?>(?=(^ a) q.n.a))
    ::  ^-  {$@($~ {$~ u/_?>(?=(^ a) q.n.a)})}
    ?~  a
      ~
    ?:  =(key p.n.a)
      `q.n.a
    ?:  (gor key p.n.a)
      $(a l.a)
    $(a r.a)
  ::
::    ::  todo: is ++got the correct interface to have? Haskell has lookup which
::    ::  returns a Maybe and a findWithDefault which passes in a default value.
::    ++  got
::      :>  todo: move impl here.
::      :>  todo: is there a way to make b/_<><>.a ?
::      |*  [a=(dict) key=*]
::      (~(got by a) key)
  ::
  ::  todo: skipping several methods which rely on the the Ord typeclass, like
  ::  lookupLT.
  ::
  :>  #  %insertion
  +|
  ++  put
    :>    inserts a new key/value pair, replacing the current value if it exists.
    :>
    :>  corresponds to {insert} in haskell.
    |*  [a=(dict) key=* value=*]
    |-  ^+  a
    ?~  a
      [[key value] ~ ~]
    ?:  =(key p.n.a)
      ?:  =(value q.n.a)
        a
      [[key value] l.a r.a]
    ?:  (gor key p.n.a)
      =+  d=$(a l.a)
      ?>  ?=(^ d)
      ?:  (vor p.n.a p.n.d)
        [n.a d r.a]
      [n.d l.d [n.a r.d r.a]]
    =+  d=$(a r.a)
    ?>  ?=(^ d)
    ?:  (vor p.n.a p.n.d)
      [n.a l.a d]
    [n.d [n.a l.a l.d] r.d]
  ::
  ++  put-with
    :>    inserts {key}/{value}, applying {fun} if {key} already exists.
    :>
    :>  corresponds to {insertWith} in haskell.
    |*  [a=(dict) key=* value=* fun=$-({* *} *)]
    |-  ^+  a
    ?~  a
      [[key value] ~ ~]
    ?:  =(key p.n.a)
      ::  key already exists; use {fun} to resolve.
      [[key (fun q.n.a value)] l.a r.a]
    ?:  (gor key p.n.a)
      =+  d=$(a l.a)
      ?>  ?=(^ d)
      ?:  (vor p.n.a p.n.d)
        [n.a d r.a]
      [n.d l.d [n.a r.d r.a]]
    =+  d=$(a r.a)
    ?>  ?=(^ d)
    ?:  (vor p.n.a p.n.d)
      [n.a l.a d]
    [n.d [n.a l.a l.d] r.d]
  ::
  ++  put-with-key
    :>    inserts {key}/{value}, applying {fun} if {key} already exists.
    :>
    :>  corresponds to {insertWithKey} in haskell.
    |*  [a=(dict) key=* value=* fun=$-({* * *} *)]
    |-  ^+  a
    ?~  a
      [[key value] ~ ~]
    ?:  =(key p.n.a)
      ::  key already exists; use {fun} to resolve.
      [[key (fun p.n.a q.n.a value)] l.a r.a]
    ?:  (gor key p.n.a)
      =+  d=$(a l.a)
      ?>  ?=(^ d)
      ?:  (vor p.n.a p.n.d)
        [n.a d r.a]
      [n.d l.d [n.a r.d r.a]]
    =+  d=$(a r.a)
    ?>  ?=(^ d)
    ?:  (vor p.n.a p.n.d)
      [n.a l.a d]
    [n.d [n.a l.a l.d] r.d]
  ::
  ++  put-lookup-with-key
    :>    combines insertion with lookup in one pass.
    :>
    :>  corresponds to {insertLookupWithKey} in haskell.
    |*  [a=(dict) key=* value=* fun=$-({* * *} *)]
    |-  ^-  {(maybe _value) _a}
    ?~  a
      [~ [[key value] ~ ~]]
    ?:  =(key p.n.a)
      ::  key already exists; use {fun} to resolve.
      [`q.n.a [[key (fun p.n.a q.n.a value)] l.a r.a]]
    ?:  (gor key p.n.a)
      =+  rec=$(a l.a)
      =+  d=+.rec
      ?>  ?=(^ d)
      ?:  (vor p.n.a p.n.d)
        [-.rec [n.a d r.a]]
      [-.rec [n.d l.d [n.a r.d r.a]]]
    =+  rec=$(a r.a)
    =+  d=+.rec
    ?>  ?=(^ d)
    ?:  (vor p.n.a p.n.d)
      [-.rec [n.a l.a d]]
    [-.rec [n.d [n.a l.a l.d] r.d]]
  ::
  :>  #  %delete-update
  +|
  ::
  ++  delete
    :>  deletes entry at {key}.
    |*  [a=(dict) key=*]
    |-  ^+  a
    ?~  a
      ~
    ?.  =(key p.n.a)
      ?:  (gor key p.n.a)
        [n.a $(a l.a) r.a]
      [n.a l.a $(a r.a)]
    (pop-top a)
  ::
  ++  adjust
    :>  updates a value at {key} by passing the value to {fun}.
    |*  [a=(dict) key=* fun=$-(* *)]
    %^  alter-with-key  a  key
    |=  [key=_p.-.n.-.a value=(maybe _q.+.n.-.a)]
    ^-  (maybe _q.+.n.-.a)
    ?~  value  ~
    [~ (fun u.value)]
  ::
  ++  adjust-with-key
    :>  updates a value at {key} by passing the key/value pair to {fun}.
    |*  [a=(dict) key=* fun=$-({* *} *)]
    %^  alter-with-key  a  key
    |=  [key=_p.-.n.-.a value=(maybe _q.+.n.-.a)]
    ^-  (maybe _q.+.n.-.a)
    ?~  value  ~
    [~ (fun key u.value)]
  ::
  ++  update
    :>  adjusts or deletes the value at {key} by {fun}.
    |*  [a=(dict) key=* fun=$-(* (maybe *))]
    %^  alter-with-key  a  key
    |=  [key=_p.-.n.-.a value=(maybe _q.+.n.-.a)]
    ^-  (maybe _q.+.n.-.a)
    ?~  value  ~
    (fun u.value)
  ::
  ++  update-with-key
    :>  adjusts or deletes the value at {key} by {fun}.
    |*  [a=(dict) key=* fun=$-({* *} (maybe *))]
    %^  alter-with-key  a  key
    |=  [key=_p.-.n.-.a value=(maybe _q.+.n.-.a)]
    ^-  (maybe _q.+.n.-.a)
    ?~  value  ~
    (fun key u.value)
  ::
  ::  todo:
  ::  ++update-lookup-with-key
  ::
  ++  alter
    :>  inserts, deletes, or updates a value by {fun}.
    |*  [a=(dict) key=* fun=$-((maybe *) (maybe *))]
    %^  alter-with-key  a  key
    |=  [key=_p.-.n.-.a value=(maybe _q.+.n.-.a)]
    (fun value)
  ::
  ++  alter-with-key
    :>  inserts, deletes, or updates a value by {fun}.
    |*  [a=(dict) key=* fun=$-({* (maybe *)} (maybe *))]
    |-  ^+  a
    ?~  a
      =+  ret=(fun key ~)
      ?~  ret
        ~
      [[key u.ret] ~ ~]
    ?:  =(key p.n.a)
      =+  ret=(fun key `q.n.a)
      ?~  ret
        (pop-top a)
      ?:  =(u.ret q.n.a)
        a
      [[key u.ret] l.a r.a]
    ?:  (gor key p.n.a)
      =+  d=$(a l.a)
      ?~  d
        [n.a ~ r.a]
      ?:  (vor p.n.a p.n.d)
        [n.a d r.a]
      [n.d l.d [n.a r.d r.a]]
    =+  d=$(a r.a)
    ?~  d
      [n.a l.a ~]
    ?:  (vor p.n.a p.n.d)
      [n.a l.a d]
    [n.d [n.a l.a l.d] r.d]
  ::
  :>  #  %combine
  +|
  ::
  ++  union
    :>  returns the union of {a} and {b}, preferring the value from {a} if dupe
    |*  [a=(dict) b=(dict)]
    |-  ^+  a
    ?~  b
      a
    ?~  a
      b
    ?:  (vor p.n.a p.n.b)
      ?:  =(p.n.b p.n.a)
        [n.a $(a l.a, b l.b) $(a r.a, b r.b)]
      ?:  (gor p.n.b p.n.a)
        $(a [n.a $(a l.a, b [n.b l.b ~]) r.a], b r.b)
      $(a [n.a l.a $(a r.a, b [n.b ~ r.b])], b l.b)
    ?:  =(p.n.a p.n.b)
      [n.b $(b l.b, a l.a) $(b r.b, a r.a)]
    ?:  (gor p.n.a p.n.b)
      $(b [n.b $(b l.b, a [n.a l.a ~]) r.b], a r.a)
    $(b [n.b l.b $(b r.b, a [n.a ~ r.a])], a l.a)
  ::
  ++  union-with
    :>  returns the union of {a} and {b}, running {fun} to resolve duplicates.
    |*  [a=(dict) b=(dict) fun=$-({* *} *)]
    |-  ^+  a
    ?~  b
      a
    ?~  a
      b
    ?:  (vor p.n.a p.n.b)
      ?:  =(p.n.b p.n.a)
        [[p.n.a (fun q.n.a q.n.b)] $(a l.a, b l.b) $(a r.a, b r.b)]
      ?:  (gor p.n.b p.n.a)
        $(a [n.a $(a l.a, b [n.b l.b ~]) r.a], b r.b)
      $(a [n.a l.a $(a r.a, b [n.b ~ r.b])], b l.b)
    ?:  =(p.n.a p.n.b)
      [n.b $(b l.b, a l.a) $(b r.b, a r.a)]
    ?:  (gor p.n.a p.n.b)
      $(b [n.b $(b l.b, a [n.a l.a ~]) r.b], a r.a)
    $(b [n.b l.b $(b r.b, a [n.a ~ r.a])], a l.a)
  ::
  ++  union-with-key
    :>  returns the union of {a} and {b}, running {fun} to resolve duplicates.
    |*  [a=(dict) b=(dict) fun=$-({* * *} *)]
    |-  ^+  a
    ?~  b
      a
    ?~  a
      b
    ?:  (vor p.n.a p.n.b)
      ?:  =(p.n.b p.n.a)
        [[p.n.a (fun p.n.a q.n.a q.n.b)] $(a l.a, b l.b) $(a r.a, b r.b)]
      ?:  (gor p.n.b p.n.a)
        $(a [n.a $(a l.a, b [n.b l.b ~]) r.a], b r.b)
      $(a [n.a l.a $(a r.a, b [n.b ~ r.b])], b l.b)
    ?:  =(p.n.a p.n.b)
      [n.b $(b l.b, a l.a) $(b r.b, a r.a)]
    ?:  (gor p.n.a p.n.b)
      $(b [n.b $(b l.b, a [n.a l.a ~]) r.b], a r.a)
    $(b [n.b l.b $(b r.b, a [n.a ~ r.a])], a l.a)
    ::
    ::  TODO: this is untested; move it.
::    ::
::    ++  difference
::      ::  todo: move real implementation here.
::      :>  returns elements in {a} that don't exist in {b}.
::      |*  [a=(dict) b=(dict)]
::      (~(dif by a) b)
::    ::
::    ::  todo:
::    ::  ++difference-with
::    ::  ++difference-with-key
::    ::
::    ++  intersection
::      ::  todo: move real implementation here.
::      :>  returns elements in {a} that exist in {b}.
::      |*  [a=(dict) b=(dict)]
::      (~(int by a) b)
::    ::
::    ::  todo:
::    ::  ++intersection-with
::    ::  ++intersection-with-key
  ::
  :>  #  %traversal
  +|
  ::
  ++  map
    :>  applies {fun} to each value in {a}.
    |*  [a=(dict) fun=$-(* *)]
    ^-  (dict _p.-.n.-.a fun)
    ?~  a
      ~
    [[p.n.a (fun q.n.a)] $(a l.a) $(a r.a)]
  ::
  ++  map-with-key
    :>  applies {fun} to each value in {a}.
    |*  [a=(dict) fun=$-({* *} *)]
    ^-  (dict _p.-.n.-.a _*fun)
    ?~  a
      ~
    [[p.n.a (fun p.n.a q.n.a)] $(a l.a) $(a r.a)]
  ::
  ++  map-fold
    :>    performs a fold on all the values in {a}.
    :>
    :>  lists have an order, but dicts are treaps. this means there isn't a
    :>  horizontal ordering, and thus the distinction between left and right
    :>  folding isn't relevant. your accumulator function will be called in
    :>  treap order.
    :>
    :>  corresponds to {mapAccum} in haskell.
    |*  [a=(dict) b=* fun=$-({* *} {* *})]
    ^-  {_b (dict _p.-.n.-.a _+:*fun)}
    ?~  a
      [b ~]
    =+  d=(fun b q.n.a)
    =.  q.n.a  +.d
    =+  e=$(a l.a, b -.d)
    =+  f=$(a r.a, b -.e)
    [-.f [n.a +.e +.f]]
  ::
  ++  map-keys
    :>  applies {fun} to all keys.
    ::  todo: the haskell version specifies that the "greatest" original key
    ::  wins in case of duplicates. this is currently unhandled. maybe i just
    ::  shouldn't have this gate.
    |*  [a=(dict) fun=$-(* *)]
    %-  from-list
    %+  map:ls  (to-list a)
    |=  item/_n.-.a
    [(fun p.item) q.item]
  ::
  ++  map-keys-with
    :>  applies {fun} to all keys, creating a new value with {combine} on dupes.
    |*  [a=(dict) fun=$-(* *) combine=$-({* *} *)]
    ^-  (dict _*fun _q.+.n.-.a)
    =/  new-list
      %+  map:ls  (to-list a)
      |=  item/_n.-.a
      [(fun p.item) q.item]
    %^  foldl:ls  new-list
    `(dict _*fun _q.+.n.-.a)`~
    |=  [m=(dict _*fun _q.+.n.-.a) p=_i.-.new-list]
    (put-with m -.p +.p combine)
  ::
  ++  fold
    :>    performs a fold on all the values in {a}.
    :>
    :>  lists have an order, but dicts are treaps. this means there isn't a
    :>  horizontal ordering, and thus the distinction between left and right
    :>  folding isn't relevant. your accumulator function will be called in
    :>  treap order.
    |*  [a=(dict) b=* fun=$-({* *} *)]
    ^-  _b
    ?~  a
      b
    =+  d=(fun b q.n.a)
    =+  e=$(a l.a, b d)
    $(a r.a, b e)
  ::
  ++  fold-with-keys
    :>    performs a fold on all the values in {a}, passing keys too.
    |*  [a=(dict) b=* fun=$-({* * *} *)]
    ^+  b
    ?~  a
      b
    =+  d=(fun b p.n.a q.n.a)
    =+  e=$(a l.a, b d)
    $(a r.a, b e)
  ::
  ++  any
    :>  returns yes if any element satisfies the predicate
    |*  [a=(dict) b=$-(* ?)]
    ^-  ?
    ?~  a
      %.n
    ?|((b q.n.a) $(a l.a) $(a r.a))
  ::
  ++  any-with-key
    :>  returns yes if any element satisfies the predicate
    |*  [a=(dict) b=$-({* *} ?)]
    ^-  ?
    ?~  a
      %.n
    ?|((b p.n.a q.n.a) $(a l.a) $(a r.a))
  ::
  ++  all
    :>  returns yes if all elements satisfy the predicate
    |*  [a=(dict) b=$-(* ?)]
    ^-  ?
    ?~  a
      %.y
    ?&((b q.n.a) $(a l.a) $(a r.a))
  ::
  ++  all-with-key
    :>  returns yes if all elements satisfy the predicate
    |*  [a=(dict) b=$-({* *} ?)]
    ^-  ?
    ?~  a
      %.y
    ?&((b p.n.a q.n.a) $(a l.a) $(a r.a))
  ::
  :>  #  %conversion
  +|
  ++  elems
    :>  return all values in the dict.
    |*  a=(dict)
    %+  turn  (to-list a)  second
  ::
  ++  keys
    :>  returns all keys in the dict.
    |*  a=(dict)
    %+  turn  (to-list a)  first
  ::
  ::  todo: ++assocs probably doesn't make sense when we have ++to-list and
  ::  when there's no general noun ordering.
  ::
  ++  keys-set
    :>  returns all keys as a set.
    |*  a=(dict)
    (si:nl (keys a))
  ::
  ++  from-set
    :>  computes a dict by running {fun} on every value in a set.
    |*  [a=(set) fun=$-(* *)]
    ^-  (dict _n.-.a _*fun)
    ?~  a
      ~
    [[n.a (fun n.a)] $(a l.a) $(a r.a)]
  ::
  :>  #  %lists
  +|
  ::
  ++  to-list
    :>  creates a list of pairs from the tree.
    |*  a=(dict)
    =|  b=(list _n.-.a)
    |-
    ^+  b
    ?~  a
      b
    $(a r.a, b [n.a $(a l.a)])
  ::
  ++  from-list
    :>  creates a tree from a list.
    |*  a=(list (pair))
    |-
    %^  foldl:ls  a
    `(dict _p.-.i.-.a _q.+.i.-.a)`~
    |=  [m=(dict _p.-.i.-.a _q.+.i.-.a) p=_i.-.a]
    (put m p)
  ::
  ++  from-list-with
    :>  creates a dict from a list, with {fun} resolving duplicates.
    |*  [a=(list (pair)) fun=$-(* *)]
    %^  foldl:ls  a
    `(dict _*fun _q.+.i.-.a)`~
    |=  [m=(dict _*fun _q.+.i.-.a) p=_i.-.a]
    (put-with m -.p +.p fun)
  ::
  ::  todo: without a natural ordering, association lists and gates to operate
  ::  on them probably don't make sense. i'm skipping them for now.
  ::
  :>  #  %filters
  +|
  ++  filter
    :>  filters a dict of all values that satisfy {fun}.
    |*  [a=(dict) fun=$-(* ?)]
    %+  filter-with-key  a
    |=  [key=* value=_q.+.n.-.a]
    (fun value)
  ::
  ++  filter-with-key
    :>  filters a dict of all values that satisfy {fun}.
    |*  [a=(dict) fun=$-({* *} ?)]
    |-
    ^+  a
    ?~  a  ~
    ?:  (fun n.a)
      =.  l.a  $(a l.a)
      =.  r.a  $(a r.a)
      (pop-top a)
    [n.a $(a l.a) $(a r.a)]
  ::
  ++  restrict-keys
    :>  returns a dict where the only allowable keys are {keys}.
    |*  [a=(dict) keys=(set)]
    %+  filter-with-key  a
    |=  [key=_p.-.n.-.a value=*]
    ::  todo: replace this with a call to our set library when we advance that
    ::  far.
    !(~(has in keys) key)
  ::
  ++  without-keys
    :>  returns a dict where the only allowable keys are not in {keys}.
    |*  [a=(dict) keys=(set)]
    %+  filter-with-key  a
    |=  [key=_p.-.n.-.a value=*]
    ::  todo: replace this with a call to our set library when we advance that
    ::  far.
    (~(has in keys) key)
  ::
  ++  partition
    :>  returns two lists, one whose elements match {fun}, the other doesn't.
    |*  [a=(dict) fun=$-(* ?)]
    ::  todo: is the runtime on this is bogus?
    =/  data
      %+  partition:ls  (to-list a)
      |=  p/_n.-.a
      (fun q.p)
    [(from-list -.data) (from-list +.data)]
  ::
  ::  todo:  ++partition-with-key once ++partition works.
  ::
  ::  i'm going to ignore all the Antitone functions; they don't seem to be
  ::  useful without ordering on the dict.
  ::
  ++  map-maybe
    :>  a version of map that can throw out items.
    |*  [a=(dict) fun=$-(* (maybe))]
    %+  map-maybe-with-key  a
    |=  [key=* value=_q.+.n.-.a]
    (fun value)
  ::
  ++  map-maybe-with-key
    :>  a version of map that can throw out items.
    |*  [a=(dict) fun=$-({* *} (maybe))]
    ^-  (dict _p.-.n.-.a _+:*fun)
    ?~  a  ~
    =+  res=(fun n.a)
    ?~  res
      =.  l.a  $(a l.a)
      =.  r.a  $(a r.a)
      (pop-top a)
    [[p.n.a +.res] $(a l.a) $(a r.a)]
  ::
  ++  map-either
    :>  splits the dict in two on a gate that returns an either.
    |*  [a=(dict) fun=$-(* (either))]
    %+  map-either-with-key  a
    |=  [key=* value=_q.+.n.-.a]
    (fun value)
  ::
  ++  map-either-with-key
    :>  splits the dict in two on a gate that returns an either.
    |*  [a=(dict) fun=$-({* *} (either))]
    |-
    ^-  $:  (dict _p.-.n.-.a _?>(?=({{%& *} *} *fun) +:*fun))
            (dict _p.-.n.-.a _?>(?=({{%| *} *} *fun) +:*fun))
        ==
    ?~  a
      [~ ~]
    ::  todo: runtime wise, can I do better than recursive unions?
    =+  lr=$(a l.a)
    =+  rr=$(a r.a)
    =+  x=(fun n.a)
    ~!  x
    ?-  -.x
      $&  [(put (union -.lr -.rr) p.n.a +.x) (union +.lr +.rr)]
      $|  [(union -.lr -.rr) (put (union +.lr +.rr) p.n.a +.x)]
    ==
  ::
  ::  ++split, ++split-lookup and ++split-root do not make sense without
  ::  ordinal keys.
  ::
  ++  is-subdict
    :>  returns %.y if every element in {a} exists in {b} with the same value.
    |*  [a=(dict) b=(dict)]
    ^-  ?
    (is-subdict-by a b |=([a=* b=*] =(a b)))
  ::
  ++  is-subdict-by
    :>  returns %.y if every element in {a} exists in {b} with the same value.
    |*  [a=(dict) b=(dict) fun=$-({* *} ?)]
    |-
    ^-  ?
    ?~  a  %.y
    ?~  b  %.n
    ~!  b
    ~!  p.n.a
    =+  x=(get b p.n.a)
    ?~  x  %.n
    |((fun q.n.a u.x) $(a l.a) $(a r.a))
  ::
  :>  #  %impl
  :>    implementation details
  +|
  ++  pop-top
    :>  removes the head of the tree and rebalances the tree below.
    |*  a=(dict)
    ^-  {$?($~ _a)}
    ?~  a  ~
    |-
    ?~  l.a  r.a
    ?~  r.a  l.a
    ?:  (vor p.n.l.a p.n.r.a)
      [n.l.a l.l.a $(l.a r.l.a)]
    [n.r.a $(r.a l.r.a) r.r.a]
  ::
  ++  valid
    :>  returns %.y if {a} if this tree is a valid treap dict.
    |*  a=(tree (pair * *))
    =|  [l=(maybe) r=(maybe)]
    |-  ^-  ?
    ?~  a   &
    ?&  ?~(l & (gor p.n.a u.l))
        ?~(r & (gor u.r p.n.a))
        ?~(l.a & ?&((vor p.n.a p.n.l.a) $(a l.a, l `p.n.a)))
        ?~(r.a & ?&((vor p.n.a p.n.r.a) $(a r.a, r `p.n.a)))
    ==
  --
++  random
  :>    produces a core which produces random numbers.
  :>
  :>  random numbers are generated through repeated sha-256 operations.
  :>
  :>  this design forces implementation details to be hidden, forces users to
  :>  go through =^. this should be less error prone for pulling out multiple
  :>  random numbers, at the cost of making getting a single random number
  :>  slightly more cumbersome.
  :>
  :>      =+  gen=(random eny)
  :>      =^  first  gen  (range:gen 0 10)
  :>      =^  second  gen  (range:gen 0 10)
  |=  a=@
  =>  |%
      ++  raw                                               ::  random bits
        |=  b=@  ^-  @
        %+  can
          0
        =+  c=(shas %og-a (mix b a))
        |-  ^-  (list {@ @})
        ?:  =(0 b)
          ~
        =+  d=(shas %og-b (mix b (mix a c)))
        ?:  (lth b 256)
          [[b (end 0 b d)] ~]
        [[256 d] $(c d, b (sub b 256))]
      ::
      ++  rad                                               ::  random in range
        |=  b=@  ^-  @
        =+  c=(raw (met 0 b))
        ?:((lth c b) c $(a +(a)))
      --
  ^?  |%
  ++  range
    :>  returns a random number in the range [start, end], and generator.
    |=  [start=@ end=@]
    ?:  (gte start end)
      ~_(leaf+"invalid range" !!)
    =+  offset=(sub end start)
    =+  r=(rad offset)
    [(add start r) +>.$(a (shas %og-s (mix a r)))]
  ::
  ++  bits
    :>  returns {b} bits in the range, and generator.
    |=  b=@
    =+  r=(raw b)
    [r +>.$(a (shas %og-s (mix a r)))]
  --
--
