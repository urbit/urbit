::  Tests for +by (map logic)
::
/+  *test
::
=>  ::  Utility core
    ::
    |%
    ++  map-of-doubles
      |=  l=(list @)
      ^-  (map @ @)
      %-  my
      ^-  (list (pair @ @))
      %+  turn  l
        |=  k=@
        [k (mul 2 k)]
    --
::
=>  ::  Test Data
    ::
    |%
    +|  %test-suite
    ++  m-uno  (map-of-doubles ~[42])
    ++  m-dos  (map-of-doubles ~[6 9])
    ++  m-tre  (map-of-doubles ~[1 0 1])
    ++  m-asc  (map-of-doubles ~[1 2 3 4 5 6 7])
    ++  m-des  (map-of-doubles ~[7 6 5 4 3 2 1])
    ++  m-uns  (map-of-doubles ~[1 6 3 5 7 2 4])
    ++  m-dup  (map-of-doubles ~[1 1 7 4 6 9 4])
    ++  m-nul  *(map @ @)
    ++  m-lis  ~[m-nul m-uno m-dos m-tre m-asc m-des m-uns m-dup]
    --
::  Testing arms
::
|%
::  Test logical AND
::
++  test-map-all  ^-  tang
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  %.y
      !>  (~(all by m-nul) |=(* &))
    %+  expect-eq
      !>  %.y
      !>  (~(all by m-nul) |=(* |))
    ::  Checks one element fails
    ::
    %+  expect-eq
      !>  %.n
      !>  (~(all by m-uno) |=(e=@ =(e 43)))
    ::  Checks >1 element fails
    ::
    %+  expect-eq
      !>  %.n
      !>  (~(all by m-dos) |=(e=@ (lth e 4)))
    ::  Checks all elements pass
    ::
    %+  expect-eq
      !>  %.y
      !>  (~(all by m-des) |=(e=@ (lth e 80)))
  ==
::
::  Test logical OR
::
++  test-map-any  ^-  tang
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  %.n
      !>  (~(any by m-nul) |=(* &))
    %+  expect-eq
      !>  %.n
      !>  (~(any by m-nul) |=(* |))
    ::  Checks one element fails
    ::
    %+  expect-eq
      !>  %.n
      !>  (~(any by m-uno) |=(e=@ =(e 43)))
    ::  Checks >1 element fails
    ::
    %+  expect-eq
      !>  %.n
      !>  (~(any by m-dos) |=(e=@ (lth e 4)))
    ::  Checks one element passes
    ::
    %+  expect-eq
      !>  %.y
      !>  (~(any by m-des) |=(e=@ =(e 14)))
    ::  Checks all element pass
    ::
    %+  expect-eq
      !>  %.y
      !>  (~(any by m-des) |=(e=@ (lth e 100)))
  ==
::
::  Test check correctnes (correct horizontal & vertical order)
::
++  test-map-apt  ^-  tang
  ::  manually constructed maps with predefined vertical/horizontal
  ::  ordering
  ::
  ::  for the following three keys (1, 2, 3) the vertical priorities are:
  ::    > (mug (mug 1))
  ::    1.405.103.437
  ::    > (mug (mug 2))
  ::    1.200.431.393
  ::    > (mug (mug 3))
  ::    1.576.941.407
  ::
  ::  and the ordering 2 < 1 < 3
  ::  a correctly balanced tree stored as a min-heap
  ::  should have key=2 as the root
  ::
  ::  The horizontal priorities are:
  ::    > (mug 1)
  ::    1.901.865.568
  ::    > (mug 2)
  ::    1.904.972.904
  ::    > (mug 3)
  ::    1.923.673.882
  ::
  ::  and the ordering 1 < 2 < 3.
  ::
  ::  1 should be in the left brach and 3 in the right one.
  ::
  =/  balanced-a=(map @ @)    [[2 2] [[1 1] ~ ~] [[3 3] ~ ~]]
  ::  doesn't follow vertical ordering
  ::
  =/  unbalanced-a=(map @ @)  [[1 1] [[2 2] ~ ~] [[3 3] ~ ~]]
  =/  unbalanced-b=(map @ @)  [[1 1] ~ [[2 2] ~ ~]]
  =/  unbalanced-c=(map @ @)  [[1 1] [[2 2] ~ ~] ~]
  ::  doesn't follow horizontal ordering
  ::
  =/  unbalanced-d=(map @ @)  [[2 2] [[3 3] ~ ~] [[1 1] ~ ~]]
  ::  doesn't follow horizontal & vertical ordering
  ::
  =/  unbalanced-e=(map @ @)  [[1 1] [[3 3] ~ ~] [[2 2] ~ ~]]
  ;:  weld
    %+  expect-eq
      !>  [%b-a %.y]
      !>  [%b-a ~(apt by balanced-a)]
    %+  expect-eq
      !>  [%u-a %.n]
      !>  [%u-a ~(apt by unbalanced-a)]
    %+  expect-eq
      !>  [%u-b %.n]
      !>  [%u-b ~(apt by unbalanced-b)]
    %+  expect-eq
      !>  [%u-c %.n]
      !>  [%u-c ~(apt by unbalanced-c)]
    %+  expect-eq
      !>  [%u-d %.n]
      !>  [%u-d ~(apt by unbalanced-d)]
    %+  expect-eq
      !>  [%u-e %.n]
      !>  [%u-e ~(apt by unbalanced-e)]
  ==
::
::  Test bifurcation (i.e. splits map a into two, discarding -.a)
::
++  test-map-bif  ^-  tang
  ::  The traversal of the +map is done comparing the double +mug
  ::  of the key of the added node and the one from the tree.
  ::  Because of this, the search will stop at different leaves,
  ::  based on the value of the hash, therefore the right and left
  ::  maps that are returned can be different
  ::  (null or a less than the total number of nodes)
  ::  The best way to check is that the sum of the number of nodes
  ::  in both maps are the same as before, and that both returned
  ::  maps are correct
  ::
  =/  splits-a=[(map) (map)]  (~(bif by m-des) [99 99])
  =/  splits-b=[(map) (map)]  (~(bif by m-des) [6 12])
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  [~ ~]
      !>  (~(bif by m-nul) [1 2])
    ::  Checks bifurcating by non-existing element
    ::
    %+  expect-eq
      !>  7
      !>  (add ~(wyt by -.splits-a) ~(wyt by +.splits-a))
    %+  expect-eq
      !>  %.y
      !>  &(~(apt by -.splits-a) ~(apt by +.splits-a))
    ::  Checks splitting by existing element
    ::
    %+  expect-eq
      !>  6
      !>  (add ~(wyt by -.splits-b) ~(wyt by +.splits-b))
    %+  expect-eq
      !>  %.y
      !>  &(~(apt by -.splits-b) ~(apt by +.splits-b))
    =/  left   (~(get by -.splits-b) [6 12])
    =/  right  (~(get by +.splits-b) [6 12])
    %+  expect-eq
      !>  %.y
      !>  &(=(left ~) =(right ~))
  ==
::
::  Test delete at key b
::
++  test-map-del  ^-  tang
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  ~
      !>  (~(del by m-nul) 1)
    ::  Checks deleting non-existing element
    ::
    %+  expect-eq
      !>  m-des
      !>  (~(del by m-des) 99)
    ::  Checks deleting the only element
    ::
    %+  expect-eq
      !>  ~
      !>  (~(del by m-uno) 42)
    ::  Checks deleting one element
    ::
    %+  expect-eq
      !>  (map-of-doubles (limo ~[6 5 4 3 2 1]))
      !>  (~(del by m-des) 7)
  ==
::
::  Test difference (removes elements of a present in b)
::
++  test-map-dif  ^-  tang
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  ~
      !>  (~(dif by ~) ~)
    %+  expect-eq
      !>  m-dup
      !>  (~(dif by m-dup) m-nul)
    ::  Checks same elements, different ordering
    ::
    %+  expect-eq
      !>  ~
      !>  (~(dif by m-asc) m-des)
    ::  Checks different map length
    ::
    %+  expect-eq
      !>  (my ~[[7 14] [1 2] [4 8]])
      !>  (~(dif by m-dup) m-dos)
    ::  Checks no elements in common
    ::
    %+  expect-eq
      !>  m-uno
      !>  (~(dif by m-uno) m-dos)
  ==
::
::  Test axis of a in b
::
++  test-map-dig  ^-  tang
  =/  custom      [[2 4] [[1 2] ~ ~] [[3 6] ~ ~]]
  =/  custome-vase  !>(custom)
  =/  manual-map=(map @ @)  custom
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  ~
      !>  (~(dig by m-nul) 6)
    ::  Checks with non-existing key
    ::
    %+  expect-eq
      !>  ~
      !>  (~(dig by m-des) 9)
    ::  Checks success via tree addressing. Uses the return axis
    ::  to address the raw noun and check that it gives the corresponding
    ::  value from the key.
    ::
    %+  expect-eq
      !>  [1 (~(got by manual-map) 1)]
      !>  +:(slot (need (~(dig by manual-map) 1)) custome-vase)
    %+  expect-eq
      !>  [2 (~(got by manual-map) 2)]
      !>  +:(slot (need (~(dig by manual-map) 2)) custome-vase)
    %+  expect-eq
      !>  [3 (~(got by manual-map) 3)]
      !>  +:(slot (need (~(dig by manual-map) 3)) custome-vase)
  ==
::
::  Test concatenate
::
++  test-map-gas  ^-  tang
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  m-dos
      !>  (~(gas by m-nul) ~[[6 12] [9 18]])
    ::  Checks with > 1 element
    ::
    %+  expect-eq
      !>  (map-of-doubles (limo ~[42 10]))
      !>  (~(gas by m-uno) [10 20]~)
    ::  Checks appending >1 elements
    ::
    %+  expect-eq
      !>  (map-of-doubles (limo ~[6 9 3 4 5 7]))
      !>  (~(gas by m-dos) ~[[3 6] [4 8] [5 10] [7 14]])
    ::  Checks concatenating existing elements
    ::
    %+  expect-eq
      !>  m-des
      !>  (~(gas by m-des) ~[[3 6] [4 8] [5 10] [7 14]])
  ==
::
::  Test grab value by key
::
++  test-map-get  ^-  tang
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  ~
      !>  (~(get by m-nul) 6)
    ::  Checks with non-existing key
    ::
    %+  expect-eq
      !>  ~
      !>  (~(get by m-des) 9)
    ::  Checks success
    ::
    %+  expect-eq
      !>  `14
      !>  (~(get by m-des) 7)
  ==
::
::  Test need value by key
::
++  test-map-got  ^-  tang
  ;:  weld
    ::  Checks with empty map
    ::
    %-  expect-fail
      |.  (~(got by m-nul) 6)
    ::  Checks with non-existing key
    ::
    %-  expect-fail
      |.  (~(got by m-des) 9)
    ::  Checks success
    ::
    %+  expect-eq
      !>  14
      !>  (~(got by m-des) 7)
  ==
::
::  Test fall value by key
::
++  test-map-gut  ^-  tang
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  42
      !>  (~(gut by m-nul) 6 42)
    ::  Checks with non-existing key
    ::
    %+  expect-eq
      !>  42
      !>  (~(gut by m-des) 9 42)
    ::  Checks success
    ::
    %+  expect-eq
      !>  14
      !>  (~(gut by m-des) 7 42)
  ==
::
::  Test +has: does :b exist in :a?
::
++  test-map-has  ^-  tang
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  %.n
      !>  (~(has by m-nul) 6)
    ::  Checks with non-existing key
    ::
    %+  expect-eq
      !>  %.n
      !>  (~(has by m-des) 9)
    ::  Checks success
    ::
    %+  expect-eq
      !>  %.y
      !>  (~(has by m-des) 7)
  ==
::
::  Test intersection
::
++  test-map-int  ^-  tang
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  ~
      !>  (~(int by m-nul) m-des)
    %+  expect-eq
      !>  ~
      !>  (~(int by m-des) m-nul)
    ::  Checks with all keys different
    ::
    %+  expect-eq
      !>  ~
      !>  (~(int by m-dos) m-uno)
    ::  Checks success (total intersection)
    ::
    %+  expect-eq
      !>  m-asc
      !>  (~(int by m-asc) m-des)
    ::  Checks success (partial intersection)
    ::
    %+  expect-eq
      !>  (map-of-doubles (limo ~[1 7 4 6]))
      !>  (~(int by m-des) m-dup)
    ::  Checks replacing value from b
    ::
    %+  expect-eq
      !>  (my [6 99]~)
      !>  (~(int by m-dos) (my [6 99]~))
  ==
::
::  Test search for a specific key and modifies
::  its value with the result of the provided gate
::
++  test-map-jab  ^-  tang
  ;:  weld
    ::  Checks with empty map
    ::
    %-  expect-fail
      |.  (~(jab by m-nul) 2 dec)
    ::  Checks success, by modifying
    ::  [2 4] to [2 3]
    ::
    %+  expect-eq
      !>  (my ~[[1 2] [2 3] [3 6] [4 8] [5 10] [6 12] [7 14]])
      !>  (~(jab by m-asc) 2 dec)
  ==
::
::  Test produce set of keys
::
++  test-map-key  ^-  tang
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  ~
      !>  ~(key by m-nul)
    ::  Checks when creating a map from a list with duplicates
    ::
    %+  expect-eq
      !>  (sy ~[1 1 7 4 6 9 4])
      !>  ~(key by m-dup)
    ::  Checks correctness
    ::
    %+  expect-eq
      !>  (sy ~[1 2 3 4 5 6 7])
      !>  ~(key by m-des)
  ==
::
::  Test add key-value pair with validation (the value is a nonempty unit)
::
++  test-map-mar  ^-  tang
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  (my [6 12]~)
      !>  (~(mar by m-nul) 6 `12)
    ::  Checks with empty value (deletes the key)
    ::
    %+  expect-eq
      !>  (~(del by m-des) 6)
      !>  (~(mar by m-des) 6 ~)
    ::  Checks success (when key exists)
    ::
    %+  expect-eq
      !>  (my ~[[6 12] [9 99]])
      !>  (~(mar by m-dos) 9 `99)
    ::  Checks success (when key does not exist)
    ::
    %+  expect-eq
      !>  (~(put by m-des) [90 23])
      !>  (~(mar by m-des) 90 `23)
  ==
::
::  Test add key-value pair
::
++  test-map-put  ^-  tang
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  (my [6 12]~)
      !>  (~(put by m-nul) 6 12)
    ::  Checks with existing key
    ::
    %+  expect-eq
      !>  (my ~[[6 99] [9 18]])
      !>  (~(put by m-dos) 6 99)
    ::  Checks success (new key)
    ::
    %+  expect-eq
      !>  (my ~[[42 84] [9 99]])
      !>  (~(put by m-uno) 9 99)
  ==
::
::  Test replace by product
::
++  test-map-rep  ^-  tang
  ::  Accumulates differences between keys and values
  ::
  =/  rep-gate  |=([a=[@ @] b=@] (add b (sub +.a -.a)))
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  b=0
      !>  (~(rep by m-nul) rep-gate)
    ::  Checks success
    ::
    %+  expect-eq
      ::  m-asc => {[5 10] [7 14] [6 12] [1 2] [2 4] [3 6] [4 8]}
      ::  acc   => 12-6+10-5+14-7+8-4+6-3+4-2+2-1 => 28
      !>  b=28
      !>  (~(rep by m-asc) rep-gate)
  ==
::
::  Test Test transform + product
::
++  test-map-rib  ^-  tang
  ::  Accumulates multiples in an array and drains the pairs
  ::  whose values are double of their keys.
  ::
  =/  rib-gate
    |=  [a=[@ @] acc=(list @)]
    :-  (weld acc ~[(div +.a -.a)])
    ?:  =(2 (div +.a -.a))
      [-.a 0]
    a
  =/  list-of-2s  (reap 7 2)
  =/  zeroed-map  (my ~[[1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [7 0]])
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  [~ ~]
      !>  (~(rib by m-nul) *(list @) rib-gate)
    ::  Checks success
    ::
    %+  expect-eq
      !>  [list-of-2s zeroed-map]
      !>  (~(rib by m-asc) *(list @) rib-gate)
  ==
::
::  Test apply gate to values
::
++  test-map-run  ^-  tang
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  ~
      !>  (~(run by m-nul) dec)
    ::  Checks success
    ::
    %+  expect-eq
      !>  (my ~[[1 1] [2 3] [3 5] [4 7] [5 9] [6 11] [7 13]])
      !>  (~(run by m-asc) dec)
  ==
::
::  Test apply gate to nodes
::
++  test-map-rut  ^-  tang
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  ~
      !>  (~(rut by m-nul) add)
    ::  Checks success
    ::
    %+  expect-eq
      !>  (my ~[[1 3] [2 6] [3 9] [4 12] [5 15] [6 18] [7 21]])
      !>  (~(rut by m-asc) add)
  ==
::
::  Test listify pairs
::
++  test-map-tap  ^-  tang
  =/  by-key  |=([[k=@ v=@] [q=@ w=@]] (gth k q))
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  ~
      !>  ~(tap by ~)
    ::  Checks success with 2 pairs
    ::
    %+  expect-eq
      !>  (sort ~[[9 18] [6 12]] by-key)
      !>  (sort ~(tap by m-dos) by-key)
    ::  Checks success with 7 pairs
    ::
    %+  expect-eq
      !>  (sort ~[[1 2] [2 4] [3 6] [4 8] [5 10] [7 14] [6 12]] by-key)
      !>  (sort ~(tap by m-asc) by-key)
    ::  Checks success with 5 pairs (from list with duplicates)
    ::
    %+  expect-eq
      !>  (sort ~[[7 14] [6 12] [9 18] [1 2] [4 8]] by-key)
      !>  (sort ~(tap by m-dup) by-key)
  ==
::
::  Test the union of maps
::
++  test-map-uni  ^-  tang
  ;:  weld
    ::  Checks with empty map (a or b)
    ::
    %+  expect-eq
      !>  m-des
      !>  (~(uni by m-nul) m-des)
    %+  expect-eq
      !>  m-des
      !>  (~(uni by m-des) m-nul)
    ::  Checks with disjoint keys
    ::
    =/  keys  (limo ~[1 2 3 4 5 6 7 8])
    =/  a=(map @ @)  (map-of-doubles (scag 4 keys))
    =/  b=(map @ @)  (map-of-doubles (slag 4 keys))
    %+  expect-eq
      !>  (map-of-doubles keys)
      !>  (~(uni by a) b)
    ::  Checks union of sets with all keys equal
    ::
    %+  expect-eq
      !>  m-asc
      !>  (~(uni by m-asc) m-des)
    ::  Checks union with value replacement from b
    ::
    =/  c=(map @ @)  (my [1 12]~)
    =/  d=(map @ @)  (my [1 24]~)
    %+  expect-eq
      !>  d
      !>  (~(uni by c) d)
  ==
::
::  Test general union
::
++  test-map-uno  ^-  tang
  =/  union-gate  |=([k=@ v=@ w=@] (add v w))
  ;:  weld
    ::  +uno:by arm test
    ::
    ::  Checks with empty map (a or b)
    ::
    %-  expect-fail
      |.  ((~(uno by m-nul) m-des) union-gate)
    %+  expect-eq
      !>  m-des
      !>  ((~(uno by m-des) m-nul) union-gate)
    ::  Checks with all keys different
    ::
    =/  keys  (limo ~[1 2 3 4 5 6 7 8])
    =/  a=(map @ @)  (map-of-doubles (scag 4 keys))
    =/  b=(map @ @)  (map-of-doubles (slag 4 keys))
    %+  expect-eq
      !>  (map-of-doubles keys)
      !>  ((~(uno by a) b) union-gate)
    ::  Checks total union
    ::
    %+  expect-eq
      !>  (my ~[[1 4] [2 8] [3 12] [4 16] [5 20] [6 24] [7 28]])
      !>  ((~(uno by m-asc) m-des) union-gate)
    ::  Checks partial union
    ::
    =/  a=(map @ @)  (my ~[[1 9] [7 3] [8 5]])
    =/  b=(map @ @)  (my ~[[1 2] [7 2]])
    %+  expect-eq
      !>  (my ~[[1 11] [7 5] [8 5]])
      !>  ((~(uno by a) b) union-gate)
  ==
::
::  Test apply gate to nodes (duplicates +rut)
::
++  test-map-urn  ^-  tang
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  ~
      !>  (~(urn by m-nul) add)
    ::  Checks success
    ::
    %+  expect-eq
      !>  (my ~[[1 3] [2 6] [3 9] [4 12] [5 15] [6 18] [7 21]])
      !>  (~(urn by m-asc) add)
  ==
::
::  Test produce list of vals
::
++  test-map-val  ^-  tang
  =/  double  |=(e=@ (mul 2 e))
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  ~
      !>  ~(val by m-nul)
    ::  Checks when creating a set from a list with duplicates
    ::
    =/  a=(list @)  ~(tap in (sy ~[1 1 7 4 6 9 4]))
    %+  expect-eq
      !>  (sort (turn a double) gth)
      !>  (sort ~(val by m-dup) gth)
    ::  Checks success
    ::
    =/  b=(list @)  ~(tap in (sy (gulf 1 7)))
    %+  expect-eq
      !>  (sort (turn b double) gth)
      !>  (sort ~(val by m-asc) gth)
  ==
::
::  Tests the size of map
::
++  test-map-wyt  ^-  tang
  ::  Runs all the tests in the suite
  ::
  =/  sizes=(list @)
    %+  turn  m-lis
      |=(m=(map @ @) ~(wyt by m))
  %+  expect-eq
    !>  sizes
    !>  (limo ~[0 1 2 2 7 7 7 5])
--
