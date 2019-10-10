::  Tests for +in (set logic)
::
/+  *test
::
::  Testing arms
::
|%
::  Test logical AND
::
++  test-set-all  ^-  tang
  =/  s-asc=(set @)   (sy (gulf 1 7))
  ;:  weld
    ::  Checks with empty set
    ::
    %+  expect-eq
      !>  %.y
      !>  (~(all in ~) |=(* &))
    %+  expect-eq
      !>  %.y
      !>  (~(all in ~) |=(* |))
    ::  Checks one element fails
    ::
    %+  expect-eq
      !>  %.n
      !>  (~(all in (sy ~[1])) |=(e=@ =(e 43)))
    ::  Checks not all elements pass
    ::
    %+  expect-eq
      !>  %.n
      !>  (~(all in s-asc) |=(e=@ (lth e 4)))
    ::  Checks all element pass
    ::
    %+  expect-eq
      !>  %.y
      !>  (~(all in s-asc) |=(e=@ (lth e 100)))
  ==
::
::  Test logical OR
::
++  test-set-any  ^-  tang
  =/  s-asc=(set @)   (sy (gulf 1 7))
  ;:  weld
    ::  Checks with empty set
    ::
    %+  expect-eq
      !>  %.n
      !>  (~(any in ~) |=(* &))
    %+  expect-eq
      !>  %.n
      !>  (~(any in ~) |=(* |))
    ::  Checks one element fails
    ::
    %+  expect-eq
      !>  %.n
      !>  (~(any in (sy ~[1])) |=(e=@ =(e 43)))
    ::  Checks >1 element success
    ::
    %+  expect-eq
      !>  %.y
      !>  (~(any in s-asc) |=(e=@ (lth e 4)))
    ::  Checks all element success
    ::
    %+  expect-eq
      !>  %.y
      !>  (~(any in s-asc) |=(e=@ (lth e 100)))
  ==
::
::  Test check correctness
::
++  test-set-apt  ^-  tang
  ::  Manually constructed sets with predefined vertical/horizontal
  ::  ordering
  ::
  ::  for the following three elements (1, 2, 3) the vertical priorities are:
  ::    > (mug (mug 1))
  ::    1.405.103.437
  ::    > (mug (mug 2))
  ::    1.200.431.393
  ::    > (mug (mug 3))
  ::    1.576.941.407
  ::
  ::  and the ordering 2 < 1 < 3
  ::  a correctly balanced tree stored as a min-heap
  ::  should have node=2 as the root
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
  ::  1 should be in the left brach and 3 in the right one.
  ::
  =/  balanced-a=(set @)    [2 [1 ~ ~] [3 ~ ~]]
  ::  Doesn't follow vertical ordering
  ::
  =/  unbalanced-a=(set @)  [1 [2 ~ ~] [3 ~ ~]]
  =/  unbalanced-b=(set @)  [1 ~ [2 ~ ~]]
  =/  unbalanced-c=(set @)  [1 [2 ~ ~] ~]
  ::  Doesn't follow horizontal ordering
  ::
  =/  unbalanced-d=(set @)  [2 [3 ~ ~] [1 ~ ~]]
  ::  Doesn't follow horizontal & vertical ordering
  ::
  =/  unbalanced-e=(set @)  [1 [3 ~ ~] [2 ~ ~]]
  ;:  weld
    %+  expect-eq
      !>  [%b-a %.y]
      !>  [%b-a ~(apt in balanced-a)]
    %+  expect-eq
      !>  [%u-a %.n]
      !>  [%u-a ~(apt in unbalanced-a)]
    %+  expect-eq
      !>  [%u-b %.n]
      !>  [%u-b ~(apt in unbalanced-b)]
    %+  expect-eq
      !>  [%u-c %.n]
      !>  [%u-c ~(apt in unbalanced-c)]
    %+  expect-eq
      !>  [%u-d %.n]
      !>  [%u-d ~(apt in unbalanced-d)]
    %+  expect-eq
      !>  [%u-e %.n]
      !>  [%u-e ~(apt in unbalanced-e)]
  ==
::
::  Test splits a in b
::
++  test-set-bif  ^-  tang
  =/  s-asc=(set @)   (sy (gulf 1 7))
  =/  s-nul=(set @)   *(set @)
  =/  splits-a=[(set) (set)]  (~(bif in s-asc) 99)
  =/  splits-b=[(set) (set)]  (~(bif in s-asc) 6)
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  [~ ~]
      !>  (~(bif in s-nul) 1)
    ::  Checks bifurcating in non-existing element
    ::
    ::  The traversal of the +map is done comparing the double +mug
    ::  of the added node and the existing one from the tree.
    ::  Because of this, the search will stop at different leaves,
    ::  based on the value of the hash, therefore the right and left
    ::  maps that are returned can be different
    ::  (null or a less than the total number of nodes)
    ::  The best way to check is that the sum of the number of nodes
    ::  in both maps are the same as before, and that both returned
    ::  sets are correct
    ::
    %+  expect-eq
      !>  7
      !>  (add ~(wyt in -.splits-a) ~(wyt in +.splits-a))
    %+  expect-eq
      !>  %.y
      !>  &(~(apt in -.splits-a) ~(apt in +.splits-a))
    ::  Checks splitting in existing element
    ::
    %+  expect-eq
      !>  6
      !>  (add ~(wyt in -.splits-b) ~(wyt in +.splits-b))
    %+  expect-eq
      !>  %.y
      !>  &(~(apt in -.splits-b) ~(apt in +.splits-b))
    =/  left   (~(has in -.splits-b) 6)
    =/  right  (~(has in +.splits-b) 6)
    %+  expect-eq
      !>  %.n
      !>  &(left right)
  ==
::
:: Test b without any a
::
++  test-set-del  ^-  tang
  =/  s-asc=(set @)   (sy (gulf 1 7))
  ;:  weld
    ::  Checks with empty set
    ::
    %+  expect-eq
      !>  ~
      !>  (~(del in ~) 1)
    ::  Checks deleting non-existing element
    ::
    %+  expect-eq
      !>  s-asc
      !>  (~(del in s-asc) 99)
    ::  Checks deleting the only element
    ::
    %+  expect-eq
      !>  ~
      !>  (~(del in (sy ~[1])) 1)
    ::  Checks deleting one element
    ::
    %+  expect-eq
      !>  (sy (gulf 1 6))
      !>  (~(del in s-asc) 7)
  ==
::
::  Test difference
::
++  test-set-dif  ^-  tang
  =/  s-des=(set @)   (sy (flop (gulf 1 7)))
  =/  s-asc=(set @)   (sy (gulf 1 7))
  =/  s-dos=(set @)   (sy ~[8 9])
  ;:  weld
    ::  Checks with empty set
    ::
    %+  expect-eq
      !>  ~
      !>  (~(dif in *(set)) ~)
    %+  expect-eq
      !>  s-asc
      !>  (~(dif in s-asc) ~)
    ::  Checks with equal sets
    ::
    %+  expect-eq
      !>  ~
      !>  (~(dif in s-asc) s-des)
    ::  Checks no elements in common
    ::
    %+  expect-eq
      !>  s-dos
      !>  (~(dif in s-dos) s-asc)
    ::  Checks with sets of diferent size
    ::
    %+  expect-eq
      !>  s-dos
      !>  (~(dif in (sy ~[1 8 9])) s-asc)
  ==
::
::  Test axis of a in b
::
++  test-set-dig  ^-  tang
  =/  custom  [2 [1 ~ ~] [3 ~ ~]]
  =/  custom-vase  !>(custom)
  =/  manual-set=(set @)  custom
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  ~
      !>  (~(dig in *(set)) 6)
    ::  Checks with non-existing key
    ::
    %+  expect-eq
      !>  ~
      !>  (~(dig in manual-set) 9)
    ::  Checks success via tree addressing. It uses the returned axis
    ::  to address the raw noun and check that it gives the corresponding
    ::  value.
    ::
    %+  expect-eq
      !>  1
      !>  +:(slot (need (~(dig in manual-set) 1)) custom-vase)
    %+  expect-eq
      !>  2
      !>  +:(slot (need (~(dig in manual-set) 2)) custom-vase)
    %+  expect-eq
      !>  3
      !>  +:(slot (need (~(dig in manual-set) 3)) custom-vase)
  ==
::
::  Test concatenate
::
++  test-set-gas  ^-  tang
  ::  Uses +apt to check the correctness
  ::  of the sets created with +gas
  ::
  =+  |%
      +|  %test-suite
      ++  s-uno  (~(gas in *(set)) ~[42])
      ++  s-dos  (~(gas in *(set)) ~[6 9])
      ++  s-tre  (~(gas in *(set)) ~[1 0 1])
      ++  s-asc  (~(gas in *(set)) ~[1 2 3 4 5 6 7])
      ++  s-des  (~(gas in *(set)) ~[7 6 5 4 3 2 1])
      ++  s-uns  (~(gas in *(set)) ~[1 6 3 5 7 2 4])
      ++  s-dup  (~(gas in *(set)) ~[1 1 7 4 6 9 4])
      ++  s-nul  (~(gas in *(set)) ~)
      --
  =/  s-lis=(list (set))  ~[s-nul s-uno s-dos s-tre s-asc s-des s-uns s-dup]
  =/  actual=?
    %+  roll  s-lis
      |=  [s=(set) b=?]
      ^-  ?
      &(b ~(apt in s))
  ;:  weld
    ::  Checks with all tests in the suite
    ::
    %+  expect-eq
      !>  %.y
      !>  actual
    ::  Checks appending >1 elements
    ::
    %+  expect-eq
      !>  %.y
      !>  ~(apt in (~(gas in s-dos) ~[9 10]))
    ::  Checks concatenating existing elements
    ::
    %+  expect-eq
      !>  s-asc
      !>  (~(gas in s-asc) (gulf 1 3))
  ==
::
::  Test +has: does :b exist in :a?
::
++  test-set-has  ^-  tang
  =/  s-nul=(set @)  *(set @)
  =/  s-asc=(set @)  (sy (gulf 1 7))
  ;:  weld
    ::  Checks with empty set
    ::
    %+  expect-eq
      !>  %.n
      !>  (~(has in s-nul) 6)
    ::  Checks with non-existing key
    ::
    %+  expect-eq
      !>  %.n
      !>  (~(has in s-asc) 9)
    ::  Checks success
    ::
    %+  expect-eq
      !>  %.y
      !>  (~(has in s-asc) 7)
  ==
::
::  Test intersection
::
++  test-set-int  ^-  tang
  =/  s-nul=(set @)  *(set @)
  =/  s-asc=(set @)  (sy (gulf 1 7))
  =/  s-des=(set @)  (sy (flop (gulf 1 7)))
  =/  s-dos=(set @)  (sy (gulf 8 9))
  =/  s-dup  (sy ~[1 1 4 1 3 5 9 4])
  ;:  weld
    ::  Checks with empty set
    ::
    %+  expect-eq
      !>  ~
      !>  (~(int in s-nul) s-asc)
    %+  expect-eq
      !>  ~
      !>  (~(int in s-asc) s-nul)
    ::  Checks with all elements different
    ::
    %+  expect-eq
      !>  ~
      !>  (~(int in s-dos) s-asc)
    ::  Checks success (total intersection)
    ::
    %+  expect-eq
      !>  s-asc
      !>  (~(int in s-asc) s-des)
    ::  Checks success (partial intersection)
    ::
    %+  expect-eq
      !>  (sy ~[9])
      !>  (~(int in s-dos) s-dup)
  ==
::
::  Test puts b in a, sorted
::
++  test-set-put  ^-  tang
  =/  s-nul=(set @)  *(set @)
  =/  s-asc=(set @)  (sy (gulf 1 7))
  ;:  weld
    ::  Checks with empty set
    ::
    %+  expect-eq
      !>  (sy ~[6])
      !>  (~(put in s-nul) 6)
    ::  Checks with existing key
    ::
    %+  expect-eq
      !>  s-asc
      !>  (~(put in s-asc) 6)
    ::  Checks adding new element
    ::
    %+  expect-eq
      !>  (sy (gulf 1 8))
      !>  (~(put in s-asc) 8)
  ==
::  Test replace in product
::
++  test-set-rep  ^-  tang
  =/  s-nul=(set @)  *(set @)
  =/  s-asc=(set @)  (sy (gulf 1 7))
  ;:  weld
    ::  Checks with empty set
    ::
    %+  expect-eq
      !>  b=0
      !>  (~(rep in s-nul) add)
    ::  Checks success
    ::
    %+  expect-eq
      !>  b=28
      !>  (~(rep in s-asc) add)
  ==
::
::  Test apply gate to values
::
++  test-set-run  ^-  tang
  =/  s-nul  *(set @)
  =/  s-asc  (sy (gulf 1 7))
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  ~
      !>  (~(run in s-nul) dec)
    ::  Checks success
    ::
    %+  expect-eq
      !>  (sy (gulf 0 6))
      !>  (~(run in s-asc) dec)
  ==
::
::  Converts a set to list
::
++  test-set-tap  ^-  tang
  =/  s-dup  (sy ~[1 1 4 1 3 5 9 4])
  =/  s-asc  (sy (gulf 1 7))
  ;:  weld
    ::  Checks with empty map
    ::
    %+  expect-eq
      !>  ~
      !>  ~(tap in *(set @))
    ::  Checks with duplicates
    ::
    %+  expect-eq
      !>  (sort ~[1 4 3 5 9] gth)
      !>  (sort ~(tap in s-dup) gth)
    ::  Checks with ascending list
    ::
    %+  expect-eq
      !>  (gulf 1 7)
      !>  (sort ~(tap in s-asc) lth)
  ==
::
::  Test the union of sets
::
++  test-set-uni  ^-  tang
  =/  asc=(list @)    (gulf 1 7)
  =/  des=(list @)    (flop (gulf 1 7))
  =/  s-des=(set @)   (sy des)
  =/  s-asc=(set @)   (sy asc)
  =/  s-nul=(set @)   *(set @)
  ;:  weld
    ::  Checks with empty map (a or b)
    ::
    %+  expect-eq
      !>  s-des
      !>  (~(uni in s-nul) s-des)
    %+  expect-eq
      !>  s-des
      !>  (~(uni in s-des) s-nul)
    ::  Checks with no intersection
    ::
    =/  a=(set @)  (sy (scag 4 asc))
    =/  b=(set @)  (sy (slag 4 asc))
    %+  expect-eq
      !>  s-asc
      !>  (~(uni in a) b)
    ::  Checks union with equal sets
    ::
    %+  expect-eq
      !>  s-asc
      !>  (~(uni in s-asc) s-des)
    ::  Checks union with partial intersection
    ::
    %+  expect-eq
      !>  s-asc
      !>  (~(uni in s-asc) (sy (gulf 1 3)))
  ==
::
::  Tests the size of set
::
++  test-set-wyt  ^-  tang
  =+  |%
      ++  s-uno  (~(gas in *(set)) ~[42])
      ++  s-dos  (~(gas in *(set)) ~[6 9])
      ++  s-tre  (~(gas in *(set)) ~[1 0 1])
      ++  s-asc  (~(gas in *(set)) ~[1 2 3 4 5 6 7])
      ++  s-des  (~(gas in *(set)) ~[7 6 5 4 3 2 1])
      ++  s-uns  (~(gas in *(set)) ~[1 6 3 5 7 2 4])
      ++  s-dup  (~(gas in *(set)) ~[1 1 7 4 6 9 4])
      ++  s-nul  (~(gas in *(set)) ~)
      ++  s-lis  ~[s-nul s-uno s-dos s-tre s-asc s-des s-uns s-dup]
      --
  ::  Runs all the tests in the suite
  ::
  =/  sizes=(list @)
    %+  turn  s-lis
      |=(s=(set) ~(wyt in s))
  %+  expect-eq
    !>  sizes
    !>  (limo ~[0 1 2 2 7 7 7 5])
--
