/+  new-hoon, tester
=,  ls:new-hoon
|_  _tester:tester
++  test-head
  (expect-eq !>([1 (head [1 ~])]))
::
++  test-last
  (expect-eq !>([2 (last:ls [1 2 ~])]))
::
++  test-tail
  (expect-eq !>([[2 3 ~] (tail [1 2 3 ~])]))
::
++  test-init
  (expect-eq !>([[1 2 ~] (init [1 2 3 ~])]))
::
++  test-size
  (expect-eq !>([3 (size ['a' 'b' 'c' ~])]))
::
++  test-map
  (expect-eq !>([[2 3 ~] (map:ls [1 2 ~] |=(a/@ (add 1 a)))]))
::
++  test-reverse
  (expect-eq !>([[3 2 1 ~] (reverse [1 2 3 ~])]))
::
++  test-intersperse
  (expect-eq !>([[5 1 5 1 5 ~] (intersperse 1 [5 5 5 ~])]))
::
++  test-intercalate
  %-  expect-eq  !>
  :-  ["one,two,three"]
  (intercalate "," ["one" "two" "three" ~])
::
++  test-transpose
  %-  expect-eq  !>
  :-  ~[~[1 4] ~[2 5] ~[3 6]]
  (transpose ~[~[1 2 3] ~[4 5 6]])
::
++  test-foldl
  (expect-eq !>([9 (foldl [1 2 3 ~] 3 |=({a/@ b/@} (add a b)))]))
::
++  test-foldr
  (expect-eq !>([7 (foldr [1 2 3 ~] 1 |=({a/@ b/@} (add a b)))]))
::
++  test-concat
  (expect-eq !>([~[1 2 3 4] (concat ~[~[1 2] ~[3 4]])]))
::
++  test-weld
  (expect-eq !>([~[1 2 3 "one" "two"] (weld:ls ~[1 2 3] ~["one" "two"])]))
::
++  test-any-true
  (expect-eq !>([%.y (any [1 2 3 ~] |=(a/@ =(a 2)))]))
::
++  test-any-false
  (expect-eq !>([%.n (any [1 2 3 ~] |=(a/@ =(a 8)))]))
::
++  test-all-true
  (expect-eq !>([%.y (all [1 1 1 ~] |=(a/@ =(a 1)))]))
::
++  test-all-false
  (expect-eq !>([%.n (all [1 3 1 ~] |=(a/@ =(a 1)))]))
::
++  test-scanl
  %-  expect-eq  !>
  :-  ~[0 1 3 6]
  (scanl ~[1 2 3] 0 |=({a/@ b/@} (add a b)))
::
++  test-scanl1
  %-  expect-eq  !>
  :-  ~[1 3 6]
  (scanl1 ~[1 2 3] |=({a/@ b/@} (add a b)))
::
++  test-scanr
  %-  expect-eq  !>
  :-  ~[6 5 3 0]
  (scanr ~[1 2 3] 0 |=({a/@ b/@} (add a b)))
::
++  test-scanr1
  %-  expect-eq  !>
  :-  ~[6 5 3]
  (scanr1 ~[1 2 3] |=({a/@ b/@} (add a b)))
::
++  test-map-foldl
  %-  expect-eq  !>
  :-  [7 ~[2 3 5]]
  (map-foldl ~[1 2 3] 1 |=({a/@ b/@} [(add a b) (add 1 a)]))
::
++  test-map-foldr
  %-  expect-eq  !>
  :-  [7 ~[7 5 2]]
  (map-foldr ~[1 2 3] 1 |=({a/@ b/@} [(add a b) (add 1 a)]))
::
++  test-unfoldr
  %-  expect-eq  !>
  :-  [5 4 3 2 1 ~]
  (unfoldr 5 |=(a/@ ?:(=(a 0) ~ `[a (dec a)])))
::
++  test-take
  %-  expect-eq  !>
  :-  [1 2 3 ~]
  (take 3 ~[1 2 3 4 5])
::
++  test-drop
  %-  expect-eq  !>
  :-  [4 5 ~]
  (drop:ls 3 ~[1 2 3 4 5])
::
++  test-split-at
  %-  expect-eq  !>
  :-  [[1 2 3 ~] [4 5 ~]]
  (split-at 3 ~[1 2 3 4 5])
::
++  test-take-while
  %-  expect-eq  !>
  :-  [1 2 ~]
  (take-while ~[1 2 3 4 5] |=(a/@ (lth a 3)))
::
++  test-drop-while
  %-  expect-eq  !>
  :-  [3 4 5 ~]
  (drop-while ~[1 2 3 4 5] |=(a/@ (lth a 3)))
::
++  test-drop-while-end
  %-  expect-eq  !>
  :-  [5 5 1 ~]
  (drop-while-end ~[5 5 1 5 5] |=(a/@ =(a 5)))
::
++  test-split-on
  %-  expect-eq  !>
  :-  [[1 2 ~] [3 4 1 2 3 4 ~]]
  (split-on ~[1 2 3 4 1 2 3 4] |=(a/@ (lth a 3)))
::
++  test-break
  %-  expect-eq  !>
  :-  [[1 2 3 ~] [4 1 2 3 4 ~]]
  (break ~[1 2 3 4 1 2 3 4] |=(a/@ (gth a 3)))
::
++  test-strip-prefix
  %-  expect-eq  !>
  :-  [~ "bar"]
  (strip-prefix "foo" "foobar")
::
++  test-inits
  %-  expect-eq  !>
  :-  ["a" "ab" "abc" ~]
  (inits "abc")
::
++  test-tails
  %-  expect-eq  !>
  :-  ["abc" "bc" "c" ~]
  (tails "abc")
::
++  test-is-prefix-of
  %-  expect-eq  !>
  :-  %.y
  (is-prefix-of "foo" "foobar")
::
++  test-is-suffix-of
  %-  expect-eq  !>
  :-  %.y
  (is-suffix-of "bar" "foobar")
::
++  test-is-infix-of
  %-  expect-eq  !>
  :-  %.y
  (is-infix-of "ob" "foobar")
::
++  test-elem
  %-  expect-eq  !>
  :-  %.y
  (elem 5 [1 2 3 4 5 ~])
::
++  test-lookup
  %-  expect-eq  !>
  :-  [~ 2]
  (lookup "two" [["one" 1] ["two" 2] ["three" 3] ~])
::
++  test-find
  %-  expect-eq  !>
  :-  [~ 5]
  (find:ls [3 2 1 5 1 2 3 ~] |=(a/@ (gth a 3)))
::
++  test-filter
  %-  expect-eq  !>
  :-  [1 1 1 ~]
  (filter [1 2 1 2 1 ~] |=(a/@ =(a 2)))
::
++  test-partition
  %-  expect-eq  !>
  :-  [[2 2 ~] [1 1 1 ~]]
  (partition [1 2 1 2 1 ~] |=(a/@ =(a 2)))
::
++  test-elem-index
  %-  expect-eq  !>
  :-  `1
  (elem-index 2 [1 2 3 4 ~])
::
++  test-elem-indices
  %-  expect-eq  !>
  :-  [1 3 ~]
  (elem-indices 2 [1 2 1 2 ~])
::
++  test-find-index
  %-  expect-eq  !>
  :-  `1
  (find-index [1 2 3 ~] |=(a/@ =(a 2)))
::
++  test-find-indices
  %-  expect-eq  !>
  :-  [1 3 ~]
  (find-indices [1 2 1 2 ~] |=(a/@ =(a 2)))
::
++  test-zip
  %-  expect-eq  !>
  :-  [[1 4 7 ~] [2 5 8 ~] [3 6 9 ~] ~]
  (zip [[1 2 3 ~] [4 5 6 ~] [7 8 9 ~] ~])
::
++  test-unique
  %-  expect-eq  !>
  :-  [1 2 3 ~]
  (unique [1 2 3 1 2 3 ~])
::
++  test-delete
  %-  expect-eq  !>
  :-  [1 3 2 ~]
  (delete 2 [1 2 3 2 ~])
::
++  test-delete-firsts
  %-  expect-eq  !>
  :-  [1 2 3 4 ~]
  (delete-firsts [1 2 2 2 3 4 5 ~] [2 2 5 ~])
::
++  test-union
  %-  expect-eq  !>
  :-  [1 2 3 4 5 ~]
  (union [1 2 3 ~] [4 2 5 ~])
::
++  test-intersect
  %-  expect-eq  !>
  :-  [6 6 8 ~]
  (intersect [5 6 6 7 8 ~] [9 8 8 6 ~])
--

