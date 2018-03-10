/+  new-hoon, tester
=,  ls:new-hoon
|_  _tester:tester
++  test-head
  (expect-eq (head [1 ~]) 1)
::
++  test-last
  (expect-eq (last:ls [1 2 ~]) 2)
::
++  test-tail
  (expect-eq (tail [1 2 3 ~]) [2 3 ~])
::
++  test-init
  (expect-eq (init [1 2 3 ~]) [1 2 ~])
::
++  test-size
  (expect-eq (size ['a' 'b' 'c' ~]) 3)
::
++  test-map
  (expect-eq (map:ls [1 2 ~] |=(a/@ (add 1 a))) [2 3 ~])
::
++  test-reverse
  (expect-eq (reverse [1 2 3 ~]) [3 2 1 ~])
::
++  test-intersperse
  (expect-eq (intersperse 1 [5 5 5 ~]) [5 1 5 1 5 ~])
::
++  test-intercalate
  %+    expect-eq
    (intercalate "," ["one" "two" "three" ~])
  ["one,two,three"]
::
++  test-transpose
  %+  expect-eq
    (transpose ~[~[1 2 3] ~[4 5 6]])
  ~[~[1 4] ~[2 5] ~[3 6]]
::
++  test-foldl
  (expect-eq (foldl [1 2 3 ~] 3 |=({a/@ b/@} (add a b))) 9)
::
++  test-foldr
  (expect-eq (foldr [1 2 3 ~] 1 |=({a/@ b/@} (add a b))) 7)
::
++  test-concat
  (expect-eq (concat ~[~[1 2] ~[3 4]]) ~[1 2 3 4])
::
++  test-weld
  (expect-eq (weld:ls ~[1 2 3] ~["one" "two"]) ~[1 2 3 "one" "two"])
::
++  test-any-true
  (expect-eq (any [1 2 3 ~] |=(a/@ =(a 2))) %.y)
::
++  test-any-false
  (expect-eq (any [1 2 3 ~] |=(a/@ =(a 8))) %.n)
::
++  test-all-true
  (expect-eq (all [1 1 1 ~] |=(a/@ =(a 1))) %.y)
::
++  test-all-false
  (expect-eq (all [1 3 1 ~] |=(a/@ =(a 1))) %.n)
::
++  test-scanl
  %+  expect-eq
    (scanl ~[1 2 3] 0 |=({a/@ b/@} (add a b)))
  ~[0 1 3 6]
::
++  test-scanl1
  %+  expect-eq
    (scanl1 ~[1 2 3] |=({a/@ b/@} (add a b)))
  ~[1 3 6]
::
++  test-scanr
  %+  expect-eq
    (scanr ~[1 2 3] 0 |=({a/@ b/@} (add a b)))
  ~[6 5 3 0]
::
++  test-scanr1
  %+  expect-eq
    (scanr1 ~[1 2 3] |=({a/@ b/@} (add a b)))
  ~[6 5 3]
::
++  test-map-foldl
  %+  expect-eq
    (map-foldl ~[1 2 3] 1 |=({a/@ b/@} [(add a b) (add 1 a)]))
  [7 ~[2 3 5]]
::
++  test-map-foldr
  %+  expect-eq
    (map-foldr ~[1 2 3] 1 |=({a/@ b/@} [(add a b) (add 1 a)]))
  [7 ~[7 5 2]]
::
++  test-unfoldr
  %+  expect-eq
    (unfoldr 5 |=(a/@ ?:(=(a 0) ~ `[a (dec a)])))
  [5 4 3 2 1 ~]
::
++  test-take
  %+  expect-eq
    (take 3 ~[1 2 3 4 5])
  [1 2 3 ~]
::
++  test-drop
  %+  expect-eq
    (drop:ls 3 ~[1 2 3 4 5])
  [4 5 ~]
::
++  test-split-at
  %+  expect-eq
    (split-at 3 ~[1 2 3 4 5])
  [[1 2 3 ~] [4 5 ~]]
::
++  test-take-while
  %+  expect-eq
    (take-while ~[1 2 3 4 5] |=(a/@ (lth a 3)))
  [1 2 ~]
::
++  test-drop-while
  %+  expect-eq
    (drop-while ~[1 2 3 4 5] |=(a/@ (lth a 3)))
  [3 4 5 ~]
::
++  test-drop-while-end
  %+  expect-eq
    (drop-while-end ~[5 5 1 5 5] |=(a/@ =(a 5)))
  [5 5 1 ~]
::
++  test-split-on
  %+  expect-eq
    (split-on ~[1 2 3 4 1 2 3 4] |=(a/@ (lth a 3)))
  [[1 2 ~] [3 4 1 2 3 4 ~]]
::
++  test-break
  %+  expect-eq
    (break ~[1 2 3 4 1 2 3 4] |=(a/@ (gth a 3)))
  [[1 2 3 ~] [4 1 2 3 4 ~]]
::
++  test-strip-prefix
  %+  expect-eq
    (strip-prefix "foo" "foobar")
  [~ "bar"]
::
++  test-inits
  %+  expect-eq
    (inits "abc")
  ["a" "ab" "abc" ~]
::
++  test-tails
  %+  expect-eq
    (tails "abc")
  ["abc" "bc" "c" ~]
::
++  test-is-prefix-of
  %+  expect-eq
    (is-prefix-of "foo" "foobar")
  %.y
::
++  test-is-suffix-of
  %+  expect-eq
    (is-suffix-of "bar" "foobar")
  %.y
::
++  test-is-infix-of
  %+  expect-eq
    (is-infix-of "ob" "foobar")
  %.y
::
++  test-elem
  %+  expect-eq
    (elem 5 [1 2 3 4 5 ~])
  %.y
::
++  test-lookup
  %+  expect-eq
    (lookup "two" [["one" 1] ["two" 2] ["three" 3] ~])
  [~ 2]
::
++  test-find
  %+  expect-eq
    (find:ls [3 2 1 5 1 2 3 ~] |=(a/@ (gth a 3)))
  [~ 5]
::
++  test-filter
  %+  expect-eq
    (filter [1 2 1 2 1 ~] |=(a/@ =(a 2)))
  [1 1 1 ~]
::
++  test-partition
  %+  expect-eq
    (partition [1 2 1 2 1 ~] |=(a/@ =(a 2)))
  [[2 2 ~] [1 1 1 ~]]
::
++  test-elem-index
  %+  expect-eq
    (elem-index 2 [1 2 3 4 ~])
  `1
::
++  test-elem-indices
  %+  expect-eq
    (elem-indices 2 [1 2 1 2 ~])
  [1 3 ~]
::
++  test-find-index
  %+  expect-eq
    (find-index [1 2 3 ~] |=(a/@ =(a 2)))
  `1
::
++  test-find-indices
  %+  expect-eq
    (find-indices [1 2 1 2 ~] |=(a/@ =(a 2)))
  [1 3 ~]
::
++  test-zip
  %+  expect-eq
    (zip [[1 2 3 ~] [4 5 6 ~] [7 8 9 ~] ~])
  [[1 4 7 ~] [2 5 8 ~] [3 6 9 ~] ~]
::
++  test-unique
  %+  expect-eq
    (unique [1 2 3 1 2 3 ~])
  [1 2 3 ~]
::
++  test-delete
  %+  expect-eq
    (delete 2 [1 2 3 2 ~])
  [1 3 2 ~]
::
++  test-delete-firsts
  %+  expect-eq
    (delete-firsts [1 2 2 2 3 4 5 ~] [2 2 5 ~])
  [1 2 3 4 ~]
::
++  test-union
  %+  expect-eq
    (union [1 2 3 ~] [4 2 5 ~])
  [1 2 3 4 5 ~]
::
++  test-intersect
  %+  expect-eq
    (intersect [5 6 6 7 8 ~] [9 8 8 6 ~])
  [6 6 8 ~]
--

