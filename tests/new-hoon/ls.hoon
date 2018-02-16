/+  new-hoon, tester
=,  ls:new-hoon
|_  tester-type:tester
++  test-head
  (expect-eq (head [1 ~]) 1 "head")
::
++  test-last
  (expect-eq (last:ls [1 2 ~]) 2 "last")
::
++  test-tail
  (expect-eq (tail [1 2 3 ~]) [2 3 ~] "tail")
::
++  test-init
  (expect-eq (init [1 2 3 ~]) [1 2 ~] "init")
::
++  test-size
  (expect-eq (size ['a' 'b' 'c' ~]) 3 "size")
::
++  test-map
  (expect-eq (map:ls [1 2 ~] |=(a/@ (add 1 a))) [2 3 ~] "map")
::
++  test-reverse
  (expect-eq (reverse [1 2 3 ~]) [3 2 1 ~] "reverse")
::
++  test-intersperse
  (expect-eq (intersperse 1 [5 5 5 ~]) [5 1 5 1 5 ~] "intersperse")
::
++  test-intercalate
  %^  expect-eq
  (intercalate "," ["one" "two" "three" ~])
  ["one,two,three"]
  "intercalate"
::
++  test-transpose
  %^  expect-eq
  (transpose ~[~[1 2 3] ~[4 5 6]])
  ~[~[1 4] ~[2 5] ~[3 6]]
  "transpose"
::
++  test-foldl
  (expect-eq (foldl [1 2 3 ~] 3 |=({a/@ b/@} (add a b))) 9 "foldl")
::
++  test-foldr
  (expect-eq (foldr [1 2 3 ~] 1 |=({a/@ b/@} (add a b))) 7 "foldr")
::
++  test-concat
  (expect-eq (concat ~[~[1 2] ~[3 4]]) ~[1 2 3 4] "concat")
::
++  test-weld
  (expect-eq (weld:ls ~[1 2 3] ~["one" "two"]) ~[1 2 3 "one" "two"] "weld")
::
++  test-any-true
  (expect-eq (any [1 2 3 ~] |=(a/@ =(a 2))) %.y "any true")
::
++  test-any-false
  (expect-eq (any [1 2 3 ~] |=(a/@ =(a 8))) %.n "any false")
::
++  test-all-true
  (expect-eq (all [1 1 1 ~] |=(a/@ =(a 1))) %.y "all true")
::
++  test-all-false
  (expect-eq (all [1 3 1 ~] |=(a/@ =(a 1))) %.n "all false")
::
++  test-scanl
  %^  expect-eq
  (scanl ~[1 2 3] 0 |=({a/@ b/@} (add a b)))
  ~[0 1 3 6]
  "scanl"
::
++  test-scanl1
  %^  expect-eq
  (scanl1 ~[1 2 3] |=({a/@ b/@} (add a b)))
  ~[1 3 6]
  "scanl1"
::
++  test-scanr
  %^  expect-eq
  (scanr ~[1 2 3] 0 |=({a/@ b/@} (add a b)))
  ~[6 5 3 0]
  "scanr"
::
++  test-scanr1
  %^  expect-eq
  (scanr1 ~[1 2 3] |=({a/@ b/@} (add a b)))
  ~[6 5 3]
  "scanr1"
::
++  test-map-foldl
  %^  expect-eq
  (map-foldl ~[1 2 3] 1 |=({a/@ b/@} [(add a b) (add 1 a)]))
  [7 ~[2 3 5]]
  "map-foldl"
::
++  test-map-foldr
  %^  expect-eq
  (map-foldr ~[1 2 3] 1 |=({a/@ b/@} [(add a b) (add 1 a)]))
  [7 ~[7 5 2]]
  "map-foldr"
::
++  test-unfoldr
  %^  expect-eq
  (unfoldr 5 |=(a/@ ?:(=(a 0) ~ `[a (dec a)])))
  [5 4 3 2 1 ~]
  "unfoldr"
::
++  test-take
  %^  expect-eq
  (take 3 ~[1 2 3 4 5])
  [1 2 3 ~]
  "take"
::
++  test-drop
  %^  expect-eq
  (drop:ls 3 ~[1 2 3 4 5])
  [4 5 ~]
  "drop"
::
++  test-split-at
  %^  expect-eq
  (split-at 3 ~[1 2 3 4 5])
  [[1 2 3 ~] [4 5 ~]]
  "split-at"
::
++  test-take-while
  %^  expect-eq
  (take-while ~[1 2 3 4 5] |=(a/@ (lth a 3)))
  [1 2 ~]
  "take-while"
::
++  test-drop-while
  %^  expect-eq
  (drop-while ~[1 2 3 4 5] |=(a/@ (lth a 3)))
  [3 4 5 ~]
  "drop-while"
::
++  test-drop-while-end
  %^  expect-eq
  (drop-while-end ~[5 5 1 5 5] |=(a/@ =(a 5)))
  [5 5 1 ~]
  "drop-while-end"
::
++  test-split-on
  %^  expect-eq
  (split-on ~[1 2 3 4 1 2 3 4] |=(a/@ (lth a 3)))
  [[1 2 ~] [3 4 1 2 3 4 ~]]
  "split-on"
::
++  test-break
  %^  expect-eq
  (break ~[1 2 3 4 1 2 3 4] |=(a/@ (gth a 3)))
  [[1 2 3 ~] [4 1 2 3 4 ~]]
  "break"
::
++  test-strip-prefix
  %^  expect-eq
  (strip-prefix "foo" "foobar")
  [~ "bar"]
  "break"
::
++  test-inits
  %^  expect-eq
  (inits "abc")
  ["a" "ab" "abc" ~]
  "inits"
::
++  test-tails
  %^  expect-eq
  (tails "abc")
  ["abc" "bc" "c" ~]
  "tails"
::
++  test-is-prefix-of
  %^  expect-eq
  (is-prefix-of "foo" "foobar")
  %.y
  "is-prefix-of"
::
++  test-is-suffix-of
  %^  expect-eq
  (is-suffix-of "bar" "foobar")
  %.y
  "is-suffix-of"
::
++  test-is-infix-of
  %^  expect-eq
  (is-infix-of "ob" "foobar")
  %.y
  "is-infix-of"
::
++  test-elem
  %^  expect-eq
  (elem 5 [1 2 3 4 5 ~])
  %.y
  "elem"
::
++  test-lookup
  %^  expect-eq
  (lookup "two" [["one" 1] ["two" 2] ["three" 3] ~])
  [~ 2]
  "lookup"
::
++  test-find
  %^  expect-eq
  (find:ls [3 2 1 5 1 2 3 ~] |=(a/@ (gth a 3)))
  [~ 5]
  "find"
::
++  test-filter
  %^  expect-eq
  (filter [1 2 1 2 1 ~] |=(a/@ =(a 2)))
  [1 1 1 ~]
  "filter"
::
++  test-partition
  %^  expect-eq
  (partition [1 2 1 2 1 ~] |=(a/@ =(a 2)))
  [[2 2 ~] [1 1 1 ~]]
  "partition"
::
++  test-elem-index
  %^  expect-eq
  (elem-index 2 [1 2 3 4 ~])
  `1
  "elem-index"
::
++  test-elem-indices
  %^  expect-eq
  (elem-indices 2 [1 2 1 2 ~])
  [1 3 ~]
  "elem-indices"
::
++  test-find-index
  %^  expect-eq
  (find-index [1 2 3 ~] |=(a/@ =(a 2)))
  `1
  "find-index"
::
++  test-find-indices
  %^  expect-eq
  (find-indices [1 2 1 2 ~] |=(a/@ =(a 2)))
  [1 3 ~]
  "find-indices"
::
++  test-zip
  %^  expect-eq
  (zip [[1 2 3 ~] [4 5 6 ~] [7 8 9 ~] ~])
  [[1 4 7 ~] [2 5 8 ~] [3 6 9 ~] ~]
  "zip"
::
++  test-unique
  %^  expect-eq
  (unique [1 2 3 1 2 3 ~])
  [1 2 3 ~]
  "unique"
::
++  test-delete
  %^  expect-eq
  (delete 2 [1 2 3 2 ~])
  [1 3 2 ~]
  "delete"
::
++  test-delete-firsts
  %^  expect-eq
  (delete-firsts [1 2 2 2 3 4 5 ~] [2 2 5 ~])
  [1 2 3 4 ~]
  "delete-firsts"
::
++  test-union
  %^  expect-eq
  (union [1 2 3 ~] [4 2 5 ~])
  [1 2 3 4 5 ~]
  "union"
::
++  test-intersect
  %^  expect-eq
  (intersect [5 6 6 7 8 ~] [9 8 8 6 ~])
  [6 6 8 ~]
  "intersect"
--

