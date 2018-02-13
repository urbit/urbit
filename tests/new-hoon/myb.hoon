/+  new-hoon, tester
=,  myb:new-hoon
|_  tester-type:tester
++  test-from-list-null
  (expect-eq (from-list ~) ~ "from-list")
::
++  test-from-list-real
  (expect-eq (from-list [5 ~]) [~ 5] "from-list")
::
++  test-to-list-null
  (expect-eq (to-list ~) ~ "to-list")
::
++  test-to-list-real
  (expect-eq (to-list [~ 5]) [5 ~] "to-list")
::
++  test-concat-null
  (expect-eq (concat ~) ~ "concat")
::
++  test-concat-real
  ::  wait, if i pull the cast out from below, the concat implementation
  ::  doesn't compile anymore?
  (expect-eq (concat `(list (maybe @ud))`[~ [~ 1] ~ [~ 2] ~]) [1 2 ~] "concat")
::
++  test-map
  %^  expect-eq
    %+  map:myb
    [1 2 3 2 ~]
    |=(a/@u ?:(=(2 a) [~ 2] ~))
  [2 2 ~]
  "map"
--

